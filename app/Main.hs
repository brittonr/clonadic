module Main where

import Clonad (ApiKey, ClonadEnv, ModelId, mkEnv, mkOllamaEnv, mkOpenAIEnv, runClonad, withTemperature)
import Config (Config (..), ConfigError (..), GridConfig (..), LlmConfig (..), LlmProvider (..), ServerConfig (..), StatsConfig (..), defaultConfig, loadConfig)
import Control.Concurrent.STM
import Control.Monad (foldM, forM_, when)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (traverse_)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Spreadsheet
import Web.Scotty

-- | Structured API error type for consistent error responses
data ApiError
  = InvalidJsonBody
  | MissingParam !Text
  | InvalidParam !Text !String
  | InvalidCoordinate !Int !Int
  deriving stock (Show, Generic)

-- | Render API error as user-facing message
apiErrorMessage :: ApiError -> Text
apiErrorMessage = \case
  InvalidJsonBody -> "Invalid JSON body"
  MissingParam name -> "Missing parameter: " <> name
  InvalidParam name msg -> "Invalid parameter '" <> name <> "': " <> T.pack msg
  InvalidCoordinate r c -> "Invalid coordinate: row=" <> T.pack (show r) <> ", col=" <> T.pack (show c)

-- | Raise an API error with consistent JSON response format
raiseApiError :: ApiError -> ActionM a
raiseApiError err = do
  status status400
  json $ object ["success" .= False, "error" .= apiErrorMessage err]
  finish

loadConfigWithFallback :: FilePath -> IO Config
loadConfigWithFallback path = do
  result <- loadConfig path
  case result of
    Left (ConfigFileError _) -> do
      TIO.putStrLn $ "Config file not found at " <> T.pack path <> ", using defaults."
      pure defaultConfig
    Left (ConfigParseError err) -> do
      TIO.putStrLn $ "Warning: Failed to parse config: " <> err
      TIO.putStrLn "Using default configuration."
      pure defaultConfig
    Left (ConfigValidationError err) -> do
      TIO.putStrLn $ "Warning: Config validation failed: " <> err
      TIO.putStrLn "Using default configuration."
      pure defaultConfig
    Right cfg -> do
      TIO.putStrLn $ "Loaded config from " <> T.pack path
      pure cfg

data AppState = AppState
  { appGrid :: TVar Grid,
    appStats :: TVar Stats,
    appEnv :: ClonadEnv,
    appConfig :: Config
  }

readAppState :: AppState -> IO (Grid, Stats)
readAppState AppState {..} = atomically $ (,) <$> readTVar appGrid <*> readTVar appStats

-- | Create ClonadEnv from config with validation
mkClonadEnvFromConfig :: LlmConfig -> ClonadEnv
mkClonadEnvFromConfig LlmConfig {..} = case llmProvider of
  Ollama -> mkOllamaEnv baseUrl modelId
  OpenAI -> mkOpenAIEnv apiKey modelId llmBaseUrl
  Anthropic -> mkEnv apiKey
  where
    baseUrl = fromMaybe "http://localhost:11434" llmBaseUrl
    modelId = fromString (T.unpack llmModel) :: ModelId
    apiKey = fromString (T.unpack (fromMaybe "" llmApiKey)) :: ApiKey

main :: IO ()
main = do
  TIO.putStrLn "Clonadic - Every formula evaluation is a prayer."

  cfg <- loadConfigWithFallback "config.toml"
  let port = serverPort (configServer cfg)
      llmCfg = configLlm cfg
  TIO.putStrLn $ "Starting server on http://" <> serverHost (configServer cfg) <> ":" <> T.pack (show port)
  TIO.putStrLn $ "LLM Provider: " <> T.pack (show (llmProvider llmCfg))
  TIO.putStrLn $ "LLM Model: " <> llmModel llmCfg

  let env = mkClonadEnvFromConfig llmCfg
  gridVar <- newTVarIO emptyGrid
  statsVar <- newTVarIO emptyStats

  let state = AppState gridVar statsVar env cfg

  scotty port $ do
    middleware logStdoutDev
    middleware $ staticPolicy (addBase "static")

    get "/" $ file "static/index.html"

    get "/api/grid" $ do
      (grid, stats) <- liftIO $ readAppState state
      json $ object ["cells" .= gridToJson grid, "stats" .= stats]

    post "/api/cell" $ do
      row <- jsonParam "row"
      col <- jsonParam "col"
      value <- jsonParam "value"
      case mkCoord row col of
        Nothing -> raiseApiError (InvalidCoordinate row col)
        Just coord -> do
          result <- liftIO $ updateCell state coord value
          json result

    post "/api/recalculate" $ do
      result <- liftIO $ recalculateAll state
      json result

    get "/api/stats" $ do
      stats <- liftIO $ readTVarIO (appStats state)
      json stats

    get "/api/autocomplete" $ do
      input <- fromMaybe "" <$> queryParamMaybe "input"
      grid <- liftIO $ readTVarIO (appGrid state)
      let gridCfg = configGrid (appConfig state)
          suggestions = getAutocompleteSuggestions input grid (gridDefaultRows gridCfg) (gridDefaultCols gridCfg)
      json $ object ["suggestions" .= suggestions]

jsonParam :: (Aeson.FromJSON a) => Text -> ActionM a
jsonParam name = do
  b <- body
  case Aeson.decode b of
    Nothing -> raiseApiError InvalidJsonBody
    Just obj -> case lookupAndParse name obj of
      Nothing -> raiseApiError (MissingParam name)
      Just (Aeson.Error msg) -> raiseApiError (InvalidParam name msg)
      Just (Aeson.Success val) -> pure val
  where
    lookupAndParse :: (Aeson.FromJSON a) => Text -> Aeson.Value -> Maybe (Aeson.Result a)
    lookupAndParse k (Aeson.Object o) =
      Aeson.fromJSON <$> KM.lookup (Key.fromText k) o
    lookupAndParse _ _ = Nothing

gridToJson :: Grid -> Aeson.Value
gridToJson grid =
  Aeson.Object $
    KM.fromList
      [ (Key.fromString $ show (unRow row) <> "," <> show (unCol col), cellToJson cell)
      | (Coord row col, cell) <- Map.toList grid
      ]

cellToJson :: Cell -> Aeson.Value
cellToJson Cell {..} =
  object
    [ "value" .= cellValue,
      "formula" .= cellFormula,
      "display" .= cellDisplay
    ]

data CellInput
  = ClearCell
  | FormulaInput !Text
  | LiteralInput !Text

classifyCellInput :: Text -> CellInput
classifyCellInput t
  | T.null t = ClearCell
  | isFormula t = FormulaInput t
  | otherwise = LiteralInput t

updateCell :: AppState -> Coord -> Text -> IO Aeson.Value
updateCell state coord inputValue =
  case classifyCellInput (T.strip inputValue) of
    ClearCell -> handleClearCell state coord
    FormulaInput formula -> handleFormulaCell state coord formula
    LiteralInput value -> handleLiteralCell state coord value

handleClearCell :: AppState -> Coord -> IO Aeson.Value
handleClearCell AppState {..} coord = do
  atomically $ modifyTVar' appGrid (Map.delete coord)
  recalculateDependents appEnv appGrid appStats appConfig coord
  (grid, stats) <- readAppState AppState {..}
  pure $ mkCellResponse emptyCell grid stats

handleFormulaCell :: AppState -> Coord -> Text -> IO Aeson.Value
handleFormulaCell state@AppState {..} coord formula = do
  grid <- readTVarIO appGrid
  logFormulaEvaluation formula grid
  newCell <- evaluateAndBuildCell appEnv formula grid
  debugLogT "--- LLM RESPONSE ---"
  debugLogT $ "Result: " <> T.pack (show (cellValue newCell))
  debugLogT "==========================================="
  updateGridAndStats appGrid appStats (configStats appConfig) coord newCell
  recalculateDependents appEnv appGrid appStats appConfig coord
  (updatedGrid, stats) <- readAppState state
  pure $ mkCellResponse newCell updatedGrid stats

handleLiteralCell :: AppState -> Coord -> Text -> IO Aeson.Value
handleLiteralCell state@AppState {..} coord value = do
  let cellVal = case TR.double value of
        Right (n, rest) | T.null rest -> CellNumber n
        _ -> CellText value
      newCell = mkLiteralCell cellVal (cellValueToDisplay cellVal)
  atomically $ modifyTVar' appGrid (Map.insert coord newCell)
  recalculateDependents appEnv appGrid appStats appConfig coord
  (updatedGrid, stats) <- readAppState state
  pure $ mkCellResponse newCell updatedGrid stats

-- | Unified formula evaluation and cell construction
-- Consolidates the repeated pattern across handlers
evaluateAndBuildCell :: ClonadEnv -> Text -> Grid -> IO Cell
evaluateAndBuildCell env formula grid = do
  result <- runClonad env $ withTemperature 0.0 $ evaluateFormula formula grid
  let displayText = cellValueToDisplay result
  pure $ mkFormulaCell formula result displayText

logFormulaEvaluation :: Text -> Grid -> IO ()
logFormulaEvaluation formula grid = do
  debugLogT ""
  debugLogT "========== FORMULA EVALUATION =========="
  debugLogT $ "Formula: " <> formula
  let refs = extractCellRefs formula
  debugLogT $ "Cell refs found: " <> T.pack (show refs)
  when debugEnabled $
    forM_ refs $ \refCoord -> do
      let refCell = getCell refCoord grid
      debugLogT $ "  " <> T.pack (show refCoord) <> " -> " <> cellDisplay refCell
  let ctx = gridContextText grid
  debugLogT ""
  debugLogT "--- PROMPT TO LLM ---"
  debugLogT "System: You are a spreadsheet formula engine. Evaluate the given formula."
  debugLogT "        Cell values are provided as context. Return ONLY the numeric result."
  debugLogT "        If the formula is invalid or references empty cells, return 'ERROR: <reason>'."
  debugLogT ""
  debugLogT $ "Input tuple: (\"" <> formula <> "\", \"" <> ctx <> "\")"
  debugLogT "--- END PROMPT ---"

-- | Recalculate all cells that depend on the changed cell
recalculateDependents :: ClonadEnv -> TVar Grid -> TVar Stats -> Config -> Coord -> IO ()
recalculateDependents env gridVar statsVar cfg changedCoord = do
  grid <- readTVarIO gridVar
  let dependents = findDependentCells changedCoord grid
  debugLogT $ "Changed cell: " <> T.pack (show changedCoord)
  debugLogT $ "Found dependents: " <> T.pack (show dependents)
  when debugEnabled $ do
    let formulas = [(c, f) | (c, cell) <- Map.toList grid, Just f <- [cellFormula cell]]
    debugLogT $ "All formulas in grid: " <> T.pack (show formulas)
  traverse_ (recalculateCell env gridVar statsVar cfg) dependents

recalculateCell :: ClonadEnv -> TVar Grid -> TVar Stats -> Config -> Coord -> IO ()
recalculateCell env gridVar statsVar cfg coord = do
  grid <- readTVarIO gridVar
  case Map.lookup coord grid of
    Nothing ->
      debugLogT $ "  Cell " <> T.pack (show coord) <> " not found in grid"
    Just cell -> case cellFormula cell of
      Nothing ->
        debugLogT $ "  Cell " <> T.pack (show coord) <> " has no formula"
      Just formula -> do
        debugLogT ""
        debugLogT $ "========== RECALCULATING " <> T.pack (show coord) <> " =========="
        debugLogT $ "Formula: " <> formula
        let refs = extractCellRefs formula
        debugLogT $ "Cell refs: " <> T.pack (show refs)
        when debugEnabled $
          forM_ refs $ \refCoord -> do
            let refCell = getCell refCoord grid
            debugLogT $ "  " <> T.pack (show refCoord) <> " -> " <> cellDisplay refCell
        debugLogT ""
        debugLogT "--- PROMPT TO LLM ---"
        debugLogT $ "Input: (\"" <> formula <> "\", \"" <> gridContextText grid <> "\")"
        debugLogT "--- END PROMPT ---"
        newCell <- evaluateAndBuildCell env formula grid
        -- Preserve original formula when updating
        let updatedCell = newCell {cellFormula = Just formula}
        debugLogT "--- LLM RESPONSE ---"
        debugLogT $ "Result: " <> cellDisplay updatedCell
        debugLogT "==========================================="
        updateGridAndStats gridVar statsVar (configStats cfg) coord updatedCell

incrementStats :: StatsConfig -> Stats -> Stats
incrementStats StatsConfig {..} Stats {..} =
  Stats
    { statsOperations = statsOperations + 1,
      statsTokensEstimate = statsTokensEstimate + statsTokensPerOp,
      statsCostEstimate = statsCostEstimate + statsCostPerOp
    }

mkCellResponse :: Cell -> Grid -> Stats -> Aeson.Value
mkCellResponse cell grid stats =
  object
    [ "success" .= True,
      "cell" .= cellToJson cell,
      "cells" .= gridToJson grid,
      "stats" .= stats
    ]

-- TODO: Make this configurable via Config or environment variable
debugEnabled :: Bool
debugEnabled = False

debugLogT :: Text -> IO ()
debugLogT msg = when debugEnabled $ TIO.putStrLn msg

-- | STM helper for atomic grid+stats update
updateGridAndStats :: TVar Grid -> TVar Stats -> StatsConfig -> Coord -> Cell -> IO ()
updateGridAndStats gridVar statsVar statsCfg coord cell =
  atomically $ do
    modifyTVar' gridVar (Map.insert coord cell)
    modifyTVar' statsVar (incrementStats statsCfg)

recalculateAll :: AppState -> IO Aeson.Value
recalculateAll AppState {..} = do
  grid <- readTVarIO appGrid
  let formulaCells = Map.toList $ Map.filter (isJust . cellFormula) grid

  newGrid <- foldM (recalcCell appEnv appStats appConfig) grid formulaCells

  atomically $ writeTVar appGrid newGrid
  stats <- readTVarIO appStats

  pure $
    object
      [ "success" .= True,
        "cells" .= gridToJson newGrid,
        "stats" .= stats,
        "recalculated" .= length formulaCells
      ]

recalcCell :: ClonadEnv -> TVar Stats -> Config -> Grid -> (Coord, Cell) -> IO Grid
recalcCell env statsVar cfg grid (coord, cell) = case cellFormula cell of
  Nothing -> pure grid
  Just formula -> do
    newCell <- evaluateAndBuildCell env formula grid
    atomically $ modifyTVar' statsVar (incrementStats (configStats cfg))
    pure $ Map.insert coord newCell grid
