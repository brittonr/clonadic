module Main where

import Clonad (ApiKey, ClonadEnv, ModelId, mkEnv, mkOllamaEnv, mkOpenAIEnv, runClonad, withTemperature)
import Config (Config (..), ConfigError (..), GridConfig (..), LlmConfig (..), LlmProvider (..), ServerConfig (..), StatsConfig (..), defaultConfig, formatValidationReason, loadConfig, serverDebug)
import Control.Concurrent.STM
import Control.Monad (foldM, forM_, when)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (traverse_)
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
    Left (ConfigValidationError reason) -> do
      TIO.putStrLn $ "Warning: Config validation failed: " <> formatValidationReason reason
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

-- | Application monad with access to AppState via ReaderT
newtype App a = App {unApp :: ReaderT AppState IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

-- | Run an App action with the given state
runApp :: AppState -> App a -> IO a
runApp state (App m) = runReaderT m state

-- | Read grid and stats atomically in App monad
readAppState :: App (Grid, Stats)
readAppState = do
  AppState {..} <- ask
  liftIO $ atomically $ (,) <$> readTVar appGrid <*> readTVar appStats

-- | Read grid in App monad
readGridM :: App Grid
readGridM = asks appGrid >>= liftIO . readTVarIO

-- | Read stats in App monad
readStatsM :: App Stats
readStatsM = asks appStats >>= liftIO . readTVarIO

-- | Modify grid atomically in App monad
modifyGridM :: (Grid -> Grid) -> App ()
modifyGridM f = asks appGrid >>= \v -> liftIO $ atomically $ modifyTVar' v f

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
      (grid, stats) <- liftIO $ runApp state readAppState
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
      | (Coord row col, cell) <- gridToList grid
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
  runApp state $ case classifyCellInput (T.strip inputValue) of
    ClearCell -> handleClearCell coord
    FormulaInput formula -> handleFormulaCell coord formula
    LiteralInput value -> handleLiteralCell coord value

-- | Handle cell clearing in App monad
handleClearCell :: Coord -> App Aeson.Value
handleClearCell coord = do
  AppState {..} <- ask
  liftIO $ deleteCellAtomic appGrid coord
  recalculateDependents coord
  (grid, stats) <- readAppState
  pure $ mkCellResponse emptyCell grid stats

-- | Handle formula cell update in App monad
handleFormulaCell :: Coord -> Text -> App Aeson.Value
handleFormulaCell coord formula = do
  AppState {..} <- ask
  grid <- readGridM
  logFormulaEvaluation formula grid
  newCell <- liftIO $ evaluateAndBuildCell appEnv formula grid
  debugLog "--- LLM RESPONSE ---"
  debugLog $ "Result: " <> T.pack (show (cellValue newCell))
  debugLog "==========================================="
  liftIO $ updateGridAndStats appGrid appStats (configStats appConfig) coord newCell
  recalculateDependents coord
  (updatedGrid, stats) <- readAppState
  pure $ mkCellResponse newCell updatedGrid stats

-- | Handle literal cell update in App monad
handleLiteralCell :: Coord -> Text -> App Aeson.Value
handleLiteralCell coord value = do
  let cellVal = case TR.double value of
        Right (n, rest) | T.null rest -> CellNumber n
        _ -> CellText value
      newCell = mkLiteralCell cellVal (cellValueToDisplay cellVal)
  modifyGridM (setCell coord newCell)
  recalculateDependents coord
  (updatedGrid, stats) <- readAppState
  pure $ mkCellResponse newCell updatedGrid stats

-- | Unified formula evaluation and cell construction
-- Consolidates the repeated pattern across handlers
evaluateAndBuildCell :: ClonadEnv -> Text -> Grid -> IO Cell
evaluateAndBuildCell env formula grid = do
  result <- runClonad env $ withTemperature 0.0 $ evaluateFormula formula grid
  let displayText = cellValueToDisplay result
  pure $ mkFormulaCell formula result displayText

-- | Log formula evaluation details
logFormulaEvaluation :: Text -> Grid -> App ()
logFormulaEvaluation formula grid = do
  debugLog ""
  debugLog "========== FORMULA EVALUATION =========="
  debugLog $ "Formula: " <> formula
  let refs = extractCellRefs formula
  debugLog $ "Cell refs found: " <> T.pack (show refs)
  whenDebug $
    forM_ refs $ \refCoord -> do
      let refCell = getCell refCoord grid
      debugLog $ "  " <> T.pack (show refCoord) <> " -> " <> cellDisplay refCell
  let ctx = gridContextText grid
  debugLog ""
  debugLog "--- PROMPT TO LLM ---"
  debugLog "System: You are a spreadsheet formula engine. Evaluate the given formula."
  debugLog "        Cell values are provided as context. Return ONLY the numeric result."
  debugLog "        If the formula is invalid or references empty cells, return 'ERROR: <reason>'."
  debugLog ""
  debugLog $ "Input tuple: (\"" <> formula <> "\", \"" <> ctx <> "\")"
  debugLog "--- END PROMPT ---"

-- | Recalculate all cells that depend on the changed cell
recalculateDependents :: Coord -> App ()
recalculateDependents changedCoord = do
  grid <- readGridM
  let dependents = findDependentCells changedCoord grid
  debugLog $ "Changed cell: " <> T.pack (show changedCoord)
  debugLog $ "Found dependents: " <> T.pack (show dependents)
  whenDebug $ do
    let formulas = [(c, f) | (c, cell) <- gridToList grid, Just f <- [cellFormula cell]]
    debugLog $ "All formulas in grid: " <> T.pack (show formulas)
  traverse_ recalculateCell dependents

-- | Recalculate a single cell
recalculateCell :: Coord -> App ()
recalculateCell coord = do
  AppState {..} <- ask
  grid <- readGridM
  case gridLookup coord grid of
    Nothing ->
      debugLog $ "  Cell " <> T.pack (show coord) <> " not found in grid"
    Just cell -> case cellFormula cell of
      Nothing ->
        debugLog $ "  Cell " <> T.pack (show coord) <> " has no formula"
      Just formula -> do
        debugLog ""
        debugLog $ "========== RECALCULATING " <> T.pack (show coord) <> " =========="
        debugLog $ "Formula: " <> formula
        let refs = extractCellRefs formula
        debugLog $ "Cell refs: " <> T.pack (show refs)
        whenDebug $
          forM_ refs $ \refCoord -> do
            let refCell = getCell refCoord grid
            debugLog $ "  " <> T.pack (show refCoord) <> " -> " <> cellDisplay refCell
        debugLog ""
        debugLog "--- PROMPT TO LLM ---"
        debugLog $ "Input: (\"" <> formula <> "\", \"" <> gridContextText grid <> "\")"
        debugLog "--- END PROMPT ---"
        newCell <- liftIO $ evaluateAndBuildCell appEnv formula grid
        let updatedCell = newCell {cellFormula = Just formula}
        debugLog "--- LLM RESPONSE ---"
        debugLog $ "Result: " <> cellDisplay updatedCell
        debugLog "==========================================="
        liftIO $ updateGridAndStats appGrid appStats (configStats appConfig) coord updatedCell

-- | Create stats for a single operation from config
statsFromConfig :: StatsConfig -> Stats
statsFromConfig StatsConfig {..} = singleOpStats statsTokensPerOp statsCostPerOp

mkCellResponse :: Cell -> Grid -> Stats -> Aeson.Value
mkCellResponse cell grid stats =
  object
    [ "success" .= True,
      "cell" .= cellToJson cell,
      "cells" .= gridToJson grid,
      "stats" .= stats
    ]

-- | Check if debug logging is enabled from config
debugEnabled :: Config -> Bool
debugEnabled = serverDebug . configServer

-- | Check if debug is enabled in App monad
isDebugEnabled :: App Bool
isDebugEnabled = asks (debugEnabled . appConfig)

-- | Log a debug message if debug mode is enabled
debugLog :: Text -> App ()
debugLog msg = whenDebug $ liftIO $ TIO.putStrLn msg

-- | Execute an action only if debug mode is enabled
whenDebug :: App () -> App ()
whenDebug action = isDebugEnabled >>= \d -> when d action

-- | STM helper for atomic grid+stats update
updateGridAndStats :: TVar Grid -> TVar Stats -> StatsConfig -> Coord -> Cell -> IO ()
updateGridAndStats gridVar statsVar statsCfg coord cell =
  atomically $ do
    modifyTVar' gridVar (setCell coord cell)
    modifyTVar' statsVar (<> statsFromConfig statsCfg)

-- | STM helper for atomic cell deletion
deleteCellAtomic :: TVar Grid -> Coord -> IO ()
deleteCellAtomic gridVar coord = atomically $ modifyTVar' gridVar (deleteCell coord)

recalculateAll :: AppState -> IO Aeson.Value
recalculateAll AppState {..} = do
  grid <- readTVarIO appGrid
  let formulaCells = gridToList $ gridFilter (isJust . cellFormula) grid

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
    atomically $ modifyTVar' statsVar (<> statsFromConfig (configStats cfg))
    pure $ setCell coord newCell grid
