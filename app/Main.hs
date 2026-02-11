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
import Data.Text.Lazy qualified as TL
import Data.Text.Read qualified as TR
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Spreadsheet
import Web.Scotty

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
    Right cfg -> do
      TIO.putStrLn $ "Loaded config from " <> T.pack path
      pure cfg

data AppState = AppState
  { appGrid :: TVar Grid,
    appStats :: TVar Stats,
    appEnv :: ClonadEnv,
    appConfig :: Config
  }

-- Create ClonadEnv from config
mkClonadEnvFromConfig :: LlmConfig -> ClonadEnv
mkClonadEnvFromConfig llmCfg = case llmProvider llmCfg of
  Ollama ->
    let baseUrl = fromMaybe "http://localhost:11434" (llmBaseUrl llmCfg)
        modelId = fromString (T.unpack (llmModel llmCfg)) :: ModelId
     in mkOllamaEnv baseUrl modelId
  OpenAI ->
    let apiKey = fromString (T.unpack (fromMaybe "" (llmApiKey llmCfg))) :: ApiKey
        modelId = fromString (T.unpack (llmModel llmCfg)) :: ModelId
     in mkOpenAIEnv apiKey modelId (llmBaseUrl llmCfg)
  Anthropic ->
    let apiKey = fromString (T.unpack (fromMaybe "" (llmApiKey llmCfg))) :: ApiKey
     in mkEnv apiKey

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

    -- Serve the main page
    get "/" $ do
      file "static/index.html"

    -- Get the current grid state
    get "/api/grid" $ do
      (grid, stats) <- liftIO $ (,) <$> readTVarIO (appGrid state) <*> readTVarIO (appStats state)
      json $
        object
          [ "cells" .= gridToJson grid,
            "stats" .= stats
          ]

    -- Update a cell
    post "/api/cell" $ do
      row <- jsonParam "row"
      col <- jsonParam "col"
      value <- jsonParam "value"
      result <- liftIO $ updateCell state (row, col) value
      json result

    -- Recalculate all formulas (the API call storm)
    post "/api/recalculate" $ do
      result <- liftIO $ recalculateAll state
      json result

    -- Get stats
    get "/api/stats" $ do
      stats <- liftIO $ readTVarIO (appStats state)
      json stats

    -- Autocomplete endpoint
    get "/api/autocomplete" $ do
      input <- fromMaybe "" <$> queryParamMaybe "input"
      grid <- liftIO $ readTVarIO (appGrid state)
      let gridCfg = configGrid (appConfig state)
          suggestions = getAutocompleteSuggestions input grid (gridDefaultRows gridCfg) (gridDefaultCols gridCfg)
      json $ object ["suggestions" .= suggestions]

jsonParam :: (Aeson.FromJSON a) => TL.Text -> ActionM a
jsonParam name = do
  b <- body
  case Aeson.decode b of
    Nothing -> do
      status status400
      text "Invalid JSON body"
      finish
    Just obj -> case lookupAndParse name obj of
      Nothing -> do
        status status400
        text $ TL.pack $ "Missing parameter: " ++ TL.unpack name
        finish
      Just (Aeson.Error msg) -> do
        status status400
        text $ TL.pack $ "Invalid parameter '" ++ TL.unpack name ++ "': " ++ msg
        finish
      Just (Aeson.Success val) -> pure val
  where
    lookupAndParse :: (Aeson.FromJSON a) => TL.Text -> Aeson.Value -> Maybe (Aeson.Result a)
    lookupAndParse k (Aeson.Object o) =
      Aeson.fromJSON <$> KM.lookup (Key.fromString $ TL.unpack k) o
    lookupAndParse _ _ = Nothing

gridToJson :: Grid -> Aeson.Value
gridToJson grid =
  Aeson.Object $
    KM.fromList
      [ (Key.fromString (show row ++ "," ++ show col), cellToJson cell)
      | ((row, col), cell) <- Map.toList grid
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
  | FormulaInput Text
  | LiteralInput Text

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
  (grid, stats) <- (,) <$> readTVarIO appGrid <*> readTVarIO appStats
  pure $ mkCellResponse (Cell CellEmpty Nothing "") grid stats

handleFormulaCell :: AppState -> Coord -> Text -> IO Aeson.Value
handleFormulaCell AppState {..} coord formula = do
  grid <- readTVarIO appGrid
  logFormulaEvaluation formula grid
  result <- runClonad appEnv $ withTemperature 0.0 $ evaluateFormula formula grid
  debugLogT "--- LLM RESPONSE ---"
  debugLogT $ "Result: " <> T.pack (show result)
  debugLogT "==========================================="
  debugLogT ""
  let displayText = cellValueToDisplay result
      newCell =
        Cell
          { cellValue = result,
            cellFormula = Just formula,
            cellDisplay = displayText
          }
      statsCfg = configStats appConfig
  updateGridAndStats appGrid appStats statsCfg coord newCell
  recalculateDependents appEnv appGrid appStats appConfig coord
  (updatedGrid, stats) <- (,) <$> readTVarIO appGrid <*> readTVarIO appStats
  pure $ mkCellResponse newCell updatedGrid stats

handleLiteralCell :: AppState -> Coord -> Text -> IO Aeson.Value
handleLiteralCell AppState {..} coord value = do
  let cellVal = case TR.double value of
        Right (n, rest) | T.null rest -> CellNumber n
        _ -> CellText value
      displayText = cellValueToDisplay cellVal
      newCell =
        Cell
          { cellValue = cellVal,
            cellFormula = Nothing,
            cellDisplay = displayText
          }
  atomically $ modifyTVar' appGrid (Map.insert coord newCell)
  recalculateDependents appEnv appGrid appStats appConfig coord
  (updatedGrid, stats) <- (,) <$> readTVarIO appGrid <*> readTVarIO appStats
  pure $ mkCellResponse newCell updatedGrid stats

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
  debugLogT "        Do not explain. Just the number or error."
  debugLogT ""
  debugLogT $ "Input tuple: (\"" <> formula <> "\", \"" <> ctx <> "\")"
  debugLogT "--- END PROMPT ---"
  debugLogT ""

-- Recalculate all cells that depend on the changed cell
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
    Nothing -> debugLogT $ "  Cell " <> T.pack (show coord) <> " not found in grid"
    Just cell -> case cellFormula cell of
      Nothing -> debugLogT $ "  Cell " <> T.pack (show coord) <> " has no formula"
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
        let ctx = gridContextText grid
        debugLogT ""
        debugLogT "--- PROMPT TO LLM ---"
        debugLogT $ "Input: (\"" <> formula <> "\", \"" <> ctx <> "\")"
        debugLogT "--- END PROMPT ---"
        result <- runClonad env $ withTemperature 0.0 $ evaluateFormula formula grid
        let displayText = cellValueToDisplay result
            newCell = cell {cellValue = result, cellDisplay = displayText}
            statsCfg = configStats cfg
        debugLogT "--- LLM RESPONSE ---"
        debugLogT $ "Result: " <> displayText
        debugLogT "==========================================="
        updateGridAndStats gridVar statsVar statsCfg coord newCell

showNumber :: Double -> String
showNumber n
  | n == fromIntegral (round n :: Integer) = show (round n :: Integer)
  | otherwise = show n

cellValueToDisplay :: CellValue -> Text
cellValueToDisplay = \case
  CellEmpty -> ""
  CellNumber n -> T.pack $ showNumber n
  CellText t -> t
  CellBoolean b -> if b then "TRUE" else "FALSE"
  CellError e -> "ERROR: " <> e

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

-- STM helper for common grid+stats update pattern
updateGridAndStats :: TVar Grid -> TVar Stats -> StatsConfig -> Coord -> Cell -> IO ()
updateGridAndStats gridVar statsVar statsCfg coord cell =
  atomically $ do
    modifyTVar' gridVar (Map.insert coord cell)
    modifyTVar' statsVar (incrementStats statsCfg)

recalculateAll :: AppState -> IO Aeson.Value
recalculateAll AppState {..} = do
  grid <- readTVarIO appGrid
  let formulaCells = Map.toList $ Map.filter (isJust . cellFormula) grid

  -- Evaluate each formula cell (the API call storm)
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
recalcCell env statsVar cfg grid (coord, cell) = do
  case cellFormula cell of
    Nothing -> pure grid
    Just formula -> do
      result <- runClonad env $ withTemperature 0.0 $ evaluateFormula formula grid
      let displayText = cellValueToDisplay result
          newCell = cell {cellValue = result, cellDisplay = displayText}
          statsCfg = configStats cfg
      atomically $ modifyTVar' statsVar (incrementStats statsCfg)
      pure $ Map.insert coord newCell grid
