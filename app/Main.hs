{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Clonad (ClonadEnv, defaultEnv, runClonad, withTemperature)
import Config
import Control.Concurrent.STM
import Control.Monad (forM_)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import Spreadsheet
import System.Directory (doesFileExist)
import Web.Scotty

loadConfigWithFallback :: FilePath -> IO Config
loadConfigWithFallback path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- loadConfig path
      case result of
        Left err -> do
          putStrLn $ "Warning: Failed to parse config: " ++ T.unpack err
          putStrLn "Using default configuration."
          pure defaultConfig
        Right cfg -> do
          putStrLn $ "Loaded config from " ++ path
          pure cfg
    else do
      putStrLn $ "Config file not found at " ++ path ++ ", using defaults."
      pure defaultConfig

data AppState = AppState
  { appGrid :: TVar Grid,
    appStats :: TVar Stats,
    appEnv :: ClonadEnv,
    appConfig :: Config
  }

main :: IO ()
main = do
  putStrLn "Clonadic - Every formula evaluation is a prayer."

  cfg <- loadConfigWithFallback "config.toml"
  let port = serverPort (configServer cfg)
  putStrLn $ "Starting server on http://" ++ T.unpack (serverHost (configServer cfg)) ++ ":" ++ show port

  env <- defaultEnv
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
      grid <- liftIO $ readTVarIO (appGrid state)
      stats <- liftIO $ readTVarIO (appStats state)
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
      inputParam <- queryParamMaybe "input"
      let input = fromMaybe "" inputParam
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

updateCell :: AppState -> Coord -> Text -> IO Aeson.Value
updateCell AppState {..} coord inputValue = do
  let trimmed = T.strip inputValue

  if T.null trimmed
    then do
      -- Clear the cell
      atomically $ modifyTVar' appGrid (Map.delete coord)
      -- Recalculate dependents after clearing
      recalculateDependents appEnv appGrid appStats appConfig coord
      grid <- readTVarIO appGrid
      stats <- readTVarIO appStats
      pure $
        object
          [ "success" .= True,
            "cell" .= cellToJson (Cell CellEmpty Nothing ""),
            "cells" .= gridToJson grid,
            "stats" .= stats
          ]
    else
      if isFormula trimmed
        then do
          -- It's a formula - Claude evaluates it
          grid <- readTVarIO appGrid
          putStrLn ""
          putStrLn "========== FORMULA EVALUATION =========="
          putStrLn $ "Formula: " ++ T.unpack trimmed
          putStrLn $ "Cell refs found: " ++ show (extractCellRefs trimmed)
          let refs = extractCellRefs trimmed
          forM_ refs $ \refCoord -> do
            let refCell = getCell refCoord grid
            putStrLn $ "  " ++ show refCoord ++ " -> " ++ T.unpack (cellDisplay refCell)
          let ctx = gridContextText grid
          putStrLn ""
          putStrLn "--- PROMPT TO LLM ---"
          putStrLn "System: You are a spreadsheet formula engine. Evaluate the given formula."
          putStrLn "        Cell values are provided as context. Return ONLY the numeric result."
          putStrLn "        If the formula is invalid or references empty cells, return 'ERROR: <reason>'."
          putStrLn "        Do not explain. Just the number or error."
          putStrLn ""
          putStrLn $ "Input tuple: (\"" ++ T.unpack trimmed ++ "\", \"" ++ T.unpack ctx ++ "\")"
          putStrLn "--- END PROMPT ---"
          putStrLn ""
          result <- runClonad appEnv $ withTemperature 0.0 $ evaluateFormula trimmed grid
          putStrLn $ "--- LLM RESPONSE ---"
          putStrLn $ "Result: " ++ show result
          putStrLn "==========================================="
          putStrLn ""

          let displayText = case result of
                CellEmpty -> ""
                CellNumber n -> T.pack $ showNumber n
                CellText t -> t
                CellBoolean b -> if b then "TRUE" else "FALSE"
                CellError e -> "ERROR: " <> e

          let newCell =
                Cell
                  { cellValue = result,
                    cellFormula = Just trimmed,
                    cellDisplay = displayText
                  }

          let statsCfg = configStats appConfig
          atomically $ do
            modifyTVar' appGrid (Map.insert coord newCell)
            modifyTVar' appStats $ \s ->
              s
                { statsOperations = statsOperations s + 1,
                  statsTokensEstimate = statsTokensEstimate s + statsTokensPerOp statsCfg,
                  statsCostEstimate = statsCostEstimate s + statsCostPerOp statsCfg
                }

          -- Recalculate cells that depend on this one
          recalculateDependents appEnv appGrid appStats appConfig coord

          updatedGrid <- readTVarIO appGrid
          stats <- readTVarIO appStats

          pure $
            object
              [ "success" .= True,
                "cell" .= cellToJson newCell,
                "cells" .= gridToJson updatedGrid,
                "stats" .= stats
              ]
        else do
          -- It's a literal value
          let cellVal = case reads (T.unpack trimmed) of
                [(n, "")] -> CellNumber n
                _ -> CellText trimmed

          let displayText = case cellVal of
                CellNumber n -> T.pack $ showNumber n
                CellText t -> t
                _ -> trimmed

          let newCell =
                Cell
                  { cellValue = cellVal,
                    cellFormula = Nothing,
                    cellDisplay = displayText
                  }

          atomically $ modifyTVar' appGrid (Map.insert coord newCell)

          -- Recalculate cells that depend on this one
          recalculateDependents appEnv appGrid appStats appConfig coord

          updatedGrid <- readTVarIO appGrid
          stats <- readTVarIO appStats

          pure $
            object
              [ "success" .= True,
                "cell" .= cellToJson newCell,
                "cells" .= gridToJson updatedGrid,
                "stats" .= stats
              ]

-- Recalculate all cells that depend on the changed cell
recalculateDependents :: ClonadEnv -> TVar Grid -> TVar Stats -> Config -> Coord -> IO ()
recalculateDependents env gridVar statsVar cfg changedCoord = do
  grid <- readTVarIO gridVar
  let dependents = nub $ findDependentCells changedCoord grid
  putStrLn $ "Changed cell: " ++ show changedCoord
  putStrLn $ "Found dependents: " ++ show dependents
  -- Debug: show all formulas in grid
  let formulas = [(c, f) | (c, cell) <- Map.toList grid, Just f <- [cellFormula cell]]
  putStrLn $ "All formulas in grid: " ++ show formulas
  -- Recalculate each dependent cell
  mapM_ (recalculateCell env gridVar statsVar cfg) dependents

recalculateCell :: ClonadEnv -> TVar Grid -> TVar Stats -> Config -> Coord -> IO ()
recalculateCell env gridVar statsVar cfg coord = do
  grid <- readTVarIO gridVar
  case Map.lookup coord grid of
    Nothing -> putStrLn $ "  Cell " ++ show coord ++ " not found in grid"
    Just cell -> case cellFormula cell of
      Nothing -> putStrLn $ "  Cell " ++ show coord ++ " has no formula"
      Just formula -> do
        putStrLn ""
        putStrLn $ "========== RECALCULATING " ++ show coord ++ " =========="
        putStrLn $ "Formula: " ++ T.unpack formula
        let refs = extractCellRefs formula
        putStrLn $ "Cell refs: " ++ show refs
        forM_ refs $ \refCoord -> do
          let refCell = getCell refCoord grid
          putStrLn $ "  " ++ show refCoord ++ " -> " ++ T.unpack (cellDisplay refCell)
        let ctx = gridContextText grid
        putStrLn ""
        putStrLn "--- PROMPT TO LLM ---"
        putStrLn $ "Input: (\"" ++ T.unpack formula ++ "\", \"" ++ T.unpack ctx ++ "\")"
        putStrLn "--- END PROMPT ---"
        result <- runClonad env $ withTemperature 0.0 $ evaluateFormula formula grid
        let displayText = case result of
              CellEmpty -> ""
              CellNumber n -> T.pack $ showNumber n
              CellText t -> t
              CellBoolean b -> if b then "TRUE" else "FALSE"
              CellError e -> "ERROR: " <> e
        putStrLn $ "--- LLM RESPONSE ---"
        putStrLn $ "Result: " ++ T.unpack displayText
        putStrLn "==========================================="
        let newCell = cell {cellValue = result, cellDisplay = displayText}
            statsCfg = configStats cfg
        atomically $ do
          modifyTVar' gridVar (Map.insert coord newCell)
          modifyTVar' statsVar $ \s ->
            s
              { statsOperations = statsOperations s + 1,
                statsTokensEstimate = statsTokensEstimate s + statsTokensPerOp statsCfg,
                statsCostEstimate = statsCostEstimate s + statsCostPerOp statsCfg
              }

showNumber :: Double -> String
showNumber n
  | n == fromIntegral (round n :: Integer) = show (round n :: Integer)
  | otherwise = show n

recalculateAll :: AppState -> IO Aeson.Value
recalculateAll AppState {..} = do
  grid <- readTVarIO appGrid
  let formulaCells = [(coord, cell) | (coord, cell) <- Map.toList grid, isFormulaCell cell]

  -- Evaluate each formula cell (the API call storm)
  newGrid <- foldlM (recalcCell appEnv appStats appConfig) grid formulaCells

  atomically $ writeTVar appGrid newGrid
  stats <- readTVarIO appStats

  pure $
    object
      [ "success" .= True,
        "cells" .= gridToJson newGrid,
        "stats" .= stats,
        "recalculated" .= length formulaCells
      ]
  where
    isFormulaCell Cell {..} = case cellFormula of
      Just _ -> True
      Nothing -> False

    foldlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
    foldlM _ z [] = pure z
    foldlM f z (x : xs) = do
      z' <- f z x
      foldlM f z' xs

recalcCell :: ClonadEnv -> TVar Stats -> Config -> Grid -> (Coord, Cell) -> IO Grid
recalcCell env statsVar cfg grid (coord, cell) = do
  case cellFormula cell of
    Nothing -> pure grid
    Just formula -> do
      result <- runClonad env $ withTemperature 0.0 $ evaluateFormula formula grid

      let displayText = case result of
            CellEmpty -> ""
            CellNumber n -> T.pack $ showNumber n
            CellText t -> t
            CellBoolean b -> if b then "TRUE" else "FALSE"
            CellError e -> "ERROR: " <> e

      let newCell =
            cell
              { cellValue = result,
                cellDisplay = displayText
              }
          statsCfg = configStats cfg

      atomically $
        modifyTVar' statsVar $ \s ->
          s
            { statsOperations = statsOperations s + 1,
              statsTokensEstimate = statsTokensEstimate s + statsTokensPerOp statsCfg,
              statsCostEstimate = statsCostEstimate s + statsCostPerOp statsCfg
            }

      pure $ Map.insert coord newCell grid
