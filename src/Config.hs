{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    ServerConfig (..),
    LlmConfig (..),
    StatsConfig (..),
    GridConfig (..),
    loadConfig,
    defaultConfig,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import Toml qualified

data Config = Config
  { configServer :: ServerConfig,
    configLlm :: LlmConfig,
    configStats :: StatsConfig,
    configGrid :: GridConfig
  }
  deriving (Show, Eq, Generic)

data ServerConfig = ServerConfig
  { serverHost :: Text,
    serverPort :: Int
  }
  deriving (Show, Eq, Generic)

data LlmConfig = LlmConfig
  { llmOllamaHost :: Text,
    llmModel :: Text,
    llmTemperature :: Double
  }
  deriving (Show, Eq, Generic)

data StatsConfig = StatsConfig
  { statsTokensPerOp :: Int,
    statsCostPerOp :: Double
  }
  deriving (Show, Eq, Generic)

data GridConfig = GridConfig
  { gridDefaultRows :: Int,
    gridDefaultCols :: Int
  }
  deriving (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    { configServer =
        ServerConfig
          { serverHost = "localhost",
            serverPort = 8080
          },
      configLlm =
        LlmConfig
          { llmOllamaHost = "http://localhost:11434",
            llmModel = "qwen2.5:0.5b",
            llmTemperature = 0.0
          },
      configStats =
        StatsConfig
          { statsTokensPerOp = 300,
            statsCostPerOp = 0.003
          },
      configGrid =
        GridConfig
          { gridDefaultRows = 20,
            gridDefaultCols = 10
          }
    }

serverCodec :: TomlCodec ServerConfig
serverCodec =
  ServerConfig
    <$> Toml.text "host" .= serverHost
    <*> Toml.int "port" .= serverPort

llmCodec :: TomlCodec LlmConfig
llmCodec =
  LlmConfig
    <$> Toml.text "ollama_host" .= llmOllamaHost
    <*> Toml.text "model" .= llmModel
    <*> Toml.double "temperature" .= llmTemperature

statsCodec :: TomlCodec StatsConfig
statsCodec =
  StatsConfig
    <$> Toml.int "tokens_per_operation" .= statsTokensPerOp
    <*> Toml.double "cost_per_operation" .= statsCostPerOp

gridCodec :: TomlCodec GridConfig
gridCodec =
  GridConfig
    <$> Toml.int "default_rows" .= gridDefaultRows
    <*> Toml.int "default_cols" .= gridDefaultCols

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table serverCodec "server" .= configServer
    <*> Toml.table llmCodec "llm" .= configLlm
    <*> Toml.table statsCodec "stats" .= configStats
    <*> Toml.table gridCodec "grid" .= configGrid

loadConfig :: FilePath -> IO (Either Text Config)
loadConfig path = do
  content <- TIO.readFile path
  pure $ case Toml.decode configCodec content of
    Left errs -> Left $ T.pack $ show errs
    Right cfg -> Right cfg
