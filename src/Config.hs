{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    ServerConfig (..),
    LlmConfig (..),
    LlmProvider (..),
    StatsConfig (..),
    GridConfig (..),
    ConfigError (..),
    loadConfig,
    validateConfig,
    defaultConfig,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import Toml qualified

data ConfigError
  = ConfigParseError !Text
  | ConfigFileError !IOException
  | ConfigValidationError !Text
  deriving stock (Show, Eq)

data Config = Config
  { configServer :: ServerConfig,
    configLlm :: LlmConfig,
    configStats :: StatsConfig,
    configGrid :: GridConfig
  }
  deriving stock (Show, Eq, Generic)

data ServerConfig = ServerConfig
  { serverHost :: Text,
    serverPort :: Int,
    serverDebug :: Bool
  }
  deriving stock (Show, Eq, Generic)

data LlmProvider = Ollama | OpenAI | Anthropic
  deriving stock (Show, Eq, Generic)

data LlmConfig = LlmConfig
  { llmProvider :: LlmProvider,
    llmBaseUrl :: Maybe Text, -- Ollama host or custom OpenAI endpoint
    llmApiKey :: Maybe Text, -- API key for OpenAI/Anthropic
    llmModel :: Text,
    llmTemperature :: Double
  }
  deriving stock (Show, Eq, Generic)

data StatsConfig = StatsConfig
  { statsTokensPerOp :: Int,
    statsCostPerOp :: Double
  }
  deriving stock (Show, Eq, Generic)

data GridConfig = GridConfig
  { gridDefaultRows :: Int,
    gridDefaultCols :: Int
  }
  deriving stock (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    { configServer =
        ServerConfig
          { serverHost = "localhost",
            serverPort = 8080,
            serverDebug = False
          },
      configLlm =
        LlmConfig
          { llmProvider = Ollama,
            llmBaseUrl = Just "http://localhost:11434",
            llmApiKey = Nothing,
            llmModel = "qwen3:4b",
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
    <*> Toml.bool "debug" .= serverDebug

providerCodec :: Toml.Key -> TomlCodec LlmProvider
providerCodec = Toml.textBy fromProvider toProvider
  where
    toProvider :: Text -> Either Text LlmProvider
    toProvider "ollama" = Right Ollama
    toProvider "openai" = Right OpenAI
    toProvider "anthropic" = Right Anthropic
    toProvider other = Left $ "Unknown provider: " <> other <> ". Expected: ollama, openai, or anthropic"

    fromProvider :: LlmProvider -> Text
    fromProvider Ollama = "ollama"
    fromProvider OpenAI = "openai"
    fromProvider Anthropic = "anthropic"

llmCodec :: TomlCodec LlmConfig
llmCodec =
  LlmConfig
    <$> providerCodec "provider" .= llmProvider
    <*> Toml.dioptional (Toml.text "base_url") .= llmBaseUrl
    <*> Toml.dioptional (Toml.text "api_key") .= llmApiKey
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

-- | Validate server configuration
validateServerConfig :: ServerConfig -> Either ConfigError ()
validateServerConfig ServerConfig {..} =
  when (serverPort < 1 || serverPort > 65535) $
    Left $
      ConfigValidationError "Port must be between 1 and 65535"

-- | Validate LLM configuration
validateLlmConfig :: LlmConfig -> Either ConfigError ()
validateLlmConfig LlmConfig {..} = do
  when (llmTemperature < 0 || llmTemperature > 2) $
    Left $
      ConfigValidationError "Temperature must be between 0 and 2"
  when (llmProvider /= Ollama && isNothing llmApiKey) $
    Left $
      ConfigValidationError "API key required for OpenAI/Anthropic"

-- | Validate grid configuration
validateGridConfig :: GridConfig -> Either ConfigError ()
validateGridConfig GridConfig {..} =
  when (gridDefaultRows < 1 || gridDefaultCols < 1) $
    Left $
      ConfigValidationError "Grid dimensions must be positive"

-- | Validate config values at load time to catch errors early
validateConfig :: Config -> Either ConfigError Config
validateConfig cfg@Config {..} = do
  validateServerConfig configServer
  validateLlmConfig configLlm
  validateGridConfig configGrid
  pure cfg

loadConfig :: FilePath -> IO (Either ConfigError Config)
loadConfig path = do
  result <- try $ TIO.readFile path
  pure $ case result of
    Left err -> Left $ ConfigFileError err
    Right content -> case Toml.decode configCodec content of
      Left errs -> Left $ ConfigParseError $ T.pack $ show errs
      Right cfg -> validateConfig cfg
