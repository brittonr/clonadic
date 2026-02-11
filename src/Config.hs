{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( -- * Configuration Types
    Config (..),
    ServerConfig (..),
    LlmConfig (..),
    LlmProvider (..),
    StatsConfig (..),
    GridConfig (..),

    -- * Validated Newtypes
    Port (..),
    mkPort,
    Temperature (..),
    mkTemperature,
    PositiveInt (..),
    mkPositiveInt,

    -- * Errors
    ConfigError (..),
    ValidationReason (..),
    formatValidationReason,

    -- * Loading
    loadConfig,
    validateConfig,
    defaultConfig,
  )
where

import Control.Category ((>>>))
import Control.Exception (IOException, try)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import Toml (TomlCodec, (.=))
import Toml qualified

-- | Structured validation failure reasons
data ValidationReason
  = InvalidPort !Int
  | InvalidTemperature !Double
  | MissingApiKey !LlmProvider
  | InvalidGridDimension !Int !Int -- rows, cols
  deriving stock (Show, Eq, Generic)

-- | Format a ValidationReason for display
formatValidationReason :: ValidationReason -> Text
formatValidationReason = \case
  InvalidPort p -> "Port must be between 1 and 65535, got: " <> T.pack (show p)
  InvalidTemperature t -> "Temperature must be between 0 and 2, got: " <> T.pack (show t)
  MissingApiKey provider -> "API key required for " <> T.pack (show provider)
  InvalidGridDimension r c -> "Grid dimensions must be positive, got: rows=" <> T.pack (show r) <> ", cols=" <> T.pack (show c)

data ConfigError
  = ConfigParseError !Text
  | ConfigFileError !IOException
  | ConfigValidationError !ValidationReason
  deriving stock (Show, Eq)

-- | Valid TCP port (1-65535)
newtype Port = Port {unPort :: Int}
  deriving stock (Show, Eq, Generic)

-- | Smart constructor for Port
mkPort :: Int -> Maybe Port
mkPort n = guard (n >= 1 && n <= 65535) $> Port n

-- | LLM temperature (0.0-2.0)
newtype Temperature = Temperature {unTemperature :: Double}
  deriving stock (Show, Eq, Generic)

-- | Smart constructor for Temperature
mkTemperature :: Double -> Maybe Temperature
mkTemperature t = guard (t >= 0 && t <= 2) $> Temperature t

-- | Positive dimension for grid (>= 1)
newtype PositiveInt = PositiveInt {unPositiveInt :: Int}
  deriving stock (Show, Eq, Generic)

-- | Smart constructor for PositiveInt
mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt n = guard (n >= 1) $> PositiveInt n

data Config = Config
  { configServer :: ServerConfig,
    configLlm :: LlmConfig,
    configStats :: StatsConfig,
    configGrid :: GridConfig
  }
  deriving stock (Show, Eq, Generic)

data ServerConfig = ServerConfig
  { serverHost :: Text,
    serverPort :: !Port,
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
    llmTemperature :: !Temperature
  }
  deriving stock (Show, Eq, Generic)

data StatsConfig = StatsConfig
  { statsTokensPerOp :: Int,
    statsCostPerOp :: Double
  }
  deriving stock (Show, Eq, Generic)

data GridConfig = GridConfig
  { gridDefaultRows :: !PositiveInt,
    gridDefaultCols :: !PositiveInt
  }
  deriving stock (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    { configServer =
        ServerConfig
          { serverHost = "localhost",
            serverPort = Port 8080,
            serverDebug = False
          },
      configLlm =
        LlmConfig
          { llmProvider = Ollama,
            llmBaseUrl = Just "http://localhost:11434",
            llmApiKey = Nothing,
            llmModel = "qwen3:4b",
            llmTemperature = Temperature 0.0
          },
      configStats =
        StatsConfig
          { statsTokensPerOp = 300,
            statsCostPerOp = 0.003
          },
      configGrid =
        GridConfig
          { gridDefaultRows = PositiveInt 20,
            gridDefaultCols = PositiveInt 10
          }
    }

-- | BiMap from Port to Int (the second type is what TOML stores)
-- prism :: (b -> a) -> (a -> Either e b) -> BiMap e a b
-- For BiMap Port Int: we need (Int -> Port) and (Port -> Either e Int)
_Port :: Toml.TomlBiMap Port Int
_Port =
  Toml.BiMap
    { Toml.forward = Right . unPort,
      Toml.backward = \n ->
        maybe (Left $ Toml.ArbitraryError $ "Port must be 1-65535, got: " <> T.pack (show n)) Right (mkPort n)
    }

-- | Codec for Port with validation using BiMap composition
portCodec :: Toml.Key -> TomlCodec Port
portCodec = Toml.match (_Port >>> Toml._Int)

-- | BiMap from Temperature to Double
_Temperature :: Toml.TomlBiMap Temperature Double
_Temperature =
  Toml.BiMap
    { Toml.forward = Right . unTemperature,
      Toml.backward = \t ->
        maybe (Left $ Toml.ArbitraryError $ "Temperature must be 0-2, got: " <> T.pack (show t)) Right (mkTemperature t)
    }

-- | Codec for Temperature with validation
temperatureCodec :: Toml.Key -> TomlCodec Temperature
temperatureCodec = Toml.match (_Temperature >>> Toml._Double)

-- | BiMap from PositiveInt to Int
_PositiveInt :: Toml.TomlBiMap PositiveInt Int
_PositiveInt =
  Toml.BiMap
    { Toml.forward = Right . unPositiveInt,
      Toml.backward = \n ->
        maybe (Left $ Toml.ArbitraryError $ "Must be positive, got: " <> T.pack (show n)) Right (mkPositiveInt n)
    }

-- | Codec for PositiveInt with validation
positiveIntCodec :: Toml.Key -> TomlCodec PositiveInt
positiveIntCodec = Toml.match (_PositiveInt >>> Toml._Int)

serverCodec :: TomlCodec ServerConfig
serverCodec =
  ServerConfig
    <$> Toml.text "host" .= serverHost
    <*> portCodec "port" .= serverPort
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
    <*> temperatureCodec "temperature" .= llmTemperature

statsCodec :: TomlCodec StatsConfig
statsCodec =
  StatsConfig
    <$> Toml.int "tokens_per_operation" .= statsTokensPerOp
    <*> Toml.double "cost_per_operation" .= statsCostPerOp

gridCodec :: TomlCodec GridConfig
gridCodec =
  GridConfig
    <$> positiveIntCodec "default_rows" .= gridDefaultRows
    <*> positiveIntCodec "default_cols" .= gridDefaultCols

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.table serverCodec "server" .= configServer
    <*> Toml.table llmCodec "llm" .= configLlm
    <*> Toml.table statsCodec "stats" .= configStats
    <*> Toml.table gridCodec "grid" .= configGrid

-- | Validate server configuration
-- Port is already validated by the Port newtype
validateServerConfig :: ServerConfig -> Either ConfigError ()
validateServerConfig _ = pure ()

-- | Validate LLM configuration
-- Temperature is already validated by the Temperature newtype
-- Only need to check API key requirement for non-Ollama providers
validateLlmConfig :: LlmConfig -> Either ConfigError ()
validateLlmConfig LlmConfig {..} = case (llmProvider, llmApiKey) of
  (Ollama, _) -> pure ()
  (_, Just _) -> pure ()
  (provider, Nothing) -> Left $ ConfigValidationError (MissingApiKey provider)

-- | Validate grid configuration
-- Grid dimensions are already validated by the PositiveInt newtype
validateGridConfig :: GridConfig -> Either ConfigError ()
validateGridConfig _ = pure ()

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
