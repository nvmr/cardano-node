
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- TODO: REMOVE ME

module Cardano.Node.Configuration.POM
  ( NodeConfiguration (..)
  , PartialNodeConfiguration(..)
  , defaultPartialNodeConfiguration
  , lastOption
  , makeNodeConfiguration
  , ncProtocol
  , parseNodeConfigurationFP
  )
where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Semigroup (Semigroup (..))
import           Data.Yaml (decodeFileThrow)
import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()
import           Options.Applicative
import           System.FilePath (takeDirectory, (</>))
import           System.Posix.Types (Fd (..))

import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types
import           Cardano.Tracing.Config
import           Ouroboros.Network.Block (MaxSlotNo (..))

data NodeConfiguration
  = NodeConfiguration
      {  ncNodeAddr        :: !(Maybe NodeAddress)
          -- | Filepath of the configuration yaml file. This file determines
          -- all the configuration settings required for the cardano node
          -- (logging, tracing, protocol, slot length etc)
       , ncConfigFile      :: !ConfigYamlFilePath
       , ncTopologyFile    :: !TopologyFile
       , ncDatabaseFile    :: !DbFile
       , ncProtocolFiles   :: !ProtocolFilepaths
       , ncValidateDB      :: !Bool
       , ncShutdownIPC     :: !(Maybe Fd)
       , ncShutdownOnSlotSynced :: !MaxSlotNo

       -- What used to be the NodeConfiguration
        -- Protocol-specific parameters:
       , ncProtocolConfig :: !NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncSocketPath     :: !(Maybe SocketPath)

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: !(Maybe MaxConcurrencyBulkSync)
       , ncMaxConcurrencyDeadline :: !(Maybe MaxConcurrencyDeadline)

         -- Logging parameters:
       , ncViewMode       :: !ViewMode
       , ncLoggingSwitch  :: !Bool
       , ncLogMetrics     :: !Bool
       , ncTraceConfig    :: !TraceOptions
       } deriving Show


data PartialNodeConfiguration
  = PartialNodeConfiguration
      {  -- Previously NodeCLI
         pncNodeAddr        :: !(Last NodeAddress)
         -- | Filepath of the configuration yaml file. This file determines
         -- all the configuration settings required for the cardano node
         -- (logging, tracing, protocol, slot length etc)
       , pncConfigFile      :: !(Last ConfigYamlFilePath)
       , pncTopologyFile    :: !(Last TopologyFile)
       , pncDatabaseFile    :: !(Last DbFile)
--       , pncsocketFile      :: !(Last (Maybe SocketPath))
       , pncProtocolFiles   :: !(Last ProtocolFilepaths)
       , pncValidateDB      :: !(Last Bool)
       , pncShutdownIPC     :: !(Last (Maybe Fd))
       , pncShutdownOnSlotSynced :: !(Last MaxSlotNo)

        -- From here onward was the original NodeConfiguration
          -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncSocketPath     :: !(Last SocketPath)

         -- BlockFetch configuration
       , pncMaxConcurrencyBulkSync :: !(Last MaxConcurrencyBulkSync)
       , pncMaxConcurrencyDeadline :: !(Last MaxConcurrencyDeadline)

         -- Logging parameters:
       , pncViewMode       :: !(Last ViewMode)
       , pncLoggingSwitch  :: !(Last Bool)
       , pncLogMetrics     :: !(Last Bool)
       , pncTraceConfig    :: !(Last TraceOptions)
       } deriving (Eq, Generic, Show)

instance AdjustFilePaths PartialNodeConfiguration where
  adjustFilePaths f x@PartialNodeConfiguration {
                        pncProtocolConfig,
                        pncSocketPath
                      } =
    x {
      pncProtocolConfig = adjustFilePaths f pncProtocolConfig,
      pncSocketPath     = adjustFilePaths f pncSocketPath
    }

instance AdjustFilePaths (Last NodeProtocolConfiguration) where

  adjustFilePaths f (Last (Just (NodeProtocolConfigurationByron pc))) =
    Last . Just $ NodeProtocolConfigurationByron (adjustFilePaths f pc)

  adjustFilePaths f (Last (Just (NodeProtocolConfigurationShelley pc))) =
    Last . Just $ NodeProtocolConfigurationShelley (adjustFilePaths f pc)

  adjustFilePaths f (Last (Just (NodeProtocolConfigurationCardano pcb pcs pch))) =
    Last . Just $ NodeProtocolConfigurationCardano (adjustFilePaths f pcb)
                                                   (adjustFilePaths f pcs)
                                                   pch
  adjustFilePaths _ (Last Nothing) = Last Nothing

instance AdjustFilePaths (Last SocketPath) where
  adjustFilePaths f (Last (Just (SocketPath p))) = Last . Just $ SocketPath (f p)
  adjustFilePaths _ (Last Nothing) = Last Nothing


instance Semigroup PartialNodeConfiguration where
  (<>) = gmappend

instance Monoid PartialNodeConfiguration where
  mempty = PartialNodeConfiguration mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
  mappend x y = PartialNodeConfiguration
                  { pncNodeAddr = pncNodeAddr x <> pncNodeAddr y
                  , pncConfigFile = pncConfigFile x <> pncConfigFile y
                  , pncTopologyFile = pncTopologyFile x <> pncTopologyFile y
                  , pncDatabaseFile = pncDatabaseFile x <> pncDatabaseFile y
          --        , pncsocketFile = pncSocketFile x <> pncSocketFile y
                  , pncProtocolFiles = pncProtocolFiles x <> pncProtocolFiles y
                  , pncValidateDB = pncValidateDB x <> pncValidateDB y
                  , pncShutdownIPC = pncShutdownIPC x <> pncShutdownIPC y
                  , pncShutdownOnSlotSynced = pncShutdownOnSlotSynced x <> pncShutdownOnSlotSynced y
                  , pncProtocolConfig = pncProtocolConfig x <> pncProtocolConfig y
                  , pncSocketPath = pncSocketPath x <> pncSocketPath y
                  , pncMaxConcurrencyBulkSync = pncMaxConcurrencyBulkSync x <> pncMaxConcurrencyBulkSync y
                  , pncMaxConcurrencyDeadline = pncMaxConcurrencyDeadline x <> pncMaxConcurrencyDeadline y
                  , pncViewMode = pncViewMode x <> pncViewMode y
                  , pncLoggingSwitch = pncLoggingSwitch x <> pncLoggingSwitch y
                  , pncLogMetrics = pncLogMetrics x <> pncLogMetrics y
                  , pncTraceConfig = pncTraceConfig x <> pncTraceConfig y
                  }


instance FromJSON PartialNodeConfiguration where
  parseJSON =
    withObject "PartialNodeConfiguration" $ \v -> do

      -- Node parameters, not protocol-specific
      pncSocketPath' <- Last <$> v .:? "SocketPath"

      -- Blockfetch parameters
      pncMaxConcurrencyBulkSync' <- Last <$> v .:? "MaxConcurrencyBulkSync"
      pncMaxConcurrencyDeadline' <- Last <$> v .:? "MaxConcurrencyDeadline"

      -- Logging parameters
      pncViewMode'      <- Last <$> v .:? "ViewMode"         -- This needs to be in the default config .!= SimpleView
      pncLoggingSwitch' <- v .:? "TurnOnLogging" .!= True   -- This needs to be in the default config .!= True
      pncLogMetrics'    <- Last <$> v .:? "TurnOnLogMetrics" -- This needs to be in the default config .!= True
      pncTraceConfig'   <- if pncLoggingSwitch' -- TODO: Need to unpackage last and check the bool
                           then Last . Just <$> traceConfigParser v
                           else return . Last $ Just TracingOff

      -- Protocol parameters
      protocol <-  v .:? "Protocol" .!= ByronProtocol -- Should be in the default config
      pncProtocolConfig' <-
        case protocol of
          ByronProtocol ->
            Last . Just . NodeProtocolConfigurationByron <$> parseByronProtocol v

          ShelleyProtocol ->
            Last . Just . NodeProtocolConfigurationShelley <$> parseShelleyProtocol v

          CardanoProtocol ->
            Last . Just  <$> (NodeProtocolConfigurationCardano <$> parseByronProtocol v
                                                               <*> parseShelleyProtocol v
                                                               <*> parseHardForkProtocol v)
      pure mempty {
             pncProtocolConfig = pncProtocolConfig'
           , pncSocketPath = pncSocketPath'
           , pncMaxConcurrencyBulkSync = pncMaxConcurrencyBulkSync'
           , pncMaxConcurrencyDeadline = pncMaxConcurrencyDeadline'
           , pncViewMode = pncViewMode'
           , pncLoggingSwitch = Last $ Just pncLoggingSwitch'
           , pncLogMetrics = pncLogMetrics'
           , pncTraceConfig = pncTraceConfig'
           }
    where
      parseByronProtocol v = do
        primary   <- v .:? "ByronGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcByronGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ByronGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ByronGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcByronGenesisFileHash <- v .:? "ByronGenesisHash"

        npcByronReqNetworkMagic     <- v .:? "RequiresNetworkMagic"
                                         .!= RequiresNoMagic
        npcByronPbftSignatureThresh <- v .:? "PBftSignatureThreshold"
        npcByronApplicationName     <- v .:? "ApplicationName"
                                         .!= Byron.ApplicationName "cardano-sl"
        npcByronApplicationVersion  <- v .:? "ApplicationVersion" .!= 1
        protVerMajor                <- v .: "LastKnownBlockVersion-Major"
        protVerMinor                <- v .: "LastKnownBlockVersion-Minor"
        protVerAlt                  <- v .: "LastKnownBlockVersion-Alt" .!= 0

        pure NodeByronProtocolConfiguration {
               npcByronGenesisFile
             , npcByronGenesisFileHash
             , npcByronReqNetworkMagic
             , npcByronPbftSignatureThresh
             , npcByronApplicationName
             , npcByronApplicationVersion
             , npcByronSupportedProtocolVersionMajor = protVerMajor
             , npcByronSupportedProtocolVersionMinor = protVerMinor
             , npcByronSupportedProtocolVersionAlt   = protVerAlt
             }

      parseShelleyProtocol v = do
        primary   <- v .:? "ShelleyGenesisFile"
        secondary <- v .:? "GenesisFile"
        npcShelleyGenesisFile <-
          case (primary, secondary) of
            (Just g, Nothing)  -> return g
            (Nothing, Just g)  -> return g
            (Nothing, Nothing) -> fail $ "Missing required field, either "
                                      ++ "ShelleyGenesisFile or GenesisFile"
            (Just _, Just _)   -> fail $ "Specify either ShelleyGenesisFile"
                                      ++ "or GenesisFile, but not both"
        npcShelleyGenesisFileHash <- v .:? "ShelleyGenesisHash"

        --TODO: these are silly names, allow better aliases:
        protVerMajor    <- v .:  "LastKnownBlockVersion-Major"
        protVerMinor    <- v .:  "LastKnownBlockVersion-Minor"
        protVerMajroMax <- v .:? "MaxKnownMajorProtocolVersion" .!= 1

        pure NodeShelleyProtocolConfiguration {
               npcShelleyGenesisFile
             , npcShelleyGenesisFileHash
             , npcShelleySupportedProtocolVersionMajor = protVerMajor
             , npcShelleySupportedProtocolVersionMinor = protVerMinor
             , npcShelleyMaxSupportedProtocolVersion   = protVerMajroMax
             }

      parseHardForkProtocol v = do
        npcTestShelleyHardForkAtEpoch   <- v .:? "TestShelleyHardForkAtEpoch"
        npcTestShelleyHardForkAtVersion <- v .:? "TestShelleyHardForkAtVersion"
        npcShelleyHardForkNotBeforeEpoch <- v .:? "ShelleyHardForkNotBeforeEpoch"
        pure NodeHardForkProtocolConfiguration {
               npcTestShelleyHardForkAtEpoch,
               npcTestShelleyHardForkAtVersion,
               npcShelleyHardForkNotBeforeEpoch
             }

-- Default configuration is mainnet
defaultPartialNodeConfiguration :: PartialNodeConfiguration
defaultPartialNodeConfiguration = mempty
                                    { pncConfigFile = Last . Just $ ConfigYamlFilePath "configuration/cardano/mainnet-config.json"
                                    , pncDatabaseFile = Last . Just $ DbFile "mainnet/db/"
                                    , pncLoggingSwitch = Last $ Just True
                                    , pncSocketPath = Last $ Just "mainnet/socket/nodesocket"
                                    , pncTopologyFile = Last . Just $ TopologyFile "configuration/cardano/mainnet-topology.json"
                                    , pncViewMode = Last $ Just SimpleView
                                    }

lastOption :: Parser a -> Parser (Last a)
lastOption parser = fmap Last $ optional parser

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfiguration
makeNodeConfiguration pnc = do
  configFile <- lastToEither "Missing YAML config file" $ pncConfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pncTopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncDatabaseFile pnc
  protocolFiles <- lastToEither "Missing ProtocolFiles" $ pncProtocolFiles pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncValidateDB pnc
  shutdownIPC <- lastToEither "Missing ShutdownIPC" $ pncShutdownIPC pnc
  shutdownOnSlotSynced <- lastToEither "Missing ShutdownOnSlotSynced" $ pncShutdownOnSlotSynced pnc
  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  viewMode <- lastToEither "Missing ViewMode" $ pncViewMode pnc
  loggingSwitch <- lastToEither "Missing LoggingSwitch" $ pncLoggingSwitch pnc
  logMetrics <- lastToEither "Missing LogMetrics" $ pncLogMetrics pnc
  traceConfig <- lastToEither "Missing TraceConfig" $ pncTraceConfig pnc
  return $ NodeConfiguration
             { ncNodeAddr = getLast $ pncNodeAddr pnc
             , ncConfigFile = configFile
             , ncTopologyFile = topologyFile
             , ncDatabaseFile = databaseFile
             , ncProtocolFiles = protocolFiles
             , ncValidateDB = validateDB
             , ncShutdownIPC = shutdownIPC
             , ncShutdownOnSlotSynced = shutdownOnSlotSynced
             , ncProtocolConfig = protocolConfig
             , ncSocketPath = getLast $ pncSocketPath pnc
             , ncMaxConcurrencyBulkSync = getLast $ pncMaxConcurrencyBulkSync pnc
             , ncMaxConcurrencyDeadline = getLast $ pncMaxConcurrencyDeadline pnc
             , ncViewMode = viewMode
             , ncLoggingSwitch = loggingSwitch
             , ncLogMetrics = logMetrics
             , ncTraceConfig = traceConfig
             }

ncProtocol :: NodeConfiguration -> Protocol
ncProtocol nc =
    case ncProtocolConfig nc of
      NodeProtocolConfigurationByron{}   -> ByronProtocol
      NodeProtocolConfigurationShelley{} -> ShelleyProtocol
      NodeProtocolConfigurationCardano{} -> CardanoProtocol


parseNodeConfigurationFP :: Maybe ConfigYamlFilePath -> IO PartialNodeConfiguration
parseNodeConfigurationFP Nothing = parseNodeConfigurationFP . getLast $ pncConfigFile defaultPartialNodeConfiguration
parseNodeConfigurationFP (Just (ConfigYamlFilePath fp)) = do
    nc <- decodeFileThrow fp
    -- Make all the files be relative to the location of the config file.
    pure $ adjustFilePaths (takeDirectory fp </>) nc
