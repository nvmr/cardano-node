
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Cardano.Node.Configuration.POM
  ( defaultPartialNodeConfiguration
  , makeNodeConfiguration
  )
where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Semigroup (Semigroup (..))
import           Generic.Data (gmappend)
import           Generic.Data.Orphans ()
import           System.Posix.Types (Fd (..))

import qualified Cardano.Chain.Update as Byron
import           Cardano.Crypto (RequiresNetworkMagic (..))
import           Cardano.Node.Protocol.Types (Protocol (..))
import           Cardano.Node.Types hiding (NodeConfiguration (..))
import           Cardano.Tracing.Config
import           Ouroboros.Network.Block (MaxSlotNo (..))

data NodeConfigurationF
  = NodeConfigurationF
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
       , ncProtocolConfig :: NodeProtocolConfiguration

         -- Node parameters, not protocol-specific:
       , ncSocketPath     :: Maybe SocketPath

         -- BlockFetch configuration
       , ncMaxConcurrencyBulkSync :: Maybe MaxConcurrencyBulkSync
       , ncMaxConcurrencyDeadline :: Maybe MaxConcurrencyDeadline

         -- Logging parameters:
       , ncViewMode       :: ViewMode
       , ncLoggingSwitch  :: Bool
       , ncLogMetrics     :: Bool
       , ncTraceConfig    :: TraceOptions
       } deriving Show


data PartialNodeConfiguration
  = PartialNodeConfiguration
      {  -- Previously NodeCLI
         pncnodeAddr        :: !(Last (Maybe NodeAddress))
         -- | Filepath of the configuration yaml file. This file determines
         -- all the configuration settings required for the cardano node
         -- (logging, tracing, protocol, slot length etc)
       , pncconfigFile      :: !(Last ConfigYamlFilePath)
       , pnctopologyFile    :: !(Last TopologyFile)
       , pncdatabaseFile    :: !(Last DbFile)
--       , pncsocketFile      :: !(Last (Maybe SocketPath))
       , pncprotocolFiles   :: !(Last ProtocolFilepaths)
       , pncvalidateDB      :: !(Last Bool)
       , pncshutdownIPC     :: !(Last (Maybe Fd))
       , pncshutdownOnSlotSynced :: !(Last MaxSlotNo)

        -- From here onward was the original NodeConfiguration
          -- Protocol-specific parameters:
       , pncProtocolConfig :: !(Last NodeProtocolConfiguration)

         -- Node parameters, not protocol-specific:
       , pncSocketPath     :: !(Last (Maybe SocketPath))

         -- BlockFetch configuration
       , pncMaxConcurrencyBulkSync :: !(Last (Maybe MaxConcurrencyBulkSync))
       , pncMaxConcurrencyDeadline :: !(Last (Maybe MaxConcurrencyDeadline))

         -- Logging parameters:
       , pncViewMode       :: !(Last ViewMode)
       , pncLoggingSwitch  :: !(Last Bool)
       , pncLogMetrics     :: !(Last Bool)
       , pncTraceConfig    :: !(Last TraceOptions)
       } deriving (Eq, Generic, Show)

instance Semigroup PartialNodeConfiguration where
  (<>) = gmappend

instance Monoid PartialNodeConfiguration where
  mempty = PartialNodeConfiguration mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty
  mappend x y = PartialNodeConfiguration
                  { pncnodeAddr = pncnodeAddr x <> pncnodeAddr y
                  , pncconfigFile = pncconfigFile x <> pncconfigFile y
                  , pnctopologyFile = pnctopologyFile x <> pnctopologyFile y
                  , pncdatabaseFile = pncdatabaseFile x <> pncdatabaseFile y
          --        , pncsocketFile = pncsocketFile x <> pncsocketFile y
                  , pncprotocolFiles = pncprotocolFiles x <> pncprotocolFiles y
                  , pncvalidateDB = pncvalidateDB x <> pncvalidateDB y
                  , pncshutdownIPC = pncshutdownIPC x <> pncshutdownIPC y
                  , pncshutdownOnSlotSynced = pncshutdownOnSlotSynced x <> pncshutdownOnSlotSynced y
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
defaultPartialNodeConfiguration :: PartialNodeConfiguration
defaultPartialNodeConfiguration = mempty
                                    { pncViewMode = Last $ Just SimpleView
                                    , pncLoggingSwitch = Last $ Just True
                                    }

lastToEither :: String -> Last a -> Either String a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeNodeConfiguration :: PartialNodeConfiguration -> Either String NodeConfigurationF
makeNodeConfiguration pnc = do
  nodeAddr <- lastToEither "Missing Node Address" $ pncnodeAddr pnc
  configFile <- lastToEither "Missing ConfigFile" $ pncconfigFile pnc
  topologyFile <- lastToEither "Missing TopologyFile" $ pnctopologyFile pnc
  databaseFile <- lastToEither "Missing DatabaseFile" $ pncdatabaseFile pnc
--  socketFile <- lastToEither "Missing SocketFile" $ pncsocketFile pnc
  protocolFiles <- lastToEither "Missing ProtocolFiles" $ pncprotocolFiles pnc
  validateDB <- lastToEither "Missing ValidateDB" $ pncvalidateDB pnc
  shutdownIPC <- lastToEither "Missing ShutdownIPC" $ pncshutdownIPC pnc
  shutdownOnSlotSynced <- lastToEither "Missing ShutdownOnSlotSynced" $ pncshutdownOnSlotSynced pnc



  protocolConfig <- lastToEither "Missing ProtocolConfig" $ pncProtocolConfig pnc
  socketPath <- lastToEither "Missing SocketPath" $ pncSocketPath pnc
  maxConcurrencyBulkSync <- lastToEither "Missing MaxConcurrencyBulkSync" $ pncMaxConcurrencyBulkSync pnc
  maxConcurrencyDeadline <- lastToEither "Missing MaxConcurrencyDeadline" $ pncMaxConcurrencyDeadline pnc
  viewMode <- lastToEither "Missing ViewMode" $ pncViewMode pnc
  loggingSwitch <- lastToEither "Missing LoggingSwitch" $ pncLoggingSwitch pnc
  logMetrics <- lastToEither "Missing LogMetrics" $ pncLogMetrics pnc
  traceConfig <- lastToEither "Missing TraceConfig" $ pncTraceConfig pnc
  return $ NodeConfigurationF
             { ncNodeAddr = nodeAddr
             , ncConfigFile = configFile
             , ncTopologyFile = topologyFile
             , ncDatabaseFile = databaseFile
             , ncProtocolFiles = protocolFiles
             , ncValidateDB = validateDB
             , ncShutdownIPC = shutdownIPC
             , ncShutdownOnSlotSynced = shutdownOnSlotSynced
             , ncProtocolConfig = protocolConfig
             , ncSocketPath = socketPath
             , ncMaxConcurrencyBulkSync = maxConcurrencyBulkSync
             , ncMaxConcurrencyDeadline = maxConcurrencyDeadline
             , ncViewMode = viewMode
             , ncLoggingSwitch = loggingSwitch
             , ncLogMetrics = logMetrics
             , ncTraceConfig = traceConfig
             }
