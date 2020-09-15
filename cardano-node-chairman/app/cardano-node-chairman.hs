{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Text as Text

import           Cardano.Chairman (chairmanTest)
import           Cardano.Chairman.Options
import           Cardano.Node.Configuration.POM (defaultPartialNodeConfiguration,
                     makeNodeConfiguration, ncProtocol, parseNodeConfigurationFP)
import           Cardano.Prelude hiding (option)
import           Control.Tracer (stdoutTracer)
import           Options.Applicative

main :: IO ()
main = do
  ChairmanArgs
    { caRunningTime
    , caMinProgress
    , caSocketPaths
    , caConfigYaml
    , caSlotLength
    , caSecurityParam
    , caNetworkMagic
    } <- execParser opts

  pnc <- liftIO . parseNodeConfigurationFP $ Just caConfigYaml

  someNodeClientProtocol <-
    case makeNodeConfiguration $ pnc <> defaultPartialNodeConfiguration of
      Left err -> panic . Text.pack $ "Error making the NodeConfiguration for the chairman: " <> err
      Right nc' -> return . mkNodeClientProtocol $ ncProtocol nc'

  chairmanTest
    stdoutTracer
    caSlotLength
    caSecurityParam
    caRunningTime
    caMinProgress
    caSocketPaths
    someNodeClientProtocol
    caNetworkMagic
