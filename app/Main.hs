{-# Language OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTChanIO)
import Control.Monad (forever, forM_)
import qualified Data.ByteString.Char8 as B
import Network.Socket (HostName)
import Network.Socket.Internal (PortNumber)
import Data.Text (Text, isSuffixOf, pack)

import Options.Applicative (option, auto, long, showDefault, value, help, helper, fullDesc,
                            progDesc, execParser, info, str,
                            (<**>), Parser)

import System.Hardware.OneWire.Thermal
import qualified Network.MQTT as MQTT

data Options = Options { optTopic :: Text
                       , optHost :: HostName
                       , optPort :: PortNumber
                       , optUser :: Maybe Text
                       , optPass :: Maybe Text
                       , optClient :: Text
                       , optPeriod :: Int
                       }

options :: Parser Options
options = Options
  <$> option str (long "topic" <> showDefault <> value "therm/" <>
                   help "mqtt topic - if ends with a slash, sensor serial number will be appended")
  <*> option str (long "host" <> showDefault <> value "localhost" <> help "mqtt host")
  <*> option auto (long "port" <> showDefault <> value 1883 <> help "mqtt port")
  <*> option auto (long "user" <> value Nothing <> help "mqtt username")
  <*> option auto (long "pass" <> value Nothing <> help "mqtt password")
  <*> option str (long "client" <> value "thermqtt" <> help "mqtt client name")
  <*> option auto (long "period" <> showDefault <> value 5 <> help "time between readings")

mktopic :: Options -> ThermalSerial -> MQTT.Topic
mktopic opts (ThermalSerial s)
  | "/" `isSuffixOf` optTopic opts = fromText $ optTopic opts <> pack s
  | otherwise = fromText $ optTopic opts

  where fromText = MQTT.toTopic . MQTT.MqttText

go :: Options -> IO ()
go opts = do
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
             { MQTT.cClean = False
             , MQTT.cClientID = optClient opts
             , MQTT.cHost = optHost opts
             , MQTT.cPort = optPort opts
             , MQTT.cUsername = optUser opts
             , MQTT.cPassword = optPass opts
             , MQTT.cKeepAlive = Just 10
             }

  _ <- forkIO $ forever $ do
    serials <- thermalSerials
    forM_ serials
      (\serial -> do
          mc <- thermalSensorCelsius serial
          maybe
            (pure ())
            (\c ->
               MQTT.publish conf MQTT.NoConfirm False (mktopic opts serial) (B.pack $ show c)
            )
            mc)

    threadDelay (optPeriod opts * 1000000)

  -- this will throw IOExceptions
  terminated <- MQTT.run conf
  print terminated

main :: IO ()
main = execParser opts >>= go

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Send MQTT therm updates")
