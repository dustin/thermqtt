{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (forM_, forever)
import qualified Data.ByteString.Lazy.Char8      as BC
import           Data.Semigroup                  ((<>))
import           Data.Text                       (Text, pack)

import           Options.Applicative             (Parser, auto, execParser,
                                                  fullDesc, help, helper, info,
                                                  long, option, progDesc,
                                                  showDefault, str, switch,
                                                  value, (<**>))

import           Network.MQTT.Client
import           System.Hardware.OneWire.Thermal

data Options = Options { optTopic    :: Text
                       , optHost     :: String
                       , optPort     :: Int
                       , optUser     :: String
                       , optPass     :: String
                       , optClient   :: String
                       , optPeriod   :: Int
                       , optAppendSN :: Bool
                       }

options :: Parser Options
options = Options
  <$> option str (long "topic" <> showDefault <> value "therm/" <>
                   help "mqtt topic - if ends with a slash, sensor serial number will be appended")
  <*> option str (long "host" <> showDefault <> value "localhost" <> help "mqtt host")
  <*> option auto (long "port" <> showDefault <> value 1883 <> help "mqtt port")
  <*> option str (long "user" <> value "" <> help "mqtt username")
  <*> option str (long "pass" <> value "" <> help "mqtt password")
  <*> option str (long "client" <> value "thermqtt" <> help "mqtt client name")
  <*> option auto (long "period" <> showDefault <> value 5 <> help "time between readings")
  <*> switch (long "appendsn" <> help "append serial number to topic")

go :: Options -> IO ()
go Options{..} = do
  mc <- runClient mqttConfig{_hostname=optHost, _port=optPort, _connID=optClient,
                             _cleanSession=False,
                             _username=nilly $ optUser, _password=nilly $ optPass
                            }

  -- This will throw an exception if the MQTT client disconnects and a
  -- message can't be sent.
  forever $ do
    serials <- thermalSerials
    forM_ serials
      (\serial -> do
          val <- thermalSensorCelsius serial
          maybe
            (pure ())
            (\c -> publishq mc (mktopic serial) (BC.pack $ show c) True QoS1)
            val)

    threadDelay (optPeriod * 1000000)

  where nilly "" = Nothing
        nilly s  = Just s

        mktopic :: ThermalSerial -> Text
        mktopic (ThermalSerial s)
          | optAppendSN = optTopic <> pack s
          | otherwise = optTopic

main :: IO ()
main = execParser opts >>= go

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Send MQTT therm updates")
