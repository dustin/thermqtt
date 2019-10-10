{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent              (threadDelay)
import           Control.Monad                   (forM_, forever)
import qualified Data.ByteString.Lazy.Char8      as BC
import           Data.Maybe                      (fromJust)
import           Data.Semigroup                  ((<>))
import           Data.Text                       (Text, pack)
import           Network.URI
import           Options.Applicative             (Parser, auto, execParser,
                                                  fullDesc, help, helper, info,
                                                  long, maybeReader, option,
                                                  progDesc, short, showDefault,
                                                  str, switch, value, (<**>))

import           Network.MQTT.Client
import           System.Hardware.OneWire.Thermal

data Options = Options { optTopic    :: Text
                       , optUri      :: URI
                       , optPeriod   :: Int
                       , optAppendSN :: Bool
                       }

options :: Parser Options
options = Options
  <$> option str (long "topic" <> showDefault <> value "therm/" <>
                   help "mqtt topic - if ends with a slash, sensor serial number will be appended")
  <*> option (maybeReader parseURI) (long "mqtt-uri" <> short 'u' <> showDefault <> value (fromJust $ parseURI "mqtt://localhost/") <> help "mqtt broker URI")
  <*> option auto (long "period" <> showDefault <> value 5 <> help "time between readings")
  <*> switch (long "appendsn" <> help "append serial number to topic")

go :: Options -> IO ()
go Options{..} = do
  mc <- connectURI mqttConfig{_protocol=Protocol50} optUri

  -- This will throw an exception if the MQTT client disconnects and a
  -- message can't be sent.
  forever $ do
    serials <- thermalSerials
    forM_ serials
      (\serial -> do
          val <- thermalSensorCelsius serial
          maybe
            (pure ())
            (\c -> pubAliased mc (mktopic serial) (BC.pack $ show c) True QoS2 [
                PropMessageExpiryInterval 900])
            val)

    threadDelay (optPeriod * 1000000)

  where mktopic :: ThermalSerial -> Text
        mktopic (ThermalSerial s)
          | optAppendSN = optTopic <> pack s
          | otherwise   = optTopic

main :: IO ()
main = execParser opts >>= go

  where opts = info (options <**> helper)
          ( fullDesc <> progDesc "Send MQTT therm updates")
