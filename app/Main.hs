{-# Language OverloadedStrings #-}

module Main where

import Data.Semigroup ((<>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (newTChanIO)
import Control.Monad (forever, forM_)
import qualified Data.ByteString.Char8 as B
import Data.Text (pack)

import System.Hardware.OneWire.Thermal
import qualified Network.MQTT as MQTT

base :: String
base = "sj/attic/therm/"

mktopic :: ThermalSerial -> MQTT.Topic
mktopic (ThermalSerial s) = (MQTT.toTopic . MQTT.MqttText . pack) (base <> (s :: String))

main :: IO ()
main = do
  cmds <- MQTT.mkCommands
  pubChan <- newTChanIO
  let conf = (MQTT.defaultConfig cmds pubChan)
             { MQTT.cClean = False
             , MQTT.cClientID = "thermqtt"
             , MQTT.cHost = "eve"
             , MQTT.cKeepAlive = Just 10
             , MQTT.cLogDebug = print
             }

  _ <- forkIO $ forever $ do
    serials <- thermalSerials
    forM_ serials
      (\serial -> do
          mc <- thermalSensorCelsius serial
          maybe
            (pure ())
            (\c ->
               MQTT.publish conf MQTT.NoConfirm False (mktopic serial) (B.pack $ show c)
            )
            mc)

    threadDelay (1000000 * 60)

  -- this will throw IOExceptions
  terminated <- MQTT.run conf
  print terminated
