module Main where

import Shake 
import qualified Config (plugin)
import qualified Apt

shakeOpts :: ShakeOptions 
shakeOpts = shakeOptions{
      shakeFiles=".build"
    }

pluginList :: [Plugin] 
pluginList = [
      Config.plugin "setup.conf"
    , Apt.plugin 
    ]

main :: IO ()
main = shakeWithPlugins pluginList shakeOpts $ do
    action $ do
        Apt.install ["fish","git","subversion"]

