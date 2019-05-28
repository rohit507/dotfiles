module Main where

import Shake
import qualified Config (plugin)
import qualified Apt
import qualified Yadm
import qualified Stack
import qualified Status
import Rules

shakeOpts :: ShakeOptions
shakeOpts = shakeOptions{
      shakeFiles=".build"
    }

pluginList :: [Plugin]
pluginList = [
      Config.plugin "config.yaml"
    , Apt.plugin
    , Yadm.plugin
    , Stack.plugin
    , Status.plugin
    ]

main :: IO ()
main = shakeWithPlugins pluginList shakeOpts $ do
  vimSetup
  gitSetup
  defaultPackageSetup
  yadmConfigSetup "Rohit Ramesh" "rohit507@gmail.com"
  lastpassSetup
  chromeSetup
  fishSetup
  fontSetup
  slackSetup
  discordSetup
  vagrantSetup
  dockerSetup
  pandocSetup
  -- fbMessengerSetup
  nixSetup
  -- haskellIdeEngineSetup
  -- gtermSolarizedSetup
  themePackageSetup
