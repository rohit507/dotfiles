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
  nixSetup
  haskellIdeEngineSetup
  gtermSolarizedSetup
  themePackageSetup
  defaultPackageSetup



    --           | sed 's/^LS_COLORS=/set -x LS_COLORS /;s/;$//')


-- Things to do during bootstrap
--

-- .bashrc
--    -- Aliases
--    -- Exports
--       -- Brittany
--    -- Facebook Messenger
--    -- Docker
--    -- Keyboard.io
--    -- other default apt installs
