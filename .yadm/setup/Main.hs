module Main where

import Shake
import qualified Config (plugin)
import qualified Apt
import qualified Yadm
import qualified Stack
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
    ]

main :: IO ()
main = shakeWithPlugins pluginList shakeOpts $ do
  vimRules
  yadmConfigRule
  defaultPackageRule



    --           | sed 's/^LS_COLORS=/set -x LS_COLORS /;s/;$//')


-- Things to do during bootstrap
--

-- .bashrc
--    -- Aliases
--    -- Exports
--       -- Pandoc
--       -- Brittany
--    -- Facebook Messenger
--    -- Slack
--    -- Discord
--    -- Cinnamon
--       -- GTK Themes
--       -- Icon Themes
--    -- Vagrant
--    -- Docker
--    -- Virtualbox
--    -- Nix
--    -- Keyboard.io
--    -- other default apt installs
