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

-- Things to do during bootstrap
--    -- Yadm
--       -- Decrypt
--    -- Vim
--    -- Fish
--    -- GitFlow 
--    -- Google Chrome 
--    -- LastPass 
--    -- Emacs
--    -- Spacemacs 
--       -- Fonts
--    -- Facebook Messenger
--    -- Slack
--    -- Discord
--    -- Cinnamon
--       -- GTK Themes 
--       -- Icon Themes
--    -- Solarized-Gtern

