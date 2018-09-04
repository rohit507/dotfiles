module Main where

import Shake 
import qualified Config (plugin)
import qualified Apt
import qualified Yadm
import qualified Stack
import Util

shakeOpts :: ShakeOptions 
shakeOpts = shakeOptions{
      shakeFiles=".build"
    }

pluginList :: [Plugin] 
pluginList = [
      Config.plugin "setup.conf"
    , Apt.plugin 
    , Yadm.plugin
    , Stack.plugin
    ]

main :: IO ()
main = shakeWithPlugins pluginList shakeOpts $ do
    action $ do
        Apt.install ["tree"]
        yadmInit
        gitInit 
        vimInit
        emacsInit
        gtermInit
        -- fishInit
        -- chromeInit
        -- lastpassInit 


yadmInit :: Action ()
yadmInit = Yadm.decrypt >> Yadm.alt

fishInit :: Action () 
fishInit = do
    Apt.install ["fish","git"]
    -- Install Plugin manager 
    cmd_ [Shell] "curl -L https://get.oh-my.fish | fish"
    -- Make fish our default shell 
    fishBin <- which "fish" 
    command_ [Traced "chsh"] "chsh" ["--shell",fishBin]

vimInit :: Action () 
vimInit = Apt.install ["vim","vim-gtk","oni"]

gitInit :: Action () 
gitInit = Apt.install ["git","git-flow"]

chromeInit :: Action ()
chromeInit = do
    Apt.install ["wget"]
    cmd_ [Shell] $ "wget -q -O - https://dl.google.com/linux/linux_signing_key.pub"
        ++ " | sudo apt-key add"
    doesFileExist "/etc/apt/sources.list.d/google-chrome.list" >>= \case 
        False -> cmd_ [Shell] $ "sudo cp ~/.yadm/google-chrome.list /etc/apt/sources.list.d/" 
        True -> return ()
    Apt.update
    Apt.install ["google-chrome-stable"]

emacsInit :: Action () 
emacsInit = do 
    Apt.install ["emacs"] 
    -- Spacemacs init
    -- Fonts
    -- Intero / brittany deps 
    -- etc...

lastpassInit :: Action () 
lastpassInit = withTempDir $ \ tempDir -> do
    -- TODO add statua flag
    command_ [Cwd tempDir, Shell] "wget" $
        ["https://download.cloud.lastpass.com/linux/lplinux.tar.bz2"]
    cmd_ [Cwd tempDir] "tar xvjf lplinux.tar.bz2"
    cmd_ [Cwd tempDir] "./install_lastpass.sh"

gtermSolarizedInit :: Action ()
gtermSolarizedInit = withTempDir $ \ tempDir -> do 
    Apt.install ["dconf-cli"] 
    cmd_ [Cwd tempDir, Shell] "git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git"
    cmd_ [Shell, Cwd $ tempDir </> "gnome-terminal-colors-solarized"] "./install.sh"  
    -- Add "eval `dircolors /path/to/dircolorsdb`" in your shell configuration 
    --    file (.bashrc, .zshrc, etc...) to use new dircolors.
    -- For Fish, add the following to config.fish instead:
	--      eval (dircolors /path/to/dircolorsdb | head -n 1 | 
    --           | sed 's/^LS_COLORS=/set -x LS_COLORS /;s/;$//')


-- Things to do during bootstrap
--

-- .bashrc
--    -- Aliases 
--    -- Exports
--    -- Stack 
--       -- Pandoc 
--       -- Brittany 
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
--    -- Gimp
--    -- Inkscape
--    -- LXC / LXD
--    -- virtual box
--    -- Vagrant
--    -- Docker
--    -- Virtualbox 
--    -- Nix
--    -- other default apt installs 
