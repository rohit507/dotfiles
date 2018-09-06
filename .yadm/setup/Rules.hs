module Rules where

import Shake

import qualified Apt
import qualified Yadm
import qualified Stack
import Util
import Status
import Install

vimRules :: Rules ()
vimRules = action $ Apt.install ["vim","vim-gtk"]

defaultPackageRule :: Rules ()
defaultPackageRule = action $ queryEnv "DEFAULT_PACKAGES" >>= Apt.install

yadmConfigRule :: Rules ()
yadmConfigRule = action $ do
  Yadm.gitconfig "user.name" "Rohit Ramesh"
  Yadm.gitconfig "user.email" "rohit507@gmail.com"

{-
chromeInit :: Rules ()
chromeInit = do
    Apt.install ["wget"]
    cmd_ [Shell] $ "wget -q -O - https://dl.google.com/linux/linux_signing_key.pub"
        ++ " | sudo apt-key add"
    doesFileExist "/etc/apt/sources.list.d/google-chrome.list" >>= \case
        False -> cmd_ [Shell] $ "sudo cp ~/.yadm/google-chrome.list /etc/apt/sources.list.d/"
        True -> return ()
    Apt.update
    Apt.install ["google-chrome-stable"]

fishInit :: Action ()
fishInit = do
    Apt.install ["fish","git"]
    -- Install Plugin manager
    cmd_ [Shell] "curl -L https://get.oh-my.fish | fish"
    -- Make fish our default shell
    fishBin <- which "fish"
    command_ [Traced "chsh"] "chsh" ["--shell",fishBin]


gitInit :: Rules ()
gitInit = action $ Apt.install ["git","git-flow"]


emacsInit :: Action ()
emacsInit = do
    Apt.install ["emacs"]
    -- install Haskell IDE engine as per
    --      https://github.com/haskell/haskell-ide-engine
    -- Spacemacs init
    -- Fonts
    -- Intero / brittany deps
    -- etc..

lastpassInit :: Action ()
lastpassInit = withTempDir $ \ tempDir -> do
    -- TODO add statua flag
    command_ [Cwd tempDir, Shell] "wget" $
        ["https://download.cloud.lastpass.com/linux/lplinux.tar.bz2"]
    cmd_ [Cwd tempDir] "tar xvjf lplinux.tar.bz2"
    cmd_ [Cwd tempDir] "./install_lastpass.sh"

gtermSolarizedInit :: Action ()
gtermSolarizedInit = withTempDir $ \ tempDir -> do
    -- TODO :: add Status flag.
    Apt.install ["dconf-cli"]
    cmd_ [Cwd tempDir, Shell] "git clone https://github.com/Anthony25/gnome-terminal-colors-solarized.git"
    cmd_ [Shell, Cwd $ tempDir </> "gnome-terminal-colors-solarized"] "./install.sh"
    -- Add "eval `dircolors /path/to/dircolorsdb`" in your shell configuration
    --    file (.bashrc, .zshrc, etc...) to use new dircolors.
    -- For Fish, add the following to config.fish instead:
	--      eval (dircolors /path/to/dircolorsdb | head -n 1 |

-}
