module Rules where

import Shake

import qualified Apt
import qualified Yadm
import qualified Stack
import Util
import Status
import Install

vimSetup :: Rules ()
vimSetup = action $ Apt.install ["vim","vim-gtk"]

defaultPackageSetup :: Rules ()
defaultPackageSetup = action $ queryEnv "DEFAULT_PACKAGES" >>= Apt.install

themePackageSetup :: Rules ()
themePackageSetup = action $ queryEnv "THEME_PACKAGES" >>= Apt.install

yadmConfigSetup :: String -> String -> Rules ()
yadmConfigSetup name email = action $ do
  Yadm.gitconfig "user.name" name
  Yadm.gitconfig "user.email" email

gitSetup :: Rules ()
gitSetup = action $ Apt.install ["git","git-flow"]


lastpassSetup :: Rules ()
lastpassSetup = withInstallDir "lastpass" $ \ dir -> do

  -- Download the file
  dir </> "lplinux.tar.bz2" %> $ \ out ->
      command_ [Cwd dir, Shell] "wget"
      ["https://download.cloud.lastpass.com/linux/lplinux.tar.bz2"]

  -- extract the contents
  map (dir </>) ["install_lastpass.sh"] %>
    cmd_ [Cwd dir] "tar xvjf lplinux.tar.bz2"

  -- run the install script
  lpFlag <- withStatusFlag "lastpass.installed" $ cmd_ [Cwd tempDir] "./install_lastpass.sh"

  need [lpFlag]

chromeSetup :: Rules ()
chromeSetup = withInstallDir "google-chrome" $ \dir -> do

  let keyFile = "linux_signing_key.pub"
      sourceFile = "google-chrome.list"
      sourceFileLoc = "${YADM_ROOT}" </> "google-chrome"
      sourceFileTgt = "/etc/apt/sources.list.d"

  dir </> keyFile %> $ \out -> do
    Apt.install ["wget"]
    let fn = takeFileName out
        url = "https://dl.google.com/linux/" ++ fn
    command_ [Cwd dir, Shell] "wget" ["-q", "-O",fn, url]

  keyFlag <- withStatusFlag "google-chrome/key.installed" $ do
    key <- readFile' $  dir </> "linux_signing_key.pub"
    command_ [Stdin key] "sudo" ["apt-key,"add"]

  sourceFileTgt </> sourceFile %> $ \ out -> do
    let stored = sourceFileLoc </> sourceFile
    want [stored]
    command_ [] "sudo" ["cp",stored,out]

  chromeFlag <- withStatusFlag "google-chrome/chrome.installed" $ do
    want [keyFlag, sourceFileTgt </> sourceFile]
    Apt.install ["google-chrome"]

  need [chromeFlag]

fishSetup :: Rules ()
fishSetup = do
   fishInstalledFlag <- withStatusFlag "fish.default" $ do
      Apt.install ["fish","git"]
      fishBin <- which "fish"
      usr <- queryEnv "USER"
      command_ [Traced "chsh"] "sudo" ["-u", usr, "--shell", fishBin]

   need [fishInstalledFlag]

gtermSolarizedInit :: Rules ()
gtermSolarizedInit = withInstallDir "getrm-solarized" $ $ \ dir -> do

    let gitUrl = "https://github.com/Anthony25/gnome-terminal-colors-solarized.git"
        installer = dir </> "install.sh"

    installer %> $\ out -> do
      Apt.install ["git"]
      command_ [Cwd dir] "git" ["clone", gitUrl, "./"]

    gtermFlag <- withStatusFlag "gterm-solarized.installed" $ do
      -- TODO :: add Status flag.
      Apt.install ["dconf-cli"]
      want [installer]
      cmd_ [] installer []
    -- Add "eval `dircolors /path/to/dircolorsdb`" in your shell configuration
    --    file (.bashrc, .zshrc, etc...) to use new dircolors.
    -- For Fish, add the following to config.fish instead:
	--      eval (dircolors /path/to/dircolorsdb | head -n 1 |

emacsInit :: Rules ()
emacsInit = do
    Apt.install ["emacs"]
    -- install Haskell IDE engine as per
    --      https://github.com/haskell/haskell-ide-engine
    -- Spacemacs init
    -- Fonts
    -- Intero / brittany deps
    -- etc..



-}
