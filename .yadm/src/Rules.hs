module Rules where

import Shake
import Control.Monad

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

pandocSetup :: Rules ()
pandocSetup = want . (:[]) =<< withStatusFlag "pandoc.installed"
  (Stack.install "pandoc")

fbMessengerSetup :: Rules ()
fbMessengerSetup = withInstallDir "fb-messenger" $ \ dir -> do

    (dir </> "fb-messenger.deb") %> \out ->
      command_
        [Cwd dir]
        "wget"
        ["https://updates.messengerfordesktop.com/download/linux/latest/beta?arch=amd64&pkg=deb"
        ,"-O", "fb-messenger.deb"]

    fbmFlag <-
      withStatusFlag "fb-messenger.installed" $ do
        need [dir </> "fb-messenger.deb"]
        command_ [Cwd dir] "sudo" ["dpkg", "-i", "fb-messenger.deb"]
  -- asser we want to install lastpass
    want [fbmFlag]


lastpassSetup :: Rules ()
lastpassSetup =
  withInstallDir "lastpass" $ \dir -> do
  -- Download the file
    (dir </> "lplinux.tar.bz2") %> \out ->
      command_
        [Cwd dir, Shell]
        "wget"
        ["https://download.cloud.lastpass.com/linux/lplinux.tar.bz2"]
  -- extract the contents
    map
       (dir </>)
       [ "install_lastpass.sh"
       , "nplastpass"
       , "nplastpass64"
       , "uninstall_lastpass.sh"
       ] &%> \out -> do
      need [dir </> "lplinux.tar.bz2"]
      cmd_ [Cwd dir] "tar xvjf lplinux.tar.bz2"
  -- run the install script
    lpFlag <-
      withStatusFlag "lastpass.installed" $ do
        need [dir </> "install_lastpass.sh"]
        cmd_ [Cwd dir] "./install_lastpass.sh"
  -- asser we want to install lastpass
    want [lpFlag]

chromeSetup :: Rules ()
chromeSetup =
  withInstallDir "google-chrome" $ \dir -> do

    let keyFile = "linux_signing_key.pub"
        sourceFile = "google-chrome.list"
        sourceFileLoc = "${YADM_ROOT}" </> "google-chrome"
        sourceFileTgt = "/etc/apt/sources.list.d"

    (dir </> keyFile) %> \out -> do
      Apt.install ["wget"]
      let fn = takeFileName out
          url = "https://dl.google.com/linux/" ++ fn
      command_ [Cwd dir, Shell] "wget" ["-q", "-O", fn, url]

    keyFlag <-
      withStatusFlag "google-chrome/key.installed" $ do
        need [dir </> keyFile]
        command_
          [FileStdin $ dir </> keyFile]
          "sudo"
          ["apt-key", "add"]

    (sourceFileTgt </> sourceFile) %> \out -> do
      stored <- applyEnv $ sourceFileLoc </> sourceFile
      need [stored]
      command_ [] "sudo" ["cp", stored, out]

    chromeFlag <-
      withStatusFlag "google-chrome/chrome.installed" $ do
        need [keyFlag, sourceFileTgt </> sourceFile]
        Apt.install ["google-chrome-stable"]
    want [chromeFlag]

dockerSetup :: Rules ()
dockerSetup =
  withInstallDir "docker" $ \dir -> do

    let keyFile = "docker_key.pub"
        sourceFileTgt = "/etc/apt/sources.list.d"
        sourceFile = "yadm-docker.list"
        sourceContents =
          "deb [arch=amd64] " ++
          "https://download.docker.com/linux/ubuntu " ++ "bionic" ++ " stable"

    (dir </> keyFile) %> \out -> do
      Apt.install ["wget"]
      let url = "https://download.docker.com/linux/ubuntu/gpg"
      command_ [Cwd dir, Shell] "wget" ["-q", "-O", out, url]

    keyFlag <-
      withStatusFlag "docker/key.installed" $ do
        Apt.install
          [ "apt-transport-https"
          , "ca-certificates"
          , "software-properties-common"
          ]
        need [dir </> keyFile]
        command_ [FileStdin $ dir </> keyFile] "sudo" ["apt-key", "add"]

    listsFlag <-
      withStatusFlag "docker/lists.installed" $ withTempFile $ \file -> do
        Apt.install
          [ "apt-transport-https"
          , "ca-certificates"
          , "software-properties-common"
          ]

        need [keyFlag]
        writeFile' file sourceContents
        command_ [] "sudo" ["cp", file, sourceFileTgt </> sourceFile]
        Apt.update

    dockerFlag <-
      withStatusFlag "docker/docker.installed" $ do
        need [listsFlag,keyFlag]
        Apt.install ["docker-ce"]

    want [dockerFlag]



vagrantSetup :: Rules ()
vagrantSetup = do
   vagrantKeyFlag <- withStatusFlag "vagrant/key.installed" $
        command_
          []
          "sudo"
          ["apt-key", "adv"
          ,"--keyserver", "hkp://keyserver.ubuntu.com:80"
          ,"--recv-key", "AD319E0F7CFFA38B4D9F6E55CE3F3DE92099F7A4"]

   vagrantFlag <- withStatusFlag "vagrant.installed" $ do
     let sourceContents = "deb https://vagrant-deb.linestarve.com/ any main"
         sourceFileTgt = "/etc/apt/sources.list.d"
         sourceFile = "wolfgang42-vagrant.list"

     withTempFile $ \file -> do
       writeFile' file sourceContents
       command_ [] "sudo" ["cp", file, sourceFileTgt </> sourceFile]

     need [vagrantKeyFlag]
     Apt.update

   want [vagrantFlag]

fishSetup :: Rules ()
fishSetup = do
  fishInstalledFlag <-
    withStatusFlag "fish.default" $ do
      Apt.install ["fish", "git"]
      -- fishBin <- which "fish"
      -- command_ [Timeout 10] "chsh" ["--shell", fishBin]

  want [fishInstalledFlag]


nixSetup :: Rules ()
nixSetup = do
  nixFlag <- withStatusFlag "nix.installed" $ do
    Apt.install ["libgtk3-nocsd0"]
    Stdout script <- command [] "curl" ["https://nixos.org/nix/install"]
    command_ [] "sh" ["-c", script, "--daemon"]

  want [nixFlag]

slackSetup :: Rules ()
slackSetup = do
  slackInstalledFlag <- withStatusFlag "slack.installed" $ do
    Apt.install ["snap","snapd-xdg-open"]
    command_ [] "sudo" ["snap", "install", "slack", "--classic"]
  want [slackInstalledFlag]

discordSetup :: Rules ()
discordSetup = do
  discordInstalledFlag <- withStatusFlag "discord.installed" $ do
    Apt.install ["snap","snapd-xdg-open"]
    command_ [] "sudo" ["snap", "install", "discord"]
  want [discordInstalledFlag]

gtermSolarizedSetup :: Rules ()
gtermSolarizedSetup =
  withInstallDir "gterm-solarized" $ \dir -> do
    let gitUrl =
          "https://github.com/Anthony25/gnome-terminal-colors-solarized.git"
        installer = dir </> "install.sh"

    installer %> \out -> do
      Apt.install ["git"]
      command_ [Cwd dir] "git" ["clone", gitUrl, "./"]

    gtermFlag <-
      withStatusFlag "gterm-solarized.installed" $ do
        Apt.install ["dconf-cli"]
        need [installer]
        command_ [] installer ["-s","dark","-p","Default","--skip-dircolors"]

    want [gtermFlag]
    -- Add "eval `dircolors /path/to/dircolorsdb`" in your shell configuration
    --  file (.bashrc, .zshrc, etc...) to use new dircolors.
    -- For Fish, add the following to config.fish instead:
    --  eval (dircolors /path/to/dircolorsdb | head -n 1 |

fontSetup :: Rules ()
fontSetup = do
  fontDir <- queryEnv "FONTS_DIR"
  fontFiles <- liftIO $ getDirectoryFilesIO fontDir ["**.otf","**.ttf"]
  flags <-
    forM fontFiles $ \fontFile ->
      withStatusFlag ("fonts" </> fontFile <.> "installed") $ do
        Apt.install ["font-manager"]
        need [fontDir </> fontFile]
        home <- queryEnv "HOME"
        command_ [Cwd home] "sudo" ["rm", "-rf", ".cache/dconf"]
        command_ [Cwd fontDir] "sudo" ["font-manager", "-i", fontFile]
        command_ [] "fc-cache" ["-f", "-v"]
  want flags


haskellIdeEngineSetup :: Rules ()
haskellIdeEngineSetup =
  withInstallDir "haskell-ide-engine" $ \dir -> do

    hieVersion :: String <- queryEnv "HASKELL_IDE_ENGINE_VERSION"

    let flagName = "haskell-ide-engine-" ++ hieVersion <.> "installed"
        hieYaml = "stack-" ++ hieVersion <.> "yaml"

    (dir </> hieYaml) %> \out -> do
      Apt.install ["git"]
      command_
        []
        "git"
        [ "clone"
        , "https://github.com/haskell/haskell-ide-engine"
        , "--recursive"
        , dir
        ]

    hieFlag <-
      withStatusFlag flagName $ do
        Apt.install ["libicu-dev", "libtinfo-dev","ghc","build-essential"]
        need [dir </> hieYaml]
        command_ [] "stack" ["install","cabal-install"]
        command_ [Cwd dir] "cabal" ["update"]
        command_ [Cwd dir] "make" ["build-all"]
        command_ [Cwd dir] "stack" ["build", "--stack-yaml=" ++ hieYaml]
        command_ [Cwd dir] "stack" ["install"]

    want [hieFlag]
    -- install Haskell IDE engine as per
    --      https://github.com/haskell/haskell-ide-engine
    -- Spacemacs init
