module Apt (plugin, install, autoremove, update, upgrade) where

import Shake
import Util (which)
import Data.List
import Data.ByteString (ByteString)
import Development.Shake.Rule
import System.Exit
import Data.Binary

-- On init we should create a new resource that only allows for a single dpkg process at a time
-- We also should create a new oracle for Apt.install and Exists ...
-- Installs are tracked with dpkg, so we don't need to use status flags.
-- install
-- upgrade

type PkgName = String

-- | flag type we use to insert and recover the dpkg resource
newtype DpkgLock = DpkgLock Resource

initDpkgLock :: ShakeOptions -> IO ShakeOptions
initDpkgLock shakeOpts = do
    resource  <- newResourceIO "dpkg-lock" 1
    return $ shakeOpts{shakeExtra =
        addShakeExtra (DpkgLock resource) (shakeExtra shakeOpts)
        }

-- | given a list of packages, splits them into two sets
--   fst is ones that are installed, snd is those that arent.
splitInstalled :: [AptPkg] -> Action ([AptPkg],[AptPkg])
splitInstalled pkgs = do
    let pkgs' = nub pkgs
    (Exit _, Stdout list) <- command [Traced "", Shell] "dpkg-query" $
        ["-W","-f='${Package}~${Status}\\n'","2>/dev/null"] ++ map getPkg pkgs'
    let statuses = map (fmap (dropWhile ('~' ==)) .  break ('~' ==)) . lines $ list
        installed =  map (AptPkg . fst) $
            filter (\ (_,status) -> "install ok" `isPrefixOf` status) statuses
        notInstalled = pkgs \\ installed
    return (installed, notInstalled)

-- | userRule for whether a package is installed
newtype AptPkg = AptPkg String
    deriving (Eq, Show, Typeable, Hashable, Binary, NFData)

getPkg :: AptPkg -> String
getPkg (AptPkg s) = s

type instance RuleResult AptPkg = ()

aptInstallRule :: Rules ()
aptInstallRule = batch 20 addRuleForm return batchAptInstall

addRuleForm :: (AptPkg -> Action ()) -> Rules ()
addRuleForm evalAction = addBuiltinRule noLint $ builtInRun evalAction
    where
        builtInRun :: (AptPkg -> Action ())
                   -> AptPkg
                   -> Maybe ByteString
                   -> Bool
                   -> Action (RunResult ())
        builtInRun action pkg _ _ = action pkg >> (return $
            RunResult ChangedRecomputeSame mempty ())

withDpkgLock :: Action a -> Action a
withDpkgLock act = do
    getShakeExtra >>= \case
        Nothing -> fail "Apt.plugin might not have been initialized."
        Just (DpkgLock r) -> withResource r 1 $ act

batchAptInstall :: [AptPkg] -> Action ()
batchAptInstall pkgs = do
  (_, neededPkgs) <- splitInstalled pkgs
  case neededPkgs of
    [] -> return ()
    np -> do
      update
      upgrade
      withDpkgLock $
        command_ [Traced "apt install"] "sudo" $
        ["apt", "install", "-qq", "-y"] ++ map getPkg np

install :: [PkgName] -> Action ()
install = (>> return ()) . apply . map AptPkg

plugin :: Plugin
plugin = Plugin pre post
    where
        pre = initDpkgLock

        post = aptInstallRule


autoremove :: Action ()
autoremove = withDpkgLock $ command_ [Traced "apt autoremove"]
    "sudo" ["apt","autoremove","-qq","-y"]

update :: Action ()
update = withDpkgLock $ command_ [Traced "apt update"]
    "sudo" ["apt","update","-qq","-y"]

upgrade :: Action ()
upgrade = withDpkgLock $ command_ [Traced "apt upgrade"]
    "sudo" ["apt","upgrade","-qq","-y"]
