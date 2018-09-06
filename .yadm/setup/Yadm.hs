module Yadm (
      plugin
    , add
    , status
    , push
    , fetch
    , alt
    , gitconfig
    , encrypt
    , decrypt
    , addEncrypt
    , initSubmodules
    ) where

import Shake
import Data.List

newtype YadmLock = YadmLock Resource

initYadmLock :: ShakeOptions -> IO ShakeOptions
initYadmLock shakeOpts = do
    resource  <- newResourceIO "yadm-lock" 1
    return $ shakeOpts{shakeExtra =
        addShakeExtra (YadmLock resource) (shakeExtra shakeOpts)
        }


withYadmLock :: Action a -> Action a
withYadmLock act = do
    getShakeExtra >>= \case
        Nothing -> fail "Yadm.plugin might not have been initialized."
        Just (YadmLock r) -> withResource r 1 $ act

plugin :: Plugin
plugin = Plugin initYadmLock $ action $ decrypt >> alt

add :: FilePath -> Action ()
add file = withYadmLock $ command_ [Traced "yadm add"] "yadm" $
    ["add", file]

status :: Action ()
status = withYadmLock $ command_ [Traced "yadm status"] "yadm" $
    ["status"]

push :: Action ()
push = withYadmLock $ command_ [Traced "yadm push"] "yadm" $
    ["push","origin","master"]

fetch :: Action ()
fetch = withYadmLock $ command_ [Traced "yadm fetch"] "yadm" $
    ["fetch","origin","master"]

alt :: Action ()
alt = withYadmLock $ command_ [Traced "yadm alt"] "yadm" $
    ["alt"]

addEncrypt :: FilePattern -> Action ()
addEncrypt path = do
    path':eFile:_ <- mapM applyEnv [path,"${YADM_ROOT}/encrypt"]
    paths <- readFileLines eFile
    case elem path' paths of
        True -> return ()
        False -> writeFileLines eFile $ paths ++ [path']

initSubmodules :: Action ()
initSubmodules = withYadmLock $ command_ [Traced "yadm submodule update"] "yadm"
  ["submodule", "update", "--recursive", "--init"]

encrypt :: Action ()
encrypt = withYadmLock $ command_ [Traced "yadm encrypt"] "yadm" $
    ["encrypt"]

gitconfig :: String -> String -> Action ()
gitconfig key val = withYadmLock $ command_ [Traced "yadm encrypt"] "yadm" $
    ["gitconfig", key, val]

decrypt :: Action ()
decrypt = withYadmLock $ command_ [Traced "yadm decrypt"] "yadm" $
    ["decrypt"]
