module Status where

import Shake
import Control.Applicative

type StatusFlag = String

plugin :: Plugin
plugin = Plugin pre post
  where
    pre = return
    post = return ()

withStatusFlag :: FilePath -> Action () -> Rules FilePath
withStatusFlag file act = let fn = "${STATUS_DIR}" </> file in
  (want [fn] <* (fn %> \file -> act >> cmd_ "touch" file)) >> return fn
