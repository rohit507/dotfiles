module Stack where

import Shake

plugin :: Plugin
plugin = Plugin return $ return ()

update :: Action ()
update = do
  home <- queryEnv "HOME"
  command_ [Cwd home, Traced "stack update"] "stack"
    ["update"]

upgrade :: Action ()
upgrade = do
  home <- queryEnv "HOME"
  command_ [Cwd home, Traced "stack upgrade"] "stack"
    ["upgrade"]

install :: String -> Action ()
install app = do
  update
  upgrade
  home <- queryEnv "HOME"
  command_ [Cwd home, Traced "stack install"] "stack"
    ["install", app]
