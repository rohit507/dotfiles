module Stack where 

import Shake 

plugin :: Plugin 
plugin = Plugin return $ return ()

update :: Action () 
update = command_ [Traced "stack update"] "stack" $ 
    ["update"]

upgrade :: Action () 
upgrade = command_ [Traced "stack upgrade"] "stack" $ 
    ["upgrade"]

install :: String -> Action () 
install app = command_ [Traced "stack install"] "stack" $ 
    ["install", app]
