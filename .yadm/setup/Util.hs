module Util where

import Shake 

-- | Check whether an executable is installed and if so, return the path. 
which :: String -> Action FilePath 
which name = do
    Stdout path <- command [Traced ""] "which" [name] 
    case path of 
        "" -> fail $ "Command '"++name++"' not found in environment." 
        a  -> return name 

