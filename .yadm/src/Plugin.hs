module Plugin where

import Development.Shake
import Development.Shake.Command 
import Development.Shake.FilePath
import Development.Shake.Util
import Development.Shake.Config
import Development.Shake.Classes

import Control.Monad 

data Plugin = Plugin {
      modOpts :: ShakeOptions -> IO ShakeOptions 
    , modRules :: Rules () 
    }

shakeWithPlugins :: [Plugin] -> ShakeOptions -> Rules () -> IO () 
shakeWithPlugins plugins shakeOpts rules = do
    -- Modify the options as needed 
    shakeOpts' <- foldM (flip modOpts) shakeOpts plugins 
    -- run rules with prepended changes
    shakeArgs shakeOpts' (mapM modRules plugins >> rules)
