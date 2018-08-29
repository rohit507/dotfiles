module Main where

import Shake 

main :: IO ()
main = shakeWithConfig "setup.conf" shakeOpts $ do 
    action $ printConfigVars 

shakeOpts :: ShakeOptions 
shakeOpts = shakeOptions{
      shakeFiles=".build"
    , shakeProgress=progressSimple
    }
