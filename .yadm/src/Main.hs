module Main where
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".build"} $ do 
  usingConfigFile "~/.yadm/setup.conf" 
  return ()
