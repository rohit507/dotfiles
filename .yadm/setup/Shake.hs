module Shake (
      module E
    ) where

import Development.Shake as E hiding (
      need
    , want
    , (%>)
    , (|%>)
    , (?>) 
    , (&%>)
    , (&?>)
    , orderOnly
    , doesFileExist
    , doesDirectoryExist
    , getDirectoryContents
    , getDirectoryFiles 
    , getDirectoryDirs 
    )

import Development.Shake.Command as E
import Development.Shake.FilePath as E
import Development.Shake.Util as E
import Development.Shake.Config as E
import Development.Shake.Classes as E

import Plugin as E
import Config as E hiding (plugin)


