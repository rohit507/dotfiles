{-# LANGUAGE OverloadedStrings #-}

module Config (
      plugin
    , EnvVar
    , EnvVars
    , MonadEnv
    , queryEnv
    , applyEnv
    , printConfigVar
    , printConfigVars
    , need
    , want
    , (%>)
    , (|%>)
    , (?>)
    , (&%>)
    -- , (&?>)
    , orderOnly
    , doesFileExist
    , doesDirectoryExist
    , getDirectoryContents
    , getDirectoryFiles
    , getDirectoryDirs
    ) where


import Development.Shake hiding (
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
    , getEnv
    )

import qualified Development.Shake as S

import Data.Text (Text)
import qualified Data.Text as T

import Data.Yaml

import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Development.Shake.Config
import Development.Shake.Classes

import Control.Monad

import Debug.Trace
import GHC.Stack

import Plugin

import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)

import Data.Char
import Data.String

import qualified System.Environment as SE

type EnvVar = Text

newtype EnvVars = EnvVars {unEnvVars :: Object} deriving (Show)

-- Stuff modifed from http://hackage.haskell.org/package/shake-0.16.4/docs/src/
data Expr = Exprs [Expr] | Lit Text | Var Text deriving (Show,Eq)

class (Monad m) => MonadEnv m where
  getEnv    :: m EnvVars
  queryEnv  :: FromJSON a => String -> m a
  queryEnv var = getEnv >>= (\ env -> return $ queryEnv var env)
  applyEnv  :: String -> m String
  applyEnv str = getEnv >>= (\ env -> return $ applyEnv str env)
  {-# MINIMAL getEnv #-}

instance MonadEnv ((->) EnvVars) where
  getEnv = id
  queryEnv = lookupEnvVar
  applyEnv = insertEnvVar

instance MonadEnv Action where
  getEnv = do
    me :: Maybe EnvVars <- getShakeExtra
    case me of
        Nothing -> fail $ "No Environment Variables Loaded into shakeOpts."
        Just e  -> return e

instance MonadEnv Rules where
  getEnv = do
    me :: Maybe EnvVars <- getShakeExtraRules
    case me of
        Nothing -> fail $ "No Environment Variables Loaded into shakeOpts."
        Just e  -> return e

readConfigWithEnvVars :: EnvVar -> FilePath -> IO EnvVars
readConfigWithEnvVars imports configFile = do
  hm :: Object <- decodeFileThrow configFile
  mV <-
    case HashMap.lookup imports hm of
      Nothing ->
        fail $ "No variable '" ++ T.unpack imports ++ "' with list of env vars"
      Just a -> return a
  -- retrieve env vars from file
  vars :: [EnvVar] <- parseMonad parseJSON mV
  -- get those vars from the environment
  sysEnv :: [(EnvVar, Value)] <-
    zip vars <$> mapM (fmap (String . T.pack) . SE.getEnv . T.unpack) vars
  let unioned :: Object = HashMap.union (HashMap.fromList sysEnv) hm
      fixed :: Object = fixValInEnv fixed <$> unioned
  return $ EnvVars fixed

plugin :: FilePath -> Plugin
plugin configFile = Plugin pre post
    where
        pre shakeOpts = do
            config <- readConfigWithEnvVars "IMPORTED_ENV_VARS" configFile
            let shakeExtra' = addShakeExtra config (shakeExtra shakeOpts)
            return $ shakeOpts{shakeExtra = shakeExtra'}

        upt (a, b) = (T.unpack a, T.unpack . renderValue $ b)

        transformMap = HashMap.fromList . map upt . HashMap.toList . unEnvVars

        post = getEnv >>= usingConfig . transformMap >> return ()

printConfigVar :: String -> Action ()
printConfigVar ev = queryEnv ev >>= liftIO . print . ((ev ++ " : ") ++) . show @Value

printConfigVars :: Action ()
printConfigVars =
  getEnv >>= mapM_ (printConfigVar . T.unpack) . HashMap.keys . unEnvVars

fixValInEnv :: Object -> Value -> Value
fixValInEnv hm (Object o) = Object $ fixValInEnv hm <$> o
fixValInEnv hm (Array  a) = Array  $ fixValInEnv hm <$> a
fixValInEnv hm (String t) = String . renderExpr hm . parseExpr $ t
fixValInEnv _  v          = v

lookupEnvVar :: FromJSON a => String -> EnvVars -> a
lookupEnvVar v (EnvVars hm) =
  case mVal of
    Nothing -> error $ "No variable '" ++ v ++ "' found in env."
    Just a -> a
  where
    mVal = do
      val <- HashMap.lookup (T.pack v) hm
      parseMaybe parseJSON val

insertEnvVar :: String -> EnvVars -> String
insertEnvVar s (EnvVars hm) = T.unpack . renderExpr hm . parseExpr . T.pack $ s

tt :: Show a => String -> a -> a
tt _ = id -- traceShowId . traceStack (p ++ " :")

parseExpr :: HasCallStack => Text -> Expr
parseExpr =  exprs . fst . f . tt "pe"
  where
    exprs [x] = x
    exprs xs = Exprs xs

    special =
      \x ->
        x <= ':' &&
        (x == ':' || x == ' ' || x == '$' || x == '\r' || x == '\n' || x == '\0')

    f :: HasCallStack => Text -> ([Expr], Text)
    f x =
      case T.break special x of
        (a, x) ->
          if a == ""
            then g (tt "f1" x)
            else Lit a $: g (tt "f2" x)

    x $: (xs, y) = (x:xs, y)

    dropSpace = T.dropWhile (== ' ')

    dropN x = snd . T.break (== '\n') $ x

    isVar x = x == '-' || x == '_' || isLower x || isUpper x || isDigit x

    isVarDot x = x == '.' || isVar x


    g :: HasCallStack => Text -> ([Expr],Text)
    g x
      | T.null x = ([], x)
    g x
      | T.head (tt "g1" x) /= '$' = ([], x)
    g x
      | c_x <- T.tail (tt "g2" x)
      , Just (c, x) <- T.uncons c_x =
        case c of
          '$' -> Lit "$" $: f (tt "g3" x)
          ' ' -> Lit " " $: f (tt "g4" x)
          ':' -> Lit ":" $: f (tt "g5" x)
          '\n' -> f $ dropSpace x
          '\r' -> f $ dropSpace $ dropN (tt "g6" x)
          '{'
            | (name, x) <- T.span isVarDot (tt "g7" x)
            , not $ name == ""
            , Just ('}', xs) <- T.uncons (tt "g8" x) -> Var name $: f (tt "g9" xs)
          _
            | (name, x) <- T.span isVar c_x
            , not $ name == "" -> Var name $: f (tt "g9" x)
          _ -> error "Unexpect $ followed by unexpected stuff"

renderExpr :: Object -> Expr -> Text
renderExpr hm (Exprs xs) = mconcat $ map (renderExpr hm) xs
renderExpr hm (Lit x) = x
renderExpr hm (Var x) =
  case HashMap.lookup x hm of
    Nothing -> error $ "could not find '" ++ T.unpack x ++ "' in environment"
    Just v  -> renderExpr hm . parseExpr . renderValue $ v

renderValue :: Value -> Text
renderValue (String s) = s
renderValue (Bool   b) = T.pack $ show b
renderValue (Number s) = T.pack $ show s
renderValue (Object _) = error "Cannot render Dictionary in env"
renderValue (Array  _) = error "Cannot render Array in env"
renderValue Null       = error "Cannot render Null in env"


need :: [FilePath] -> Action ()
need pats = mapM applyEnv pats >>= S.need

want :: [FilePath] -> Rules ()
want pats = mapM applyEnv pats >>= S.want

(%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(%>) pat act = do
    pat' <- applyEnv pat
    (S.%>) pat' act

(|%>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(|%>) pats act = do
    pats' <- mapM applyEnv pats
    (S.|%>) pats' act

(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) pred act = do
    (EnvVars env) <- getEnv
    let pred' = pred . T.unpack . renderExpr env . parseExpr . T.pack
    (S.?>) pred' act

(&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
(&%>) pats act = do
    pats' <- mapM applyEnv pats
    (S.&%>) pats' act


orderOnly :: [FilePath] -> Action ()
orderOnly pats = mapM applyEnv pats >>= S.orderOnly

doesFileExist :: FilePath -> Action Bool
doesFileExist pat = applyEnv pat >>= S.doesFileExist

doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist pat = applyEnv pat >>= S.doesDirectoryExist

getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents pat = applyEnv pat >>= S.getDirectoryContents

getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles pat pats = do
    pat' <- applyEnv pat
    pats' <- mapM applyEnv pats
    S.getDirectoryFiles pat' pats'

getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs pat =  applyEnv pat >>= S.getDirectoryDirs
