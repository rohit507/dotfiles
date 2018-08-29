module Shake (
      module E
    , shakeWithConfig
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
    , replaceEnvRule 
    , replaceEnvAction
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
import qualified Development.Shake as S (
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

import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)

import Data.Char

import System.Environment

getVars :: [String] -> IO [(String,String)]
getVars envVars = mapM getVar envVars

getVar :: String -> IO (String,String)
getVar varName = lookupEnv varName >>= \case
    Just varVal -> return (varName,varVal)
    Nothing -> fail $ "Did not find devault env var '$"++varName++"'."

readConfigFileWithEnvVars :: String -> FilePath -> IO (HashMap String String)
readConfigFileWithEnvVars varListName configFile = do
    maybeVarNames <- HashMap.lookup varListName <$> readConfigFile configFile
    varNames <- case maybeVarNames of
        Nothing -> fail $ "config file '"++configFile++"' must have variable '"++varListName
                              ++"' in as a literal of type `[String]`."
        Just a -> return a
    envVars  <- getVars (read @[String] varNames)
    readConfigFileWithEnv envVars configFile

newtype EnvVars = EnvVars (HashMap String String)

printConfigVars :: Action () 
printConfigVars = do 
    keys <- getConfigKeys 
    (zip keys <$> mapM getConfig keys) >>= liftIO . print 

shakeWithConfig :: FilePath -> ShakeOptions -> Rules () -> IO () 
shakeWithConfig configFile shakeOpts rules = do
    config <- readConfigFileWithEnvVars "IMPORTED_ENV_VARS" configFile
    let shakeExtra' = addShakeExtra (EnvVars config) (shakeExtra shakeOpts)
        shakeOpts'  = shakeOpts{shakeExtra = shakeExtra'} 
    shakeArgs shakeOpts' (usingConfig config >> rules)

getEnvValRule :: Rules EnvVars
getEnvValRule = do
    me :: Maybe EnvVars <- getShakeExtraRules 
    case me of 
        Nothing -> fail $ "No Environment Variables Loaded into shakeOpts." 
        Just e  -> return e 

getEnvValAction :: Action EnvVars
getEnvValAction = do
    me :: Maybe EnvVars <- getShakeExtra
    case me of 
        Nothing -> fail $ "No Environment Variables Loaded into shakeOpts." 
        Just e  -> return e 

replaceEnvRule :: [FilePattern] -> Rules ([FilePattern], Action ())
replaceEnvRule pats = do 
    e <- getEnvValRule
    let exprs = map lexxExpr pats 
    return (map (askExpr e) exprs, mapM_ getConfig $ concatMap (getExprVars e) exprs) 

replaceEnvAction :: [FilePattern] -> Action [FilePattern]
replaceEnvAction pats = do 
    e <- getEnvValAction
    let exprs = map lexxExpr pats 
    mapM_ getConfig $ concatMap (getExprVars e) exprs
    return $ map (askExpr e) exprs


-- Stuff fromhttp://hackage.haskell.org/package/shake-0.16.4/docs/src/
data Expr = Exprs [Expr] | Lit String | Var String deriving (Show,Eq)

askExpr :: EnvVars -> Expr -> String
askExpr ev@(EnvVars e) = f
    where f (Exprs xs) = concat $ map f xs
          f (Lit x) = x
          f (Var x) = case HashMap.lookup x e of 
             Nothing -> error $ "could not find '"++x++"' in environment"
             Just v  -> askExpr ev $ lexxExpr v 

getExprVars :: EnvVars -> Expr -> [String]
getExprVars e (Exprs l) = concatMap (getExprVars e) l 
getExprVars _ (Lit _) = []
getExprVars e@(EnvVars m) (Var v) = case HashMap.lookup v m of 
    Nothing -> [v] 
    Just l  -> v : getExprVars e (lexxExpr l) 

lexxExpr :: String -> Expr -- snd will start with one of " :\n\r" or be empty
lexxExpr = exprs . fst .  f
    where
        exprs [x] = x
        exprs xs = Exprs xs

        special = \x -> x <= ':' && (x == ':' || x == ' ' || x == '$' || x == '\r' || x == '\n' || x == '\0')
        
        f x = case break special x of (a,x) -> if a == "" then g x else Lit a $: g x

        x $: (xs,y) = (x:xs,y)

        dropSpace = dropWhile (== ' ')

        dropN ('\n':xs) = xs
        dropN x         = x

        isVar x = x == '-' || x == '_' || isLower x || isUpper x || isDigit x

        isVarDot x = x == '.' || isVar x

        g x | head x /= '$' = ([], x)
        g x | c_x <- tail x, (c:x) <- c_x = case c of
            '$' -> Lit "$" $: f x
            ' ' -> Lit " " $: f x
            ':' -> Lit ":" $: f x
            '\n' -> f $ dropSpace x
            '\r' -> f $ dropSpace $ dropN x
            '{' | (name,x) <- span isVarDot x, not $ name == "", ('}':xs) <- x -> Var name $: f xs
            _   | (name,x) <- span isVar  c_x, not $ name == "" -> Var name $: f x
            _   -> error "Unexpect $ followed by unexpected stuff"

need :: [FilePath] -> Action () 
need pats =  replaceEnvAction pats >>= S.need

want :: [FilePath] -> Rules () 
want pats = fst <$> replaceEnvRule pats >>= S.want

(%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(%>) pat act = do 
    (pat':[], added) <- replaceEnvRule [pat]
    (S.%>) pat' (\ f -> added >> act f)

(|%>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(|%>) pats act = do 
    (pats', added) <- replaceEnvRule pats
    (S.|%>) pats' (\ f -> added >> act f)

(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) pred act = do 
    env <- getEnvValRule
    let pred' = pred . askExpr env . lexxExpr 
    (S.?>) pred' act

(&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
(&%>) pats act = do 
    (pats', added) <- replaceEnvRule pats
    (S.&%>) pats' (\ f -> added >> act f)


orderOnly :: [FilePath] -> Action ()
orderOnly pats =replaceEnvAction pats >>= S.orderOnly

doesFileExist :: FilePath -> Action Bool
doesFileExist pat = head <$> replaceEnvAction [pat] >>= S.doesFileExist

doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist pat = head <$> replaceEnvAction [pat] >>= S.doesDirectoryExist

getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents pat = head <$> replaceEnvAction [pat] >>= S.getDirectoryContents

getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles pat pats = do
    pat' <- head <$> replaceEnvAction [pat] 
    pats' <- replaceEnvAction pats
    S.getDirectoryFiles pat' pats'

getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs pat =  head <$> replaceEnvAction [pat] >>= S.getDirectoryDirs
