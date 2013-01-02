{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative
import Crypto.Hash.Whirlpool (hash)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (take, pack, unpack, dropWhile)
import Data.Char (isDigit, isSpace)
import Data.Maybe
import qualified Data.Map as Map
import Prelude hiding (dropWhile, take)
import System.Console.CmdArgs
import System.Console.Haskeline (runInputT, defaultSettings, getPassword)
import qualified System.Directory as Dir
import qualified System.IO as IO
import Text.ParserCombinators.Parsec
import Debug.Trace


data Hshpw = Hshpw {
  key :: String,
  mapFile :: FilePath,
  listKeys :: Bool,
  stdin :: Bool
} deriving (Show, Data, Typeable)

hpwdOpts = Hshpw {
  key = "",
  mapFile = "",
  listKeys = False,
  stdin = False
}


main = do
  opts @ Hshpw { listKeys = list } <- cmdArgs hpwdOpts
  if list then doListKeys opts else doPrintPwd opts

doListKeys opts = let mf = mapFile opts in do
  mfReadable <- fileIsReadable mf
  if not mfReadable
    then putStrLn $ "Unable to read map file " ++ mf
    else IO.withFile mf IO.ReadMode $ \h ->
      fmap Map.keys (readMapFile h) >>= mapM_ putStrLn

doPrintPwd opts = do
  let Hshpw { key = k, mapFile = mf, stdin = stdin } = opts
  mfReadable <- fileIsReadable mf
  pwd <- fromMaybe "" <$> readPwd stdin
  if not mfReadable
    then putStrLn $ mkPwd $ pwd ++ k
    else IO.withFile mf IO.ReadMode $ \h -> do
      salt <- fmap (Map.findWithDefault k k) (readMapFile h)
      putStrLn $ mkPwd $ pwd ++ salt


readPwd True = fmap Just getLine
readPwd False = runInputT defaultSettings $ getPassword Nothing "Password:"

mkPwd = unpack . take 10 . dropWhile (not . isDigit) . encode . hash . pack


readMapFile = fmap parseMapFile . IO.hGetContents
  where 
    parseMapFile = Map.fromList . either (const []) id . parse mappings ""
    mappings = many1 mapping <* eof
    mapping = do
      skipMany space
      k <- ident
      skipMany1 space
      s <- ident
      skipMany space
      return (k,s)
    ident = many1 (satisfy $ not . isSpace)


fileIsReadable path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return False
    else Dir.readable <$> Dir.getPermissions path
