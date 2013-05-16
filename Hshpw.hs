{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative
import Crypto.Hash.Whirlpool (hash)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as C
import Data.Maybe
import qualified Data.Map as Map
import Prelude hiding (dropWhile, take, filter)
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

data HashType = DefaultHash | DigitHash Int

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
    then putStrLn $ mkPwd DefaultHash $ pwd ++ k
    else IO.withFile mf IO.ReadMode $ \h -> do
      (ht,salt) <- fmap (Map.findWithDefault (DefaultHash,k) k) (readMapFile h)
      putStrLn $ mkPwd ht $ pwd ++ salt


readPwd True = fmap Just getLine
readPwd False = runInputT defaultSettings $ getPassword Nothing "Password:"

--mkPwd = unpack . take 10 . dropWhile (not . isDigit) . encode . hash . pack
mkPwd :: HashType -> String -> String
mkPwd ht = BC.unpack . BC.take n . f . encode . hash . BC.pack
  where
    (n, f) = case ht of
      DefaultHash -> (10, BC.dropWhile (not . C.isDigit))
      DigitHash n -> (n, BC.map $ \c -> C.intToDigit $ C.ord c `mod` 10)


readMapFile = fmap parseMapFile . IO.hGetContents
  where 
    parseMapFile = Map.fromList . either (const []) id . parse mappings ""
    mappings = many1 mapping <* eof
    mapping = do
      skipMany space
      k <- ident
      skipMany1 space
      ht <- option DefaultHash hashType
      s <- ident
      skipMany space
      return (k,(ht,s))
    ident = many1 (satisfy $ not . C.isSpace)
    hashType = do
      char '{'
      ht <- choice [digitHash,defaultHash]
      char '}'
      return ht
    defaultHash = do
      string "DEFAULT"
      return DefaultHash
    digitHash = do
      string "DIGIT:"
      intStr <- many1 (satisfy C.isDigit)
      return $ DigitHash (read intStr)

fileIsReadable path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return False
    else Dir.readable <$> Dir.getPermissions path
