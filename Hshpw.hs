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
  printUser :: Bool,
  stdin :: Bool
} deriving (Show, Data, Typeable)

data HashType = DefaultHash | DigitHash Int | AlphaNumHash Int
  deriving (Show)

hpwdOpts = Hshpw {
  key = "",
  mapFile = "",
  listKeys = False,
  printUser = False,
  stdin = False
}


main = do
  opts @ Hshpw { listKeys = list, printUser = user } <- cmdArgs hpwdOpts
  if list
    then doListKeys opts
    else if user
      then doPrintUser opts
      else doPrintPwd opts

doListKeys opts = let mf = mapFile opts in do
  mfReadable <- fileIsReadable mf
  if not mfReadable
    then putStrLn $ "Unable to read map file " ++ mf
    else IO.withFile mf IO.ReadMode $ \h ->
      fmap Map.keys (readMapFile h) >>= mapM_ putStrLn

getInfo opts = do
  let Hshpw { key = k, mapFile = mf, stdin = stdin } = opts
  mfReadable <- fileIsReadable mf
  if not mfReadable
    then return (DefaultHash, k, Nothing)
    else IO.withFile mf IO.ReadMode $ \h -> do
      m <- readMapFile h
      return $! Map.findWithDefault (DefaultHash,k,Nothing) k m

doPrintPwd opts = do
  let Hshpw { stdin = stdin } = opts
  pwd <- fromMaybe "" <$> readPwd stdin
  (ht,salt,_) <- getInfo opts
  putStrLn $ mkPwd ht $ pwd ++ salt

doPrintUser opts = do
  (_,_,u) <- getInfo opts
  putStrLn $ fromMaybe "" u

readPwd True = fmap Just getLine
readPwd False = runInputT defaultSettings $ getPassword Nothing "Password:"

mkPwd :: HashType -> String -> String
mkPwd ht = BC.unpack . BC.take n . f . encode . hash . BC.pack
  where
    (n, f) = case ht of
      DefaultHash -> (10, BC.dropWhile (not . C.isDigit))
      AlphaNumHash n -> (n, BC.filter C.isAlphaNum)
      DigitHash n -> (n, BC.map $ \c -> C.intToDigit $ C.ord c `mod` 10)


readMapFile = fmap parseMapFile . IO.hGetContents
  where
    parseMapFile = Map.fromList . either (const []) id . parse mappings ""
    mappings = sepEndBy1 mapping newline <* eof
    mapping = do
      spaces'
      k <- ident
      spaces'
      ht <- option DefaultHash hashType
      s <- ident
      spaces'
      u <- optionMaybe ident
      spaces'
      return (k,(ht,s,u))
    ident = many1 (satisfy $ not . C.isSpace)
    hashType = do
      char '{'
      ht <- choice [alphaNumHash,digitHash,defaultHash]
      char '}'
      return ht
    defaultHash = do
      string "DEFAULT"
      return DefaultHash
    alphaNumHash = do
      string "ALPHANUM:"
      intStr <- many1 (satisfy C.isDigit)
      return $ AlphaNumHash (read intStr)
    digitHash = do
      string "DIGIT:"
      intStr <- many1 (satisfy C.isDigit)
      return $ DigitHash (read intStr)
    spaces' = skipMany $ oneOf " \t"

fileIsReadable path = do
  exists <- Dir.doesFileExist path
  if not exists
    then return False
    else Dir.readable <$> Dir.getPermissions path
