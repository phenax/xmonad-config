module Theme where

import GHC.IO.Exception (ExitCode)
import System.Process
import qualified Data.Map as M
import Text.Parsec
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)

type Xres = M.Map String String
type XresValue = Xres -> String

emptyXres :: Xres
emptyXres = M.empty

background :: XresValue
background = fromMaybe "#000" . M.lookup "*.background"

accent :: XresValue
accent = fromMaybe "#0cd" . M.lookup "*.accent"

foreground :: XresValue
foreground = fromMaybe "#fff" . M.lookup "*.foreground"

danger :: XresValue
danger = fromMaybe "#800" . M.lookup "*.color1"


-- | Parsing

whitespace :: Parsec String u String
whitespace = many $ oneOf [' ', '\n', '\t']

xLineParser :: Parsec String u (String, String)
xLineParser = do
  key <- anyChar `manyTill` char ':'
  whitespace
  value <- anyChar `manyTill` endOfLine
  return (key, value)

xParser :: Parsec String u Xres
xParser = do
  whitespace
  ls <- many xLineParser
  whitespace
  return $ M.fromList ls

parseXres :: String -> Either ParseError Xres
parseXres = parse xParser "ParserError"

stdout :: (ExitCode, String, String) -> String
stdout (_, s, _) = s

loadXres :: IO Xres
loadXres = fromRight emptyXres . parseXres . stdout <$> readProcessWithExitCode "xrdb" ["-query"] ""

