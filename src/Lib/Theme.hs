module Lib.Theme where

import Data.Either (fromRight)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.IO.Exception (ExitCode)
import qualified Lib.Config as C
import System.Process (readProcessWithExitCode)
import Text.Parsec
import XMonad.Layout.Tabbed

type Xres = M.Map String String

type XresValue = Xres -> String

emptyXres :: Xres
emptyXres = M.empty

getXVal :: String -> String -> Xres -> String
getXVal k def = fromMaybe def . M.lookup k

background :: XresValue
background = getXVal "*.background" "#000"

accent :: XresValue
accent = getXVal "*.accent" "#0cd"

foreground :: XresValue
foreground = getXVal "*.foreground" "#fff"

faded :: XresValue
faded = getXVal "*.color8" "#888"

danger :: XresValue
danger = getXVal "*.color1" "#800"

getTabTheme xres =
  let acc = accent xres
      fg = foreground xres
      bg = background xres
   in def
        { fontName = C.font,
          activeColor = acc,
          inactiveColor = bg,
          activeBorderColor = acc,
          inactiveBorderColor = bg,
          activeTextColor = bg,
          inactiveTextColor = fg
        }

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
