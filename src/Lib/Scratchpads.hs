{-# LANGUAGE LambdaCase #-}

module Lib.Scratchpads where

import Lib.Utils
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

data ReplLang
  = Js
  | Haskell
  | RateSx
  deriving (Show)

data Scratchpad
  = SystemMonitor
  | Notes
  | Terminal
  | Repl ReplLang
  | Calendar
  | None
  deriving (Show)

data LayoutType
  = Small
  | Medium
  | Large

getLayout = \case
  Small -> customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)
  Medium -> customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
  Large -> customFloating $ W.RationalRect (1 / 10) (1 / 10) (4 / 5) (4 / 5)

newNS (s, cmd, layout) = NS cls cmd (className =? cls) $ getLayout layout
  where
    cls = show s

newTerminalNS (s, cmd, layout, cwd) = newNS (s, inTerm (show s) cwd cmd, layout)

-- | List of scratchpads
scratchpads =
  map
    newTerminalNS
    [ (SystemMonitor, "gotop", Large, "~"),
      (Notes, inEditor "~/nixos/extras/notes/index.md", Large, "~/nixos/extras/notes"),
      (Terminal, "zsh", Medium, "~"),
      (Calendar, "wyrd", Medium, "~"),
      (Repl Js, "node", Small, "~"),
      (Repl RateSx, "~/scripts/rate-sx.sh", Medium, "~"),
      (Repl Haskell, "ghci", Small, "~")
    ]

scratchpad :: Scratchpad -> X ()
scratchpad = namedScratchpadAction scratchpads . show
