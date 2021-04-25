{-# LANGUAGE LambdaCase #-}
module Scratchpads where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import Utils

data Scratchpad = SystemMonitor | Notes | None

instance Show Scratchpad where
  show SystemMonitor = "sysmon"
  show Notes = "notes"
  show None = ""

data LayoutType = Small | Medium | Large

getLayout = \case
  Small -> customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
  Medium -> customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)
  Large -> customFloating $ W.RationalRect (1/10) (1/10) (4/5) (4/5)


newNS (s, cmd, layout) = NS n cmd (className =? n) $ getLayout layout
  where n = show s

newTerminalNS (s, cmd, layout) = newNS (s, inTerm (show s) cmd, layout)

scratchpads = map newTerminalNS
  [ (SystemMonitor, "gotop", Large)
  , (Notes, "sensible-editor ~/dump/tmp-notes", Medium)
  ]

scratchpad :: Scratchpad -> X ()
scratchpad = namedScratchpadAction scratchpads . show

