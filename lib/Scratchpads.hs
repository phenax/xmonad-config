module Scratchpads where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad

import Utils

data Scratchpad = SystemMonitor | None

instance Show Scratchpad where
  show SystemMonitor = "sysmon"
  show None = ""

newNS s cmd = NS n cmd (className =? n)
  where n = show s

newTerminalScratchpad s cmd = newNS s $ inTerm (show s) cmd

scratchpads =
  [ newTerminalScratchpad SystemMonitor "gotop"
      $ customFloating (W.RationalRect (1/6) (1/6) (2/3) (2/3))
  ]

scratchpad :: Scratchpad -> X ()
scratchpad = namedScratchpadAction scratchpads . show

