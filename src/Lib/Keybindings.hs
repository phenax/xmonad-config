module Lib.Keybindings where

import qualified Lib.Config as C
import qualified Lib.Scratchpads as NS
import Lib.Utils
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (toggleWS')
import XMonad.Actions.WithAll (killAll)
import XMonad.Layout.LayoutCombinators (JumpToLayout (..))
import XMonad.Layout.ResizableTile (MirrorResize (..))
import XMonad.Layout.SubLayouts (GroupMsg (..), pullGroup)
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.Layout.WindowNavigation (Direction2D (..))
import XMonad.StackSet (sink)

layoutPrefix = mod' "l"

replPrefix = mod' "r"

-- TODO: Add monitor key bindings
keybindings =
  [ (modShift "r", restart "/home/imsohexy/.xmonad/bin/xmonad-wm" True),
    --
    -- Window management
    (modAlt "h", sendMessage Shrink),
    (modAlt "l", sendMessage Expand),
    (modAlt "j", sendMessage MirrorShrink),
    (modAlt "k", sendMessage MirrorExpand),
    (modShift "q", kill1),
    (modCtrl "<Delete>", killAll),
    --
    -- Workspaces
    (mod' "<Tab>", toggleWS' ["NSP"]),
    --
    -- Layout
    (mod' "f", sendMessage $ Toggle "monocle"),
    (mod' "=", sendMessage $ IncMasterN 1),
    (mod' "-", sendMessage $ IncMasterN (-1)),
    (layoutPrefix +> "t", sendMessage $ JumpToLayout "tall"),
    (layoutPrefix +> "w", sendMessage $ JumpToLayout "wide"), -- TODO: Fix set layout
    (layoutPrefix +> shift "t", sendMessage $ JumpToLayout "tallAccordion"),
    (layoutPrefix +> shift "w", sendMessage $ JumpToLayout "wideAccordion"),
    -- (layoutPrefix +> "f", sendMessage $ JumpToLayout "floating"),
    (layoutPrefix +> shift "f", withFocused $ windows . sink),
    --
    -- Sublayout
    (modCtrl "\\", withFocused $ sendMessage . UnMergeAll),
    (modCtrl "h", sendMessage $ pullGroup L),
    (modCtrl "l", sendMessage $ pullGroup R),
    (modCtrl "k", sendMessage $ pullGroup U),
    (modCtrl "j", sendMessage $ pullGroup D),
    --
    -- Scratchpads
    (mod' "o", NS.scratchpad NS.SystemMonitor),
    (mod' "n", NS.scratchpad NS.Notes),
    (replPrefix +> "n", NS.scratchpad $ NS.Repl NS.Js),
    (replPrefix +> "h", NS.scratchpad $ NS.Repl NS.Haskell),
    --
    -- Temporary
    (modCtrl "q", io exitSuccess),
    ("M-C-p", sendMessage NextLayout),
    ("M-S-<Return>", spawn C.terminal) -- temporary
  ]

mousebindings =
  [ ((C.modKey .|. mod1Mask, button1), \w -> focus w >> mouseResizeWindow w)
  ]
