module Lib.Keybindings where

import qualified Lib.Config as C
import qualified Lib.Scratchpads as NS
import Lib.Utils
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (toggleWS')
import XMonad.Actions.WithAll (killAll)
import XMonad.Layout.ResizableTile (MirrorResize (..))
import XMonad.Layout.SubLayouts (GroupMsg (..), pullGroup)
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.Layout.WindowNavigation (Direction2D (..))
import XMonad.StackSet (sink)

keybindings =
  [ (modCtrl "q", io exitSuccess),
    (modShift "r", spawn "xmonad --recompile && xmonad --restart"),
    -- Window management
    (modAlt "h", sendMessage Shrink),
    (modAlt "l", sendMessage Expand),
    (modAlt "j", sendMessage MirrorShrink),
    (modAlt "k", sendMessage MirrorExpand),
    (modShift "q", kill1),
    (modCtrl "<Delete>", killAll),
    (mod' "<Tab>", toggleWS' ["NSP"]),
    -- TODO: Add monitor key bindings

    -- Layout
    (mod' "f", sendMessage $ Toggle "monocle"),
    (mod' "=", sendMessage $ IncMasterN 1),
    (mod' "-", sendMessage $ IncMasterN (-1)),
    (mod' "l" +> "t", sendMessage $ Toggle "tall"),
    (mod' "l" +> "f", sendMessage $ Toggle "floating"),
    (mod' "l" +> shift "f", withFocused $ windows . sink),
    -- Sublayout
    (modCtrl "\\", withFocused $ sendMessage . UnMergeAll),
    (modCtrl "h", sendMessage $ pullGroup L),
    (modCtrl "l", sendMessage $ pullGroup R),
    (modCtrl "k", sendMessage $ pullGroup U),
    (modCtrl "j", sendMessage $ pullGroup D),
    -- Scratchpads
    (mod' "o", NS.scratchpad NS.SystemMonitor),
    (mod' "n", NS.scratchpad NS.Notes),
    -- Temporary
    ("M-C-p", sendMessage NextLayout),
    ("M-<Return>", spawn C.terminal), -- temporary
    ("M-d", spawn "dmenu_run -i -p \"Run: \"")
  ]
