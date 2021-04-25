module Keybindings where

import XMonad
import XMonad.Prompt.Shell (shellPrompt)
import System.Exit (exitSuccess)
import XMonad.Layout (ChangeLayout (NextLayout))
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.StackSet (swapMaster, sink)
import XMonad.Layout.SubLayouts (pullGroup, GroupMsg (..))
import XMonad.Layout.WindowNavigation (Direction2D (..))
import XMonad.Layout.ResizableTile (MirrorResize (..))
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)

import qualified Config as C
import qualified Scratchpads as NS
import qualified Layouts as L
import Utils

keybindings =
  [ (modCtrl "q", io exitSuccess)
  , (modShift "r", spawn "sh -c 'xmonad --recompile && xmonad --restart'")

  -- Window management
  -- , (modAlt "m", windows swapMaster)
  , (modAlt "h", sendMessage Shrink)
  , (modAlt "l", sendMessage Expand)
  , (modAlt "j", sendMessage MirrorShrink)
  , (modAlt "k", sendMessage MirrorExpand)
  , (modShift "q", kill1)
  , (modCtrl "<Delete>", killAll)

  -- TODO: Add monitor key bindings

  -- Layout
  , (mod' "f", sendMessage $ Toggle "monocle")
  , (mod' "l" +> "t", sendMessage $ Toggle "tall")
  , (mod' "=", sendMessage $ IncMasterN 1)
  , (mod' "-", sendMessage $ IncMasterN (-1))

  -- Sublayout
  , (modCtrl "\\", withFocused (sendMessage . UnMergeAll))
  , (modCtrl "h", sendMessage $ pullGroup L)
  , (modCtrl "l", sendMessage $ pullGroup R)
  , (modCtrl "k", sendMessage $ pullGroup U)
  , (modCtrl "j", sendMessage $ pullGroup D)

  , (mod' "o", NS.scratchpad NS.SystemMonitor)
  , (mod' "n", NS.scratchpad NS.Notes)


  -- Floating
  , (mod' "l" +> shift "f", withFocused $ windows . sink)

  -- Temporary
  , ("M-C-p", sendMessage NextLayout)
  , ("M-<Return>", spawn C.terminal) -- temporary
  , ("M-d", spawn "dmenu_run -i -p \"Run: \"")
  ]

