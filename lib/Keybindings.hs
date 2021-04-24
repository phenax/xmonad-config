module Keybindings where

import XMonad
import XMonad.Prompt.Shell (shellPrompt)
import System.Exit (exitSuccess)
import XMonad.Layout (ChangeLayout (NextLayout))
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.StackSet (focusMaster, sink)
import XMonad.Layout.SubLayouts (pullGroup, GroupMsg (..))
import XMonad.Layout.WindowNavigation (Direction2D (..))
import XMonad.Layout.ResizableTile (MirrorResize (..))

import qualified Config as C
import qualified Layouts as L
import Utils

keybindings =
  [ (modCtrl "q", io exitSuccess)
  , (modShift "r", spawn "sh -c 'xmonad --recompile && xmonad --restart'")

  -- Window management
  , (modAlt "m", windows focusMaster)
  , (modAlt "h", sendMessage Shrink)
  , (modAlt "l", sendMessage Expand)
  , (modAlt "j", sendMessage MirrorShrink)
  , (modAlt "k", sendMessage MirrorExpand)

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

  -- Floating
  , (mod' "l" +> shift "f", withFocused $ windows . sink)

  -- Temporary
  , ("M-C-p", sendMessage NextLayout)
  , ("M-<Return>", spawn C.terminal) -- temporary
  , ("M-d", spawn "dmenu_run -i -p \"Run: \"")
  ]

