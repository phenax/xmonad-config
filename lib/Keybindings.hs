module Keybindings where

import XMonad
import XMonad.Prompt.Shell (shellPrompt)
import System.Exit (exitSuccess)
import XMonad.Layout (ChangeLayout (NextLayout))
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.StackSet (sink)

import qualified Config as C
import qualified Layouts as L

keybindings =
  [ ("M-C-q", io exitSuccess)
  , ("M-S-r", spawn "sh -c 'xmonad --recompile && xmonad --restart'")

  -- Layout
  , ("M-f", sendMessage $ Toggle "monocle")
  --, ("M-l t", sendMessage $ ChangeLayout "tall")
  , ("M-l <Return>", sendMessage NextLayout)

  -- Floating
  , ("M-l S-f", withFocused $ windows . sink)

  -- Temporary
  , ("M-<Return>", spawn C.terminal) -- temporary
  , ("M-d", spawn "dmenu_run -i -p \"Run: \"")
  ]

