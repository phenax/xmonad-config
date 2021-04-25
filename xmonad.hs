module Main (main) where

import System.IO (hPutStrLn)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import Control.Concurrent (forkIO)
import XMonad.Util.NamedScratchpad

import qualified Control.Exception.Extensible as E

import qualified Config as C
import Keybindings (keybindings)
import Scratchpads (scratchpads)
import qualified Layouts
import Utils (runExternal)

import qualified Theme

myManageHook =
  composeOne [ transience, isDialog -?> doCenterFloat ]
  <+> composeAll
    [ className =? "Pidgin" --> doFloat
    , className =? "XCalc" --> doFloat
    ]
  <+> namedScratchpadManageHook scratchpads

getConfig barProc xres =
  let
    fg = Theme.foreground xres
    accent = Theme.accent xres
  in desktopConfig
  { modMask = C.modKey
  , terminal = C.terminal
  , workspaces = C.workspaces
  , borderWidth = C.borderSize
  , normalBorderColor  = fg
  , focusedBorderColor = accent
  , manageHook = myManageHook <+> manageHook desktopConfig
  , layoutHook = Layouts.layoutHook
  , logHook = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
    { ppOutput = hPutStrLn barProc
    , ppCurrent = xmobarColor accent "" . wrap "[" "]"
    , ppTitle = xmobarColor fg "" . shorten 60 -- Faded title
    , ppHidden = xmobarColor fg ""
    , ppVisible = xmobarColor accent ""
    }
  } `additionalKeysP` keybindings

main = do
  xres <- runExternal Theme.loadXres

  barProc <- spawnPipe "xmobar ~/.xmonad/bar.hs"

  xmonad $ getConfig barProc xres

