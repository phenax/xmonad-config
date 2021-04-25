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
import XMonad.Actions.SpawnOn
import qualified XMonad.StackSet as W
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

import qualified Control.Exception.Extensible as E

import qualified Config as C
import Keybindings (keybindings)
import Scratchpads (scratchpads)
import qualified Layouts
import Utils (runExternal)

import qualified Theme

myManageHook =
  doF W.swapDown
  <+> namedScratchpadManageHook scratchpads
  <+> composeAll
    [ isDialog --> doCenterFloat
    , className =? "Pidgin" --> doFloat
    , className =? "XCalc" --> doFloat
    ]

padding = wrap " " " "

onClick fn ws = "<action=`" ++ fn ws ++ "`>" ++ ws ++ "</action>"

viewWorkspace ws = "xdotool key super+" ++ show n
  where
    n = (+ 1) . fromMaybe 0 . elemIndex ws $ C.workspaces

getConfig barProc xres =
  let
    bg = Theme.background xres
    fg = Theme.foreground xres
    accent = Theme.accent xres
    danger = Theme.danger xres
  in desktopConfig
  { modMask = C.modKey
  , terminal = C.terminal
  , workspaces = C.workspaces
  , focusFollowsMouse = False
  , clickJustFocuses = True

  -- Border
  , borderWidth = C.borderSize
  , normalBorderColor  = bg
  , focusedBorderColor = accent

  -- Hooks
  , manageHook = myManageHook <+> manageSpawn <+> manageHook desktopConfig
  , layoutHook = Layouts.layoutHook
  , logHook =
      dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ xmobarPP
        { ppOutput = hPutStrLn barProc
        , ppCurrent = xmobarColor fg accent . padding
        , ppHidden = xmobarColor fg "" . padding . onClick viewWorkspace
        , ppVisible = xmobarColor accent "" . padding . onClick viewWorkspace
        , ppTitle = xmobarColor fg "" . shorten 60
        , ppUrgent = xmobarColor danger "" . wrap "!" "!"
        }
  } `additionalKeysP` keybindings

main = do
  xres <- runExternal Theme.loadXres
  barProc <- spawnPipe "~/.xmonad/bin/statusbar"
  xmonad $ getConfig barProc xres

