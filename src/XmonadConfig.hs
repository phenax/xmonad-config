module Main (main) where

--import Control.Concurrent (forkIO, threadDelay)

import qualified Data.Map as Map
import qualified Lib.Config as C
import Lib.Keybindings (keybindings, mousebindings)
import qualified Lib.Layouts as Layouts
import Lib.Scratchpads (scratchpads)
import qualified Lib.Theme as Theme
import Lib.Utils as Util
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.OnScreen (viewOnScreen)
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (docksEventHook, manageDocks)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.PerWorkspace (onWorkspace)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

-- TODO: Autostart after xmonad
-- TODO: Add server mode (https://gist.github.com/czaplicki/37ab38da4245deaea8c86ceae3ff2fa2)
main = do
  xres <- runExternal Theme.loadXres
  barProc <- spawnPipe "xmobar"
  --spawn "~/scripts/bin/with_zsh shotkey"
  --spawn "dunst -config ~/.config/dunst/dunstrc"
  --spawn "~/.fehbg"
  -- spawn "~/scripts/battery-watch.sh start"
  spawn "~/nixos/scripts/monitor.sh sidekick" -- Configure sidekick monitor
  spawn "zsh ~/nixos/packages/xmonad/autostart.sh"
  dirs <- getDirectories
  launch (getConfig barProc xres) dirs

onStartup = do
  let sidekick = Util.workspaceId 9
  let primary = Util.workspaceId 1

  -- Load on second screen
  empty <- not <$> Util.hasWindows sidekick
  if empty
    then spawnOn sidekick "st -c clock -e tty-clock -t -b -c -s -f '%A, %d %b' -C 5"
    else pure ()

  -- Sidekick on w9 and primary on w1
  windows $ viewOnScreen 1 sidekick
  windows $ viewOnScreen 0 primary

onLayout =
  onWorkspace (Util.workspaceId 9) Layouts.monocle

-- Manage hook
myManageHook =
  doF W.swapDown
    <+> namedScratchpadManageHook scratchpads
    <+> FS.fullscreenManageHook
    <+> manageDocks
    <+> manageSpawn
    <+> manageHook desktopConfig
    <+> composeAll
      [ isDialog --> doCenterFloat,
        className =? "Pidgin" --> doFloat,
        className =? "XCalc" --> doFloat
      ]

getConfig barProc xres =
  let bg = Theme.background xres
      fg = Theme.foreground xres
      accent = Theme.accent xres
      danger = Theme.danger xres
      formatWS = Util.pad 1 1
      refocusPred = refocusingIsActive <||> isFloat
      logHook =
        dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $
          xmobarPP
            { ppOutput = hPutStrLn barProc,
              ppCurrent = xmobarColor fg accent . formatWS,
              ppHidden = xmobarColor fg "" . formatWS . onClick viewWorkspace,
              ppVisible = xmobarColor accent "" . formatWS . onClick viewWorkspace,
              ppTitle = xmobarColor fg "" . shorten 60,
              ppUrgent = xmobarColor danger "" . wrap "[" "]"
            }
   in desktopConfig
        { modMask = C.modKey,
          terminal = C.terminal,
          workspaces = C.workspaces,
          focusFollowsMouse = False,
          clickJustFocuses = True,
          -- Border
          borderWidth = C.borderSize,
          normalBorderColor = bg,
          focusedBorderColor = accent,
          -- Hooks
          handleEventHook = refocusLastWhen refocusPred <+> docksEventHook <+> FS.fullscreenEventHook,
          manageHook = myManageHook,
          layoutHook = refocusLastLayoutHook $ Layouts.layoutHook xres,
          logHook = refocusLastLogHook <+> logHook,
          startupHook = startupHook desktopConfig >> onStartup,
          mouseBindings = \_ -> Map.empty
        }
        `removeKeysP` ["M-<Return>", "M-p", "M-S-p", "M-S-c", "M-h", "M-l", "M-,", "M-."]
        `additionalKeysP` keybindings
        `additionalMouseBindings` mousebindings
