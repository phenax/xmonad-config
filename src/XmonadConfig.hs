module Main (main) where

import qualified Lib.Config as C
import Lib.Keybindings (keybindings)
import qualified Lib.Layouts as Layouts
import Lib.Scratchpads (scratchpads)
import qualified Lib.Theme as Theme
import Lib.Utils
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

myManageHook =
  doF W.swapDown
    <+> namedScratchpadManageHook scratchpads
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
          manageHook = myManageHook <+> manageSpawn <+> manageHook desktopConfig,
          layoutHook = Layouts.layoutHook,
          logHook =
            dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $
              xmobarPP
                { ppOutput = hPutStrLn barProc,
                  ppCurrent = xmobarColor fg accent . padding,
                  ppHidden = xmobarColor fg "" . padding . onClick viewWorkspace,
                  ppVisible = xmobarColor accent "" . padding . onClick viewWorkspace,
                  ppTitle = xmobarColor fg "" . shorten 60,
                  ppUrgent = xmobarColor danger "" . wrap "!" "!"
                }
        }
        `additionalKeysP` keybindings

main = do
  xres <- runExternal Theme.loadXres
  barProc <- spawnPipe "~/.xmonad/bin/statusbar"
  spawn "shotkey"
  spawn "dunst -config ~/.config/dunst/dunstrc"
  spawn "~/.fehbg"
  -- spawn "~/scripts/battery-watch.sh start"
  xmonad $ getConfig barProc xres
