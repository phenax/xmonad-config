module Main (main) where

--import Control.Concurrent (forkIO, threadDelay)
import Data.Semigroup (All (..))
import qualified Lib.Config as C
import Lib.Keybindings (keybindings, mousebindings)
import qualified Lib.Layouts as Layouts
import Lib.Scratchpads (scratchpads)
import qualified Lib.Theme as Theme
import Lib.Utils as Util
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Layout.Fullscreen as FS
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

myManageHook =
  doF W.swapDown
    <+> namedScratchpadManageHook scratchpads
    <+> FS.fullscreenManageHook
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
          handleEventHook = FS.fullscreenEventHook,
          manageHook = myManageHook <+> manageSpawn <+> manageHook desktopConfig,
          layoutHook = FS.fullscreenFloat . FS.fullscreenFull $ Layouts.layoutHook xres,
          logHook =
            dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $
              xmobarPP
                { ppOutput = hPutStrLn barProc,
                  ppCurrent = xmobarColor fg accent . formatWS,
                  ppHidden = xmobarColor fg "" . formatWS . onClick viewWorkspace,
                  ppVisible = xmobarColor accent "" . formatWS . onClick viewWorkspace,
                  ppTitle = xmobarColor fg "" . shorten 60,
                  ppUrgent = xmobarColor danger "" . wrap "!" "!"
                }
        }
        `removeKeysP` ["M-<Return>", "M-p", "M-S-p"]
        `additionalKeysP` keybindings
        `additionalMouseBindings` mousebindings

-- TODO: Autostart after xmonad
main = do
  xres <- runExternal Theme.loadXres
  barProc <- spawnPipe "~/.xmonad/xmobar"
  --spawn "~/scripts/bin/with_zsh shotkey"
  --spawn "dunst -config ~/.config/dunst/dunstrc"
  --spawn "~/.fehbg"
  -- spawn "~/scripts/battery-watch.sh start"
  spawn "zsh ~/nixos/packages/dwm/autostart.sh"
  xmonad $ getConfig barProc xres
