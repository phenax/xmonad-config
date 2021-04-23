--------------------------------------------------------------------------------

-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
import System.Exit
import System.IO (hPutStrLn)
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

import qualified Config as C
import Keybindings (keybindings)
import qualified Layouts

myManageHook =
  composeOne
    [ transience,
      isDialog -?> doCenterFloat
    ]
    <+> composeAll
      [ className =? "Pidgin" --> doFloat,
        className =? "XCalc" --> doFloat
      ]

--------------------------------------------------------------------------------
main = do
  barProc <- spawnPipe "xmobar ~/.xmonad/bar.hs"

  xmonad $
    desktopConfig
      { modMask = C.modKey,
        manageHook = myManageHook <+> manageHook desktopConfig,
        layoutHook = Layouts.layoutHook,
        workspaces = C.workspaces,
        logHook =
          dynamicLogWithPP
            xmobarPP
              { ppOutput = hPutStrLn barProc,
                ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]",
                ppTitle = xmobarColor "#b3afc2" "" . shorten 60,
                ppHidden = xmobarColor "#82AAFF" "" . wrap "*" "",
                ppVisible = xmobarColor "#98be65" ""
              }
      }
      `additionalKeysP` keybindings

--------------------------------------------------------------------------------
