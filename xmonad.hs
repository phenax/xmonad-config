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
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.ToggleLayouts (ToggleLayout (..), toggleLayouts)
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)

modKey = mod4Mask

keybindings =
  [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)),
    ("M-p", shellPrompt myXPConfig),
    ("M-<Esc>", sendMessage (Toggle "Full")),
    ("M-<Return>", spawn "st")
  ]

myLayouts = toggleLayouts (noBorders Full) others
  where
    others = ResizableTall 1 (1.5 / 100) (3 / 5) [] ||| emptyBSP

myXPConfig =
  def
    { position = Top,
      alwaysHighlight = True,
      promptBorderWidth = 0,
      font = "xft:monospace:size=9"
    }

myManageHook =
  composeOne
    [ transience,
      isDialog -?> doCenterFloat
    ]
    <+> composeAll
      [ className =? "Pidgin" --> doFloat,
        className =? "XCalc" --> doFloat
      ]

myWorkspaces = map (\n -> " " ++ show n ++ " ") [1 .. 9]

myWorkspaceIndices = [1 .. 9]

--------------------------------------------------------------------------------
main = do
  barProc <- spawnPipe "xmobar ~/.xmonad/bar.hs"

  xmonad $
    desktopConfig
      { modMask = modKey, -- Use the "Win" key for the mod key
        manageHook = myManageHook <+> manageHook desktopConfig,
        layoutHook = desktopLayoutModifiers $ myLayouts,
        workspaces = myWorkspaces,
        logHook =
          dynamicLogWithPP
            xmobarPP
              { ppOutput = hPutStrLn barProc,
                ppTitle = xmobarColor "green" "" . shorten 50
              }
              --, logHook    = dynamicLogWithPP $ xmobarPP
              --{ ppOutput = hPutStrLn barProc
              --, ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]"
              --, ppVisible = xmobarColor "#98be65" ""
              --, ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""
              --, ppHiddenNoWindows = xmobarColor "#c792ea" ""
              --, ppTitle = xmobarColor "#b3afc2" "" . shorten 60
              --, ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"
              --, ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"
              ---- , ppExtras  = [windowCount]
              ---- , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              --}
      }
      `additionalKeysP` keybindings

--------------------------------------------------------------------------------
