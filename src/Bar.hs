module Main where

import qualified Lib.Config as C
import qualified Lib.Theme as Theme
import Lib.Utils
import XMonad.Hooks.DynamicLog (wrap)
import Xmobar

main :: IO ()
main = do
  xres <- Theme.loadXres
  xmobar $ config xres

sep = "|"

block = wrap "%" "%"

(<==>) a b = a ++ "}{" ++ b

(.|) a b = a ++ pad 1 1 sep ++ b

foreground c s = "<fc=" ++ c ++ ">" ++ s ++ "</fc>"

-- TODO: Update on signal
config xres =
  let bg = Theme.background xres
      fg = Theme.foreground xres
      -- accent = Theme.accent xres
      danger = Theme.danger xres
      faded = Theme.faded xres
   in defaultConfig
        { font = C.fontBar,
          additionalFonts = C.altFonts,
          border = NoBorder,
          bgColor = bg,
          fgColor = fg,
          alpha = 255,
          position = Top,
          textOffset = -1,
          iconOffset = -1,
          lowerOnStart = True,
          pickBroadest = False,
          persistent = False,
          hideOnStart = False,
          iconRoot = ".",
          allDesktops = True,
          overrideRedirect = True,
          template =
            block "UnsafeStdinReader"
              <==> block "default:Capture" ++ (pad 1 1 . block $ "default:Master")
              .| (pad 1 1 . block $ "bright")
              .| (pad 1 1 . block $ "date")
              .| (pad 1 1 . block $ "wi")
              .| (pad 1 3 . block $ "battery"),
          commands =
            [ Run UnsafeStdinReader,
              Run $ Date "\61463  %A, %e %b - %I:%M %p" "date" 50,
              Run $ Wireless "" ["-t", foreground faded "<ssid>" ++ ": <quality>%"] 50,
              Run $
                flip Brightness 30 $
                  concat
                    [ ["-t", "\61829  <percent>%"],
                      ["--"],
                      ["-D", "intel_backlight"]
                    ],
              Run $
                flip (Volume "default" "Capture") 10 $
                  concat
                    [ ["-t", "<status>"],
                      ["--"],
                      ["--on", "[mic: \61744] "],
                      ["--off", ""]
                    ],
              Run $
                flip (Volume "default" "Master") 10 $
                  concat
                    [ ["-t", "<status> <volume>%"],
                      ["--"],
                      ["--on", "\62559"],
                      ["--off", "\61453"]
                    ],
              Run . flip Battery 50 $
                concat
                  [ ["--template", onClick (const "~/scripts/powercontrol.sh menu") "\61457  <acstatus>"],
                    ["--Low", "30"],
                    ["--High", "80"],
                    ["--low", danger],
                    ["--normal", fg],
                    ["--high", fg],
                    ["--"],
                    ["-o", "<left>%"], --  (<timeleft>)
                    ["-O", "<left>% Charging"],
                    ["-i", "<left>%"]
                  ]
            ]
        }
