module Main where

import qualified Lib.Config as C
import qualified Lib.Theme as Theme
import Xmobar

main :: IO ()
main = Theme.loadXres >>= xmobar . config

config xres =
  let bg = Theme.background xres
      fg = Theme.foreground xres
      --accent = Theme.accent xres
      danger = Theme.danger xres
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
          template = "%StdinReader% }{  %default:Capture% %default:Master%  |  %bright%  |  %date%  |  %battery%  ",
          commands =
            [ Run StdinReader,
              Run $ Date "\61463  %A, %e %b - %I:%M %p" "date" 10,
              Run $
                flip Brightness 30 $
                  concat
                    [ ["-t", "\61829  <percent>%"],
                      ["--"],
                      ["-D", "intel_backlight"]
                    ],
              Run $
                flip (Volume "default" "Capture") 20 $
                  concat
                    [ ["-t", "<status>"],
                      ["--"],
                      ["--on", "[mic: \61744] "],
                      ["--off", ""]
                    ],
              Run $
                flip (Volume "default" "Master") 20 $
                  concat
                    [ ["-t", "<status> <volume>%"],
                      ["--"],
                      ["--on", "\61480"],
                      ["--off", "\61478"]
                    ],
              Run . flip Battery 50 $
                concat
                  [ ["--template", "\61457  <acstatus>"],
                    ["--Low", "20"],
                    ["--High", "80"],
                    ["--low", danger],
                    ["--normal", "darkorange"],
                    ["--high", "darkgreen"],
                    ["--"],
                    ["-o", "<left>%"], --  (<timeleft>)
                    ["-O", "<left>% Charging"],
                    ["-i", "<left>%"]
                  ]
            ]
        }
