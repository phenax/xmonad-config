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
          commands =
            [ Run StdinReader,
              Run $ Date "%A, %e %b - %I:%M %p" "date" 10,
              Run . flip Battery 50 $
                concat
                  [ ["--template", "<acstatus>"],
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
            ],
          template = "%StdinReader% }{ %date% | %battery% "
        }
