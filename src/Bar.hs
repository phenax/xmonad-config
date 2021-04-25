module Main where

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
        { font = "xft:JetBrainsMono Nerd Font:size=8",
          additionalFonts = ["xft:Font Awesome 5 Free:size=7", "xft:Symbols Nerd Font:size=7"],
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
              Run $
                Battery
                  [ "--template",
                    "<acstatus>",
                    "--Low",
                    "20",
                    "--High",
                    "80",
                    "--low",
                    danger,
                    "--normal",
                    "darkorange",
                    "--high",
                    "darkgreen",
                    "--",
                    "-o",
                    "<left>%", --  (<timeleft>)
                    "-O",
                    "<left>% Charging",
                    "-i",
                    "<left>%"
                  ]
                  50
            ],
          template = "%StdinReader% }{ %date% | %battery% "
        }
