Config
  { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
   , additionalFonts = []
   , borderColor = "black"
   , border = TopB
   , bgColor = "black"
   , fgColor = "grey"
   , alpha = 255
   , position = Top
   , textOffset = -1
   , iconOffset = -1
   , lowerOnStart = True
   , pickBroadest = False
   , persistent = False
   , hideOnStart = False
   , iconRoot = "."
   , allDesktops = True
   , overrideRedirect = True
   , commands = [ Run Date "%A, %e %b - %I:%M %p" "date" 10
                , Run StdinReader
                , Run Battery
                  [ "--template" , "<acstatus>"
                   , "--Low"      , "20"
                   , "--High"     , "80"
                   , "--low"      , "darkred"
                   , "--normal"   , "darkorange"
                   , "--high"     , "darkgreen"

                   , "--"
                   , "-o", "<left>%" --  (<timeleft>)
                   , "-O", "<left>% Charging"
                   , "-i", "<left>%"
                   ] 50
                ]
   , sepChar = "%"
   , alignSep = "}{"
   , template = "%StdinReader% }{ %date% | %battery% "
   }
