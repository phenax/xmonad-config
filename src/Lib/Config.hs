module Lib.Config where

import XMonad

-- | UI
fontFamily = "JetBrainsMono Nerd Font"

font = "xft:" ++ fontFamily ++ ":size=8"

fontBar = font

altFonts = ["xft:Font Awesome 5 Free:size=7", "xft:Symbols Nerd Font:size=7"]

-- | Keybindings
modKey = mod4Mask

-- | Apps
terminal = "sensible-terminal"

editor = "sensible-editor"

browser = "sensible-browser"

fileManager = "lf"

-- | Windows/workspace
borderSize = 2 :: Dimension

gaps = 2 :: Integer

workspaces = ["1", "2", "3", "4", "5", "6: www", "7: meet", "8", "9: notes"]
