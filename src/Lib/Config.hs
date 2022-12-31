module Lib.Config where

import XMonad

-- | UI

font = "JetBrainsMono Nerd Font" ++ " 8"

fontBar = font

altFonts = ["Font Awesome 5 Free 7", "Symbols Nerd Font 7"]

-- | Keybindings
modKey = mod4Mask

-- | Apps
terminal = "sensible-terminal"

editor = "sensible-editor"

browser = "sensible-browser"

fileManager = "lf"

-- | Windows/workspace
borderSize = 3 :: Dimension

gaps = 2 :: Integer

workspaces = ["1: main", "2", "3", "4", "5", "6: www", "7", "8", "9: sidekick"]
