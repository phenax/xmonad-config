module Config where

import XMonad
import XMonad.Prompt

-- | Keybindings

modKey = mod4Mask

promptConfig = def
  { position = Top,
    alwaysHighlight = True,
    promptBorderWidth = 0,
    font = "xft:monospace:size=9"
  }


-- | Apps

terminal = "sensible-terminal"

browser = "sensible-browser"

fileManager = "lf"


-- | Windows/workspace

borderSize = 2 :: Dimension

workspaceCount = 9

workspaces = map (\n -> " " ++ show n ++ " ") [1 .. workspaceCount]

