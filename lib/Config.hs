module Config where

import XMonad
import XMonad.Prompt (XPPosition (..))
import qualified XMonad.Prompt as P

-- | UI

font = "xft:JetBrainsMono Nerd Font:size=8"


-- | Keybindings

modKey = mod4Mask

promptConfig = def
  { P.position = Top,
    P.alwaysHighlight = True,
    P.promptBorderWidth = 0,
    P.font = font
  }


-- | Apps

terminal = "sensible-terminal"

editor = "sensible-editor"

browser = "sensible-browser"

fileManager = "lf"


-- | Windows/workspace

borderSize = 2 :: Dimension

gaps = 2 :: Integer

workspaceCount = 9

workspaces = map (\n -> " " ++ show n ++ " ") [1 .. workspaceCount]

