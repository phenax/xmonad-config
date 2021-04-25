module Lib.Config where

import XMonad
import XMonad.Prompt (XPPosition (..))
import qualified XMonad.Prompt as P

-- | UI
fontFamily = "JetBrainsMono Nerd Font"

font = "xft:" ++ fontFamily ++ ":size=8"

fontBar = font

altFonts = ["xft:Font Awesome 5 Free:size=7", "xft:Symbols Nerd Font:size=7"]

-- | Keybindings
modKey = mod4Mask

promptConfig =
  def
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
