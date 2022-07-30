module Lib.Utils where

import Data.List (elemIndex, find)
import Data.Maybe (fromMaybe, isJust)
import qualified Lib.Config as C
import XMonad
import XMonad.Hooks.DynamicLog (wrap)
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W

--
-- Keybinding helpers
--

mod' = ("M-" ++)

ctrl = ("C-" ++)

alt = ("M1-" ++)

shift = ("S-" ++)

modCtrl = mod' . ctrl

modShift = mod' . shift

modAlt = mod' . alt

(+>) prefix k = prefix ++ " " ++ k

--
-- Layout helpers
--

gaps i = spacingRaw False (Border i i i i) True (Border i i i i) True

--
-- spawn helpers
--
runExternal :: MonadIO m => m a -> m a
runExternal m = do
  uninstallSignalHandlers
  x <- m
  installSignalHandlers
  return x

inTerm cls cwd cmd = C.terminal ++ " -c '" ++ cls ++ "' -d " ++ cwd ++ " -e " ++ cmd

inEditor file = C.editor ++ " " ++ file

--inGuiEditor cls = inTerm cls . inEditor

--
-- XMobar helpers
--
pad pStart pEnd = replicate pStart ' ' `wrap` replicate pEnd ' '

padding = pad 1 1

onClick fn ws = "<action=`" ++ fn ws ++ "`>" ++ ws ++ "</action>"

viewWorkspace ws = "xdotool key super+" ++ show n
  where
    n = (+ 1) . fromMaybe 0 . elemIndex ws $ C.workspaces

workspaceId = (C.workspaces !!) . flip (-) 1

hasWindows :: String -> X Bool
hasWindows workspaceId = do
  ws <- gets windowset
  let workspaceM = find ((== workspaceId) . W.tag) . W.workspaces $ ws
  pure $ maybe False (isJust . W.stack) workspaceM
