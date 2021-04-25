module Lib.Utils where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Lib.Config as C
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing

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

inTerm cls cmd = C.terminal ++ " -c " ++ cls ++ " -e " ++ cmd

inEditor file = C.editor ++ " " ++ file

inGuiEditor cls = inTerm cls . inEditor

--
-- XMobar helpers
--
padding = wrap " " " "

onClick fn ws = "<action=`" ++ fn ws ++ "`>" ++ ws ++ "</action>"

viewWorkspace ws = "xdotool key super+" ++ show n
  where
    n = (+ 1) . fromMaybe 0 . elemIndex ws $ C.workspaces
