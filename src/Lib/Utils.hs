module Lib.Utils where

import Control.Monad.IO.Class (MonadIO)
import qualified Lib.Config as C
import XMonad
import XMonad.Layout.Spacing

mod' = ("M-" ++)

ctrl = ("C-" ++)

alt = ("M1-" ++)

shift = ("S-" ++)

modCtrl = mod' . ctrl

modShift = mod' . shift

modAlt = mod' . alt

(+>) prefix k = prefix ++ " " ++ k

gaps i = spacingRaw False (Border i i i i) True (Border i i i i) True

runExternal :: MonadIO m => m a -> m a
runExternal m = do
  uninstallSignalHandlers
  x <- m
  installSignalHandlers
  return x

inTerm cls cmd = C.terminal ++ " -c " ++ cls ++ " -e " ++ cmd

inEditor file = C.editor ++ " " ++ file

inGuiEditor cls = inTerm cls . inEditor
