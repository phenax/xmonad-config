module Utils where

import XMonad
import XMonad.Layout.Spacing
import Control.Monad.IO.Class (MonadIO)

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

