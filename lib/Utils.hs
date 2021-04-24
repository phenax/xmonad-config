module Utils where

mod' = ("M-" ++)
ctrl = ("C-" ++)
alt = ("M1-" ++)
shift = ("S-" ++)

modCtrl = mod' . ctrl
modShift = mod' . shift
modAlt = mod' . alt

(+>) prefix k = prefix ++ " " ++ k

