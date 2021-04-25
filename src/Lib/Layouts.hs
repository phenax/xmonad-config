module Lib.Layouts where

import qualified Lib.Config as C
import Lib.Utils
import XMonad
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Accordion
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.Simplest
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)

myTabTheme =
  def
    { fontName = C.font,
      activeColor = "#46d9ff",
      inactiveColor = "#313846",
      activeBorderColor = "#46d9ff",
      inactiveBorderColor = "#282c34",
      activeTextColor = "#282c34",
      inactiveTextColor = "#d0d0d0"
    }

addGaps = gaps C.gaps

tall =
  withBorder C.borderSize $
    renamed [Replace "tall"]
      . smartBorders
      . addGaps
      . addTabs shrinkText myTabTheme
      . subLayout [] (smartBorders Simplest)
      $ ResizableTall 1 (3 / 100) (1 / 2) []

wide =
  withBorder C.borderSize $
    renamed [Replace "wide"]
      . smartBorders
      . addGaps
      $ Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])

monocle =
  noBorders $
    renamed [Replace "monocle"]
      . smartBorders
      $ Full

floating =
  renamed [Replace "floating"]
    . smartBorders
    $ simplestFloat

tallAccordion =
  withBorder C.borderSize $
    renamed [Replace "tallAccordion"]
      . smartBorders
      . addGaps
      $ Accordion

wideAccordion =
  withBorder C.borderSize $
    renamed [Replace "wideAccordion"]
      . smartBorders
      . addGaps
      $ Mirror Accordion

layoutHook = layoutModifiers defaultLayout
  where
    layoutModifiers =
      avoidStruts
        . mouseResize
        . toggleLayouts monocle
        . windowArrange
        . windowNavigation
        . mkToggle (NBFULL ?? NOBORDERS ?? EOT)

    defaultLayout =
      tall
        ||| wide
        ||| monocle
        ||| tallAccordion
        ||| wideAccordion
        ||| floating
