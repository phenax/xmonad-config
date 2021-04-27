module Lib.Layouts where

import qualified Lib.Config as C
import qualified Lib.Theme as Theme
import Lib.Utils
--import XMonad

import XMonad hiding ((|||))
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Accordion
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts (toggleLayouts)
import XMonad.Layout.WindowArranger (windowArrange)
import XMonad.Layout.WindowNavigation (windowNavigation)

addGaps = gaps C.gaps

tall =
  withBorder C.borderSize $
    renamed [Replace "tall"]
      . addGaps
      . subLayout [] (smartBorders Simplest)
      $ ResizableTall 1 0 0.6 []

wide =
  withBorder C.borderSize $
    renamed [Replace "wide"]
      . addGaps
      . subLayout [] (smartBorders Simplest)
      $ Mirror (ResizableTall 1 (3 / 100) 0.65 [])

monocle = noBorders $ renamed [Replace "monocle"] Full

tallAccordion =
  withBorder C.borderSize $
    renamed [Replace "tallAccordion"]
      . addGaps
      . subLayout [] (smartBorders Simplest)
      $ Accordion

wideAccordion =
  withBorder C.borderSize $
    renamed [Replace "wideAccordion"]
      . addGaps
      . subLayout [] (smartBorders Simplest)
      $ Mirror Accordion

layoutHook xres = layoutModifiers defaultLayout
  where
    accent = Theme.accent xres
    fg = Theme.foreground xres
    bg = Theme.background xres
    tabTheme =
      def
        { fontName = C.font,
          activeColor = accent,
          inactiveColor = bg,
          activeBorderColor = accent,
          inactiveBorderColor = bg,
          activeTextColor = bg,
          inactiveTextColor = fg
        }

    layoutModifiers =
      avoidStruts
        . addTabs shrinkText tabTheme
        . smartBorders
        . mouseResize
        . windowArrange
        . toggleLayouts monocle
        . windowNavigation
        . mkToggle (NBFULL ?? NOBORDERS ?? EOT)

    defaultLayout =
      named "tall" tall
        ||| named "wide" wide
        ||| named "monocle" monocle
        ||| named "tallAccordion" tallAccordion
        ||| named "wideAccordion" wideAccordion
