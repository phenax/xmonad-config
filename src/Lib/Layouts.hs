{-# LANGUAGE FlexibleContexts #-}

module Lib.Layouts where

import qualified Lib.Config as C
import qualified Lib.Theme as Theme
import Lib.Utils
import XMonad hiding ((|||))
import XMonad.Actions.MouseResize (mouseResize)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Accordion
import qualified XMonad.Layout.Fullscreen as FS
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.MultiToggle (EOT (EOT), mkToggle, (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
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

layoutModifiers =
  avoidStruts
    . mouseResize
    . windowArrange
    . smartBorders
    . windowNavigation
    . toggleLayouts monocle
    . FS.fullscreenFloat
    . FS.fullscreenFull
    . mkToggle (NBFULL ?? NOBORDERS ?? EOT)

layoutHook xres =
  layoutModifiers $
    tall tabTheme
      ||| wide tabTheme
      ||| monocle
      ||| tallAccordion tabTheme
      ||| wideAccordion tabTheme
  where
    tabTheme = Theme.getTabTheme xres

tall tabTheme =
  withBorder C.borderSize $
    renamed [Replace "tall"]
      . addGaps
      . addTabs shrinkText tabTheme
      . subLayout [] (smartBorders Simplest)
      $ ResizableTall nmaster resizeDiff masterSize []
  where
    nmaster = 1
    masterSize = 0.6
    resizeDiff = 3 / 100

wide tabTheme =
  withBorder C.borderSize $
    renamed [Replace "wide"]
      . addGaps
      . addTabs shrinkText tabTheme
      . subLayout [] (smartBorders Simplest)
      $ Mirror (ResizableTall nmaster resizeDiff masterSize [])
  where
    nmaster = 1
    masterSize = 0.65
    resizeDiff = 3 / 100

monocle = noBorders $ renamed [Replace "monocle"] Full

tallAccordion tabTheme =
  withBorder C.borderSize $
    renamed [Replace "tallAccordion"]
      . addGaps
      . addTabs shrinkText tabTheme
      . subLayout [] (smartBorders Simplest)
      $ Accordion

wideAccordion tabTheme =
  withBorder C.borderSize $
    renamed [Replace "wideAccordion"]
      . addGaps
      . addTabs shrinkText tabTheme
      . subLayout [] (smartBorders Simplest)
      $ Mirror Accordion
