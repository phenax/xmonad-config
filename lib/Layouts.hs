module Layouts where

import XMonad
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ToggleLayouts (ToggleLayout (..), toggleLayouts)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Actions.MouseResize
import XMonad.Layout.Accordion

import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.SimplestFloat
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Layout.WindowNavigation (windowNavigation)

import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))

import qualified Config as C

gaps i =
  spacingRaw False (Border i i i i) True (Border i i i i) True

addGaps = gaps 2

tall = withBorder C.borderSize
  $ renamed [Replace "tall"]
  . smartBorders
  . addGaps
  -- . addTabs shrinkText myTabTheme
  . subLayout [] (smartBorders Simplest)
  $ ResizableTall 1 (3/100) (1/2) []

wide = withBorder C.borderSize
  $ renamed [Replace "wide"]
  . smartBorders
  . addGaps
  $ Mirror (ResizableTall 1 (3/100) (1/2) [])

monocle = noBorders
  $ renamed [Replace "monocle"]
  . smartBorders
  $ Full

floating =
  renamed [Replace "floating"]
  . smartBorders
  $ simplestFloat

tallAccordion = withBorder C.borderSize
  $ renamed [Replace "tallAccordion"]
  . smartBorders
  . addGaps
  $ Accordion

wideAccordion  = withBorder C.borderSize
  $ renamed [Replace "wideAccordion"]
  . smartBorders
  . addGaps
  $ Mirror Accordion

layoutHook = layoutModifiers $ defaultLayout
    where
      layoutModifiers = avoidStruts
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
        -- ||| noBorders tabs
        -- ||| grid
        -- ||| threeCol
        -- ||| threeRow


