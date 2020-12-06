module UI.Style
  ( bold
  , dim
  , active
  , activeBold
  , light
  , dark
  ) where

import Brick.AttrMap (AttrName, attrName)

bold :: AttrName
bold = attrName "bold"

dim :: AttrName
dim = attrName "dim"

active :: AttrName
active = attrName "active"

activeBold :: AttrName
activeBold = attrName "active-bold"

light :: AttrName
light = attrName "light"

dark :: AttrName
dark = attrName "dark"
