module UI.Editor
  ( render
  ) where

import qualified Brick.Types as T (Widget)
import qualified Brick.Widgets.Border as B (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Core (str, withBorderStyle)
import Brick.Widgets.Edit (renderEditor)
import Lib (Panel(..), State(..), Widget(..))

render :: State -> T.Widget Widget
render s =
  withBorderStyle unicodeRounded . B.border $
  renderEditor (str . unlines) (_panel s == Editor) (_editor s)
