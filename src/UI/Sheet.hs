module UI.Sheet
  ( render
  ) where

import Brick (strWrap)
import qualified Brick.AttrMap as A (applyAttrMappings)
import qualified Brick.Types as T (Padding(..), Widget)
import Brick.Util (on)
import qualified Brick.Widgets.Border as B (border, borderAttr, hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Center as C (center, hCenter)
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , padBottom
  , padLeft
  , str
  , updateAttrMap
  , vBox
  , withAttr
  , withBorderStyle
  )
import Data.Bool (bool)
import qualified Data.Time.Format as F (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (ZonedTime(..))
import qualified Graphics.Vty as V (black, magenta)
import Lib (Entry(..), State(..), Widget(..), asHours)
import UI.Style (activeBold, bold)

render :: State -> T.Widget Widget
render s =
  C.hCenter $
  vBox (drawEntries s `lor` [C.center $ str "Create a new entry with [a]"])

drawEntries :: State -> [T.Widget Widget]
drawEntries State {_focus = x, _entries = es} =
  map (\e -> drawEntry (Just (_id e) == x) e) es

drawEntry :: Bool -> Entry -> T.Widget Widget
drawEntry focused e =
  focusedStyle focused . withBorderStyle unicodeRounded . B.border $
  (task' <+> time) <=> B.hBorder <=> notes' <=> (hours' <+> cost)
  where
    task' = (withAttr bold . str . show . task) e
    time = padLeft T.Max . str $ formatTime (createdAt e)
    notes' = padBottom (T.Pad 1) (strWrap $ notes e)
    hours' = clockStyle e . str . asHours $ hours e
    cost = padLeft T.Max . str $ "$" ++ show ((round $ hours e * rate e) :: Int)

focusedStyle :: Bool -> T.Widget Widget -> T.Widget Widget
focusedStyle True =
  updateAttrMap (A.applyAttrMappings [(B.borderAttr, V.magenta `on` V.black)])
focusedStyle False = id

clockStyle :: Entry -> T.Widget Widget -> T.Widget Widget
clockStyle Entry {running = False} = id
clockStyle Entry {running = True} = withAttr activeBold

formatTime :: ZonedTime -> String
formatTime = F.formatTime F.defaultTimeLocale "%F %R"

lor :: (Foldable f) => f a -> f a -> f a
lor a b = bool a b (null a)
