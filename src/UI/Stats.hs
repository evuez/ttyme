module UI.Stats
  ( render
  ) where

import qualified Brick.Types as T (Widget)
import qualified Brick.Widgets.Border as B (border)
import Brick.Widgets.Border.Style (unicodeRounded)
import qualified Brick.Widgets.Center as C (hCenter)
import Brick.Widgets.Core (hBox, padTopBottom, str, withAttr, withBorderStyle)
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (localDay, zonedTimeToLocalTime)
import Lib (Entry(..), State(..), Widget(..), asHours)
import UI.Style (bold)

render :: State -> T.Widget Widget
render s =
  withBorderStyle unicodeRounded . B.border . C.hCenter . padTopBottom 1 $
  hBox
    [ withAttr bold . drawHours . filter (isNow $ _day s) . _entries $ s
    , str " | "
    , drawDate s
    ]

drawHours :: [Entry] -> T.Widget Widget
drawHours = str . asHours . sum . map hours

drawDate :: State -> T.Widget Widget
drawDate = str . formatTime defaultTimeLocale "%F" . _day

isNow :: Day -> Entry -> Bool
isNow d e = d == (localDay . zonedTimeToLocalTime $ createdAt e)
