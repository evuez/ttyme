{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib
  ( Entry(..)
  , Event(..)
  , Panel(..)
  , State(..)
  , Widget(..)
  , TimeUnit(..)
  , asHours
  , command
  , day
  , editor
  , entries
  , focus
  , focused
  , panel
  , splitOn
  , trimLeft
  , updateAll
  , updateOthers
  ) where

import API.Harvest (Entry(..), Id, Task(..))
import Brick.BChan (BChan)
import qualified Brick.Widgets.Edit as E (Editor)
import Config (Config)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime)
import qualified Data.Time.Format as F (defaultTimeLocale, formatTime)
import Lens.Micro (Lens', (%~), each, filtered)

data Panel
  = Sheet
  | Editor
  deriving (Eq)

data Event
  = Tick
  | Refreshed State

data Widget =
  Default
  deriving (Show, Eq, Ord)

data TimeUnit
  = Hour
  | Minute

data State = State
  { _config :: Config
  , _queue :: BChan Event
  , _command :: String
  , _panel :: Panel
  , _focus :: Maybe Id
  , _running :: Maybe Id
  , _day :: Day
  , _now :: UTCTime
  , _entries :: [Entry]
  , _tasks :: NonEmpty Task
  , _editor :: E.Editor String Widget
  }

--
-- Lens
--
panel :: Lens' State Panel
panel f s = (\x -> s {_panel = x}) <$> f (_panel s)

editor :: Lens' State (E.Editor String Widget)
editor f s = (\x -> s {_editor = x}) <$> f (_editor s)

entries :: Lens' State [Entry]
entries f s = (\x -> s {_entries = x}) <$> f (_entries s)

focus :: Lens' State (Maybe Id)
focus f s = (\x -> s {_focus = x}) <$> f (_focus s)

command :: Lens' State String
command f s = (\x -> s {_command = x}) <$> f (_command s)

day :: Lens' State Day
day f s = (\x -> s {_day = x}) <$> f (_day s)

focused :: Lens' State (Maybe Entry)
focused f s =
  (\case
     Just x -> updateFocused (const x) s
     Nothing -> s) <$>
  f (focused' s)

--
-- Helpers
--
focused' :: State -> Maybe Entry
focused' State {_focus = Nothing} = Nothing
focused' State {_focus = Just x, _entries = es} = find ((==) x . _id) es

updateFocused :: (Entry -> Entry) -> State -> State
updateFocused _ s@State {_focus = Nothing} = s
updateFocused f s@State {_focus = Just x} =
  s & entries . each . filtered ((==) x . _id) %~ f

updateOthers :: (Entry -> Entry) -> State -> State
updateOthers f s@State {_focus = Nothing} = s & entries . each %~ f
updateOthers f s@State {_focus = Just x} =
  s & entries . each . filtered ((/=) x . _id) %~ f

updateAll :: (Entry -> Entry) -> (Entry -> Entry) -> State -> State
updateAll f g = updateFocused f . updateOthers g

--
-- Global Helpers
--
asHours :: Float -> String
asHours t =
  F.formatTime
    F.defaultTimeLocale
    "%0H:%0M"
    (realToFrac (t * 60 * 60) :: DiffTime)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c s =
  case dropWhile (== c) s of
    [] -> []
    s' -> w : splitOn c s''
      where (w, s'') = break (== c) s'

trimLeft :: String -> String
trimLeft = dropWhile isSpace
