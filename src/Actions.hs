module Actions
  ( add
  , autoRefresh
  , delete
  , edit
  , focusNext
  , focusPrev
  , nextDay
  , nextTask
  , nextWeek
  , open
  , prevDay
  , prevWeek
  , pushCommand
  , refresh
  , resetCommand
  , save
  , syncFetch
  , today
  , togglePause
  , updateTime
  ) where

import API.Harvest
  ( Entry(..)
  , Task(..)
  , createEntry
  , deleteEntry
  , fetchEntries
  , fetchTasks
  , updateHours
  , updateNote
  , updateRunning
  , updateTask
  )
import Brick.BChan (BChan, writeBChan)
import qualified Brick.Types as T (EventM, handleEventLensed)
import qualified Brick.Widgets.Edit as E
  ( applyEdit
  , editor
  , getEditContents
  , handleEditorEvent
  )
import Config (Config)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Function ((&))
import Data.List (find, intercalate)
import qualified Data.List.NonEmpty as N (break, fromList, head)
import Data.Maybe (listToMaybe)
import qualified Data.Text.Zipper as Z (clearZipper, insertMany)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (diffUTCTime, getCurrentTime, utctDay)
import qualified Graphics.Vty.Input.Events as V (Event)
import Lens.Micro ((%~), (.~), (?~), (^.), (^..), each, filtered)
import Lib
  ( Event(..)
  , Panel(..)
  , State(..)
  , TimeUnit(..)
  , Widget(..)
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
  )

add :: State -> IO State
add s = do
  e <- createEntry (_config s) $ N.head (_tasks s)
  pure $
    updateOthers
      (\e' -> e' {running = False})
      (s & focus ?~ _id e & entries %~ (:) e)

nextTask :: State -> IO State
nextTask s@State {_tasks = ts} =
  case s ^. focused of
    Just e@Entry {task = Task {taskId = t}} ->
      let (xs, y:ys) = N.break ((==) t . taskId) ts
          t' = (taskId . N.head . N.fromList) (ys ++ xs ++ [y])
      in do e' <- updateTask (_config s) e t'
            pure $ s & focused ?~ e'
    Nothing -> pure s

edit :: State -> V.Event -> T.EventM Widget State
edit s = T.handleEventLensed s editor E.handleEditorEvent

open :: State -> State
open s =
  case s ^. focused of
    Just Entry {notes = n} ->
      s & panel .~ Editor & editor .~ E.applyEdit (insert n) (_editor s)
    Nothing -> s
  where
    insert n = Z.insertMany (renderNotes n) . Z.clearZipper

focusNext :: State -> State
focusNext s@State {_focus = Nothing, _entries = es} =
  s {_focus = _id <$> listToMaybe es}
focusNext s@State {_focus = Just x, _entries = es} =
  open (s & focus .~ listToMaybe (map _id xs ++ [x])) & panel .~ Sheet
  where
    (_:xs) = dropWhile ((/=) x . _id) es

focusPrev :: State -> State
focusPrev s@State {_focus = Nothing, _entries = es} =
  s {_focus = _id <$> (listToMaybe . reverse) es}
focusPrev s@State {_focus = Just x, _entries = es} =
  open (s & focus .~ listToMaybe (map _id xs ++ [x])) & panel .~ Sheet
  where
    (_:xs) = dropWhile ((/=) x . _id) (reverse es)

togglePause :: State -> IO State
togglePause s =
  case s ^. focused of
    Just e -> do
      e' <- updateRunning (_config s) e (not . running $ e)
      pure $
        (updateAll (const e') (\e'' -> e'' {running = False}) s)
        {_running = Just (_id e)}
    Nothing -> pure s

refresh :: State -> IO State
refresh s@State {_queue = q} =
  forkIO (writeBChan q =<< (Refreshed <$> syncRefresh s)) >> pure s

autoRefresh :: State -> IO State
autoRefresh s = do
  d <- getCurrentTime
  if diffUTCTime d (_now s) > 30
    then refresh s
    else pure s

syncRefresh :: State -> IO State
syncRefresh s = do
  d <- getCurrentTime
  es <- fetchEntries (_config s) (s ^. day)
  pure $
    s
    { _running = _id <$> find running es
    , _now = d
    , _entries = es
    , _focus = _id <$> listToMaybe es
    }

syncFetch :: Config -> IO (BChan Event -> State)
syncFetch c = do
  d <- getCurrentTime
  es <- fetchEntries c (utctDay d)
  ts <- fetchTasks c
  pure $ \q ->
    State
    { _config = c
    , _queue = q
    , _command = []
    , _panel = Sheet
    , _focus = _id <$> listToMaybe es
    , _running = _id <$> find running es
    , _now = d
    , _day = utctDay d
    , _entries = es
    , _tasks = ts
    , _editor = E.editor Default Nothing (renderNotes $ notes' es)
    }
  where
    notes' :: [Entry] -> String
    notes' es = maybe "" notes (listToMaybe es)

save :: State -> IO State
save s@State {_config = c, _editor = ed} =
  case s ^. focused of
    Just e -> do
      e' <- updateNote c e notes'
      pure $ s & focused ?~ e' & panel .~ Sheet
      where notes' = intercalate "; " . filter (/= "") $ E.getEditContents ed
    Nothing -> pure (s & panel .~ Sheet)

updateTime :: TimeUnit -> State -> IO State
updateTime t s@State {_config = conf, _command = c} =
  case s ^. focused of
    Just e -> do
      e' <- updateHours conf e (h e t)
      pure . resetCommand $ s & focused ?~ e'
    Nothing -> pure $ resetCommand s
  where
    h e Hour = max 0 $ hours e + read (reverse c) :: Float
    h e Minute = max 0 $ hours e + read (reverse c) / 60

pushCommand :: State -> Char -> State
pushCommand s c = s & command %~ (:) c

resetCommand :: State -> State
resetCommand s = s {_command = []}

delete :: State -> IO State
delete s =
  case s ^. focused of
    Just e@Entry {_id = x} -> do
      void $ deleteEntry (_config s) e
      pure
        (s & entries .~ es & focus .~ (_id <$> listToMaybe es))
        {_running = _id <$> find running es}
      where es = s ^. entries ^.. each . filtered ((/=) x . _id)
    Nothing -> pure s

nextDay :: State -> IO State
nextDay s = syncRefresh (s & day %~ addDays 1)

prevDay :: State -> IO State
prevDay s = syncRefresh (s & day %~ addDays (-1))

nextWeek :: State -> IO State
nextWeek s = syncRefresh (s & day %~ addDays 7)

prevWeek :: State -> IO State
prevWeek s = syncRefresh (s & day %~ addDays (-7))

today :: State -> IO State
today s = do
  d <- utctDay <$> getCurrentTime
  syncRefresh (s & day .~ d)

renderNotes :: String -> String
renderNotes = unlines . filter (/= "") . map trimLeft . splitOn ';'
