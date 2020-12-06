{-# LANGUAGE OverloadedStrings #-}

module Main where

import Actions
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
  )
import qualified Brick.AttrMap as A
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
  ( App(..)
  , appAttrMap
  , appChooseCursor
  , appDraw
  , appHandleEvent
  , appStartEvent
  , continue
  , customMain
  , halt
  , showFirstCursor
  )
import qualified Brick.Types as T (BrickEvent(..), EventM, Next, Widget)
import Brick.Util (on)
import Brick.Widgets.Core (hBox, padLeftRight, vBox)
import qualified Brick.Widgets.Edit as E (editAttr, editFocusedAttr)
import qualified Brick.Widgets.List as L (listAttr, listSelectedAttr)
import qualified Config as C (load)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import qualified Graphics.Vty as V
  ( Event(..)
  , Key(..)
  , black
  , bold
  , brightBlack
  , brightWhite
  , defAttr
  , defaultConfig
  , dim
  , magenta
  , mkVty
  , white
  , withBackColor
  , withForeColor
  , withStyle
  )
import Lib (Event(..), Panel(..), State(..), TimeUnit(..), Widget)
import qualified UI.Editor as Editor (render)
import qualified UI.Sheet as Sheet (render)
import qualified UI.Stats as Stats (render)
import qualified UI.Style as S (active, activeBold, bold, dark, dim, light)

drawUI :: State -> [T.Widget Widget]
drawUI s =
  [ hBox
      [ padLeftRight 1 (Sheet.render s)
      , padLeftRight 1 (vBox [Stats.render s, Editor.render s])
      ]
  ]

appEvent :: State -> T.BrickEvent Widget Event -> T.EventM Widget (T.Next State)
appEvent s@State {_command = c, _panel = p} (T.VtyEvent e) =
  case (p, c, e) of
    (Sheet, [], V.EvKey (V.KChar 'q') []) -> M.halt s
    (Sheet, [], V.EvKey (V.KChar 'k') []) -> M.continue (focusPrev s)
    (Sheet, [], V.EvKey (V.KChar 'j') []) -> M.continue (focusNext s)
    (Sheet, [], V.EvKey (V.KChar 'h') []) -> M.continue =<< liftIO (prevDay s)
    (Sheet, [], V.EvKey (V.KChar 'l') []) -> M.continue =<< liftIO (nextDay s)
    (Sheet, [], V.EvKey (V.KChar 'H') []) -> M.continue =<< liftIO (prevWeek s)
    (Sheet, [], V.EvKey (V.KChar 'L') []) -> M.continue =<< liftIO (nextWeek s)
    (Sheet, [], V.EvKey (V.KChar 'n') []) -> M.continue =<< liftIO (today s)
    (Sheet, [], V.EvKey (V.KChar 't') []) -> M.continue =<< liftIO (nextTask s)
    (Sheet, [], V.EvKey (V.KChar 'a') []) -> M.continue =<< liftIO (add s)
    (Sheet, [], V.EvKey (V.KChar 'p') []) ->
      M.continue =<< liftIO (togglePause s)
    (Sheet, [], V.EvKey (V.KChar 'r') []) -> M.continue =<< liftIO (refresh s)
    (Sheet, [], V.EvKey (V.KChar 'e') []) -> M.continue (open s)
    (Sheet, [], V.EvKey (V.KChar k) [])
      | isDigit k || k == '-' || k == 'd' -> M.continue (pushCommand s k)
    (Sheet, _:_, V.EvKey (V.KChar k) [])
      | isDigit k -> M.continue (pushCommand s k)
    (Sheet, _:_, V.EvKey (V.KChar 'h') []) ->
      M.continue =<< liftIO (updateTime Hour s)
    (Sheet, _:_, V.EvKey (V.KChar 'm') []) ->
      M.continue =<< liftIO (updateTime Minute s)
    (Sheet, ['d'], V.EvKey (V.KChar 'd') []) -> M.continue =<< liftIO (delete s)
    (Editor, [], V.EvKey V.KEsc []) -> M.continue =<< liftIO (save s)
    (Editor, [], _) -> M.continue =<< edit s e
    _ -> M.continue (resetCommand s)
appEvent s (T.AppEvent Tick) = M.continue =<< liftIO (autoRefresh s)
appEvent s (T.AppEvent (Refreshed State {_running = r, _now = n, _entries = es})) =
  M.continue (s {_running = r, _now = n, _entries = es})
appEvent s _ = M.continue s

attrMap :: A.AttrMap
attrMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black)
    , (L.listSelectedAttr, V.black `on` V.white)
    , (S.bold, V.withStyle V.defAttr V.bold)
    , (S.dim, V.withStyle V.defAttr V.dim)
    , (S.active, V.magenta `on` V.black)
    , ( S.activeBold
      , V.withForeColor
          (V.withBackColor (V.withStyle V.defAttr V.bold) V.black)
          V.magenta)
    , (S.light, V.white `on` V.brightBlack)
    , (S.dark, V.brightBlack `on` V.black)
    , (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.brightWhite `on` V.black)
    ]

app :: M.App State Event Widget
app =
  M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrMap
  }

main :: IO ()
main = do
  Right c <- C.load
  chan <- newBChan 10
  _ <-
    forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 30000000
  s <- syncFetch c
  initVty <- buildVty
  void $ M.customMain initVty buildVty (Just chan) app (s chan)
  where
    buildVty = V.mkVty V.defaultConfig
