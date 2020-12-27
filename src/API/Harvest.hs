{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Harvest
  ( Entry(..)
  , Id
  , Project(..)
  , Task(..)
  , createEntry
  , fetchEntries
  , fetchTasks
  , updateHours
  , updateNote
  , updateRunning
  , updateTask
  , deleteEntry
  ) where

import Config (Config)
import qualified Config as C (Config(..))
import Control.Monad (void)
import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value(Null)
  , (.:)
  , (.=)
  , object
  , withObject
  )
import Data.Bool (bool)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (ZonedTime(..), getZonedTime)
import Network.HTTP.Req
  ( DELETE(..)
  , GET(..)
  , NoReqBody(..)
  , Option
  , PATCH(..)
  , POST(..)
  , ReqBodyJson(..)
  , Scheme(..)
  , Url
  , (/:)
  , (=:)
  , defaultHttpConfig
  , header
  , https
  , ignoreResponse
  , jsonResponse
  , req
  , responseBody
  , runReq
  )

type Id = Int

newtype Entries = Entries
  { entries :: [Entry]
  } deriving (Show)

data Entry = Entry
  { _id :: Id
  , date :: Day
  , project :: Project
  , task :: Task
  , hours :: Float
  , notes :: String
  , running :: Bool
  , rate :: Float
  , createdAt :: ZonedTime
  , updatedAt :: ZonedTime
  } deriving (Show)

data NewEntry = NewEntry
  { projectId :: String
  , taskId :: Id
  , spentDate :: ZonedTime
  , notes :: String
  }

data Task = Task
  { taskId :: Id
  , name :: String
  }

newtype Project = Project
  { name :: String
  }

newtype Tasks = Tasks
  { tasks :: NonEmpty Task
  } deriving (Show)

data Update = Update
  { notes :: Maybe String
  , taskId :: Maybe Id
  , hours :: Maybe Float
  } deriving (Show)

instance FromJSON Entries where
  parseJSON = withObject "Entries" $ \v -> Entries <$> (v .: "time_entries")

instance FromJSON Entry where
  parseJSON =
    withObject "Entry" $ \v ->
      Entry <$> (v .: "id") <*> (v .: "spent_date") <*> (v .: "project") <*>
      (v .: "task") <*>
      (v .: "hours") <*>
      (v .: "notes") <*>
      (v .: "is_running") <*>
      (v .: "billable_rate") <*>
      (v .: "created_at") <*>
      (v .: "updated_at")

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> Task <$> (v .: "id") <*> (v .: "name")

instance FromJSON Project where
  parseJSON = withObject "Project" $ \v -> Project <$> (v .: "name")

instance FromJSON Tasks where
  parseJSON = withObject "Tasks" $ \v -> Tasks <$> (v .: "tasks")

instance Show Task where
  show = name :: Task -> String

instance Show Project where
  show = name :: Project -> String

instance ToJSON Update where
  toJSON u =
    object $
    filter
      ((/=) Null . snd)
      [ "notes" .= notes (u :: Update)
      , "task_id" .= taskId (u :: Update)
      , "hours" .= hours (u :: Update)
      ]

instance ToJSON NewEntry where
  toJSON e =
    object
      [ "project_id" .= projectId e
      , "task_id" .= taskId (e :: NewEntry)
      , "spent_date" .= spentDate e
      , "notes" .= notes (e :: NewEntry)
      ]

root :: Url 'Https
root = https "api.harvestapp.com" /: "v2"

showId :: Entry -> Text
showId = pack . show . _id

mkUpdate :: Update
mkUpdate = Update {notes = Nothing, taskId = Nothing, hours = Nothing}

headers :: Config -> [Option scheme]
headers c =
  [ header "Authorization" (mconcat ["Bearer ", C.harvestToken c])
  , header "Harvest-Account-ID" (C.harvestAccount c)
  , header "User-Agent" "ttyme (evuez@liftm.io)"
  ]

fetchEntries :: Config -> Day -> IO [Entry]
fetchEntries c d =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (root /: "time_entries")
        NoReqBody
        jsonResponse
        (mconcat $ ["from" =: d, "to" =: d] ++ headers c)
    pure $ entries (responseBody r :: Entries)

createEntry :: Config -> Task -> IO Entry
createEntry c t = do
  z <- getZonedTime
  runReq defaultHttpConfig $ do
    r <-
      req
        POST
        (root /: "time_entries")
        (ReqBodyJson $ entry z)
        jsonResponse
        (mconcat $ headers c)
    pure (responseBody r :: Entry)
  where
    entry z =
      NewEntry
      { projectId = C.projectId c
      , taskId = taskId (t :: Task)
      , spentDate = z
      , notes = ""
      }

deleteEntry :: Config -> Entry -> IO ()
deleteEntry c e =
  runReq defaultHttpConfig $
  void $
  req
    DELETE
    (root /: "time_entries" /: showId e)
    NoReqBody
    ignoreResponse
    (mconcat $ headers c)

fetchTasks :: Config -> IO (NonEmpty Task)
fetchTasks c =
  runReq defaultHttpConfig $ do
    r <-
      req
        GET
        (root /: "tasks")
        NoReqBody
        jsonResponse
        (mconcat $ ("is_active" =: True) : headers c)
    pure $ tasks (responseBody r :: Tasks)

updateNote :: Config -> Entry -> String -> IO Entry
updateNote c e s = update c e mkUpdate {notes = Just s}

updateTask :: Config -> Entry -> Id -> IO Entry
updateTask c e t = update c e mkUpdate {taskId = Just t}

updateHours :: Config -> Entry -> Float -> IO Entry
updateHours c e h = update c e mkUpdate {hours = Just h}

updateRunning :: Config -> Entry -> Bool -> IO Entry
updateRunning c e v =
  runReq defaultHttpConfig $ do
    r <- req PATCH url NoReqBody jsonResponse (mconcat $ headers c)
    pure (responseBody r :: Entry)
  where
    url = root /: "time_entries" /: showId e /: bool "stop" "restart" v

update :: Config -> Entry -> Update -> IO Entry
update c e u =
  runReq defaultHttpConfig $ do
    r <-
      req
        PATCH
        (root /: "time_entries" /: showId e)
        (ReqBodyJson u)
        jsonResponse
        (mconcat $ headers c)
    pure (responseBody r :: Entry)
