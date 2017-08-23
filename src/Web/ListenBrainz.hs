{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Web.ListenBrainz
  ( MonadListenBrainz(..)
  , runListenBrainzEff
  , runListenBrainzT

    -- * API Calls
    -- ** Submitting Listens
  , submitListens
  , SubmitListens(..)
  , ListenData(..)
  , TrackMetadata(..)

    -- ** Retrieving Listens
  , Listens(..)
  , getListens

    -- ** Retrieving Import Timestamps
  , LatestImport(..)
  , getLatestImport

    -- ** Updating Import Timestamps
  , updateLatestImport

    -- * Types
  , UserToken(..)

    -- * Implementation Details
  , ListenBrainzAPICall(..)
  , ListenBrainzT(..)
  , performAPICall

    -- ** Servant API
  , ListenBrainzAPI
  , Authorization(..)
  , TimeStamp(..)
  , UserName(..)
  ) where

import Control.Monad
import qualified Control.Monad.Except as MTL
import Control.Monad.Free
import Control.Monad.Freer
import Control.Monad.Freer.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson (FromJSON(..), ToJSON(..), (.=), (.:))
import qualified Data.Aeson as JSON
import Data.Functor.Coyoneda
import Data.Monoid
import Data.Proxy (Proxy(..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.Generics
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Client

-- | A submission of listens to ListenBrainz.
--
-- The ListenBrainz documentation states:
--
-- Listens should be submitted for tracks when the user has listened to half
-- the track or 4 minutes of the track, whichever is lower. If the user hasn’t
-- listened to 4 minutes or half the track, it doesn’t fully count as a listen
-- and should not be submitted.
data SubmitListens
  = SingleListen ListenData
    -- ^ Submit a single track.
  | NowPlaying TrackMetadata
    -- ^ Submit now playing information.
  | Import [ListenData]
    -- ^ Submit a batch of listens as an import.


instance ToJSON SubmitListens where
  toJSON l =
    case l of
      SingleListen l' ->
        JSON.object
          [ "listen_type" .= JSON.String "single"
          , "payload" .= [ l' ]
          ]

      NowPlaying track ->
        JSON.object
          [ "listen_type" .= JSON.String "playing_now"
          , "payload" .= [ JSON.object [ "track_metadata" .= track ] ]
          ]

      Import listens ->
        JSON.object
          [ "listen_type" .= JSON.String "import"
          , "payload" .= listens
          ]


data ListenData = ListenData
  { listenListenedAt :: UTCTime
  , listenTrackMetadata :: TrackMetadata
  } deriving (Eq, Ord, Read, Show, Generic)


instance ToJSON ListenData where
  toJSON ListenData {..} =
    JSON.object
      [ "listened_at" .= utcTimeToPOSIXSeconds listenListenedAt
      , "track_metadata" .= listenTrackMetadata
      ]


instance FromJSON ListenData where
  parseJSON = JSON.withObject "ListenData" $ \o ->
    do
      listenListenedAt <-
        fmap posixSecondsToUTCTime (o .: "listened_at")

      listenTrackMetadata <-
        o .: "track_metadata"

      pure ListenData{..}


data TrackMetadata = TrackMetadata
  { trackArtist :: Text
  , trackName :: Text
  } deriving (Eq, Ord, Read, Show, Generic)


instance ToJSON TrackMetadata where
  toJSON TrackMetadata {..} =
    JSON.object
      [ "artist_name" .= trackArtist
      , "track_name" .= trackName
      ]


instance FromJSON TrackMetadata where
  parseJSON = JSON.withObject "TrackMetadata" $ \o ->
    do
      trackArtist <-
        o .: "artist_name"

      trackName <-
        o .: "track_name"

      pure TrackMetadata{..}


newtype Authorization = Authorization Text


instance ToHttpApiData Authorization where
  toUrlPiece (Authorization a) = a


newtype UserName = UserName Text


instance ToHttpApiData UserName where
  toUrlPiece (UserName u) = u


newtype TimeStamp = TimeStamp UTCTime


instance ToJSON TimeStamp where
  toJSON (TimeStamp t) =
    JSON.object [ "ts" .= utcTimeToPOSIXSeconds t ]


data LatestImport = LatestImport
  { latestImportUserId :: Text
  , latestImportTimestamp :: UTCTime
  } deriving (Eq, Ord, Read, Show, Generic)


instance FromJSON LatestImport where
  parseJSON = JSON.withObject "LatestImport" $ \o ->
    do
      latestImportUserId <-
        o .: "musicbrainz_id"

      latestImportTimestamp <-
        fmap posixSecondsToUTCTime (o .: "latest_import")

      pure LatestImport {..}


type ListenBrainzAPI =
       Header "Authorization" Authorization :> "1" :> "submit-listens" :> ReqBody '[JSON] SubmitListens :> Post '[ JSON ] JSON.Value
  :<|> "1" :> "latest-import" :> QueryParam "user_name" UserName :> Get '[ JSON ] LatestImport
  :<|> Header "Authorization" Authorization :> "1" :> "latest-import" :> ReqBody '[ JSON ] TimeStamp :> Post '[ JSON ] JSON.Value
  :<|> "1" :> "user" :> Capture "user-name" Text :> "listens" :> QueryParam "max_ts" Int :> QueryParam "min_ts" Int :> QueryParam "count" Int :> Get '[ JSON ] Listens


-- | Possible API calls, tagged with their successful return value.
data ListenBrainzAPICall a where
  SubmitListens :: UserToken -> SubmitListens -> ListenBrainzAPICall JSON.Value
  GetLatestImport :: Text -> ListenBrainzAPICall LatestImport
  UpdateLatestImport :: UserToken -> UTCTime -> ListenBrainzAPICall JSON.Value
  GetListens :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> ListenBrainzAPICall [ListenData]


performAPICall :: Manager -> ListenBrainzAPICall a -> IO (Either ServantError a)
performAPICall m call =
  runClientM
    (case call of
      SubmitListens userToken listen ->
        apiPostSubmitListen
          (Just (tokenToAuthHeader userToken))
          listen

      GetLatestImport userName ->
        apiGetLatestImport
          (Just (UserName userName))

      UpdateLatestImport userToken ts ->
        apiPostLatestImport
          (Just (tokenToAuthHeader userToken))
          (TimeStamp ts)

      GetListens userName maxTs minTs count ->
        fmap (\(Listens l) -> l)
          (apiGetListens
            userName
            maxTs
            minTs
            count))

    (ClientEnv m baseUrl)

  where
    apiPostSubmitListen :<|> apiGetLatestImport :<|> apiPostLatestImport :<|> apiGetListens =
      client (Proxy @ListenBrainzAPI)

    tokenToAuthHeader (UserToken userToken) =
      Authorization $ "Token " <> userToken


-- | The class of monads that can embed calls to the ListenBrainz API.
class Monad m => MonadListenBrainz m where
  liftListenBrainz :: Free (Coyoneda ListenBrainzAPICall) a -> m a


{-# INLINEABLE submitListens #-}
submitListens
  :: MonadListenBrainz m
  => UserToken -- ^ The private token of the user to submit a listen for.
  -> SubmitListens
  -> m JSON.Value
submitListens userKey listen =
  liftListenBrainz (liftF (liftCoyoneda (SubmitListens userKey listen)))


getLatestImport
  :: MonadListenBrainz m
  => Text -- ^ The user name (not user token) of the user to find the latest import timestamp
  -> m LatestImport
getLatestImport userName =
  liftListenBrainz (liftF (liftCoyoneda (GetLatestImport userName)))


updateLatestImport
  :: MonadListenBrainz m
  => UserToken
  -> UTCTime
  -> m JSON.Value
updateLatestImport userName utcTime =
  liftListenBrainz (liftF (liftCoyoneda (UpdateLatestImport userName utcTime)))


getListens
  :: MonadListenBrainz m
  => Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> m [ListenData]
getListens userName maxTs minTs count =
  liftListenBrainz (liftF (liftCoyoneda (GetListens userName maxTs minTs count)))


newtype Listens = Listens [ListenData]


instance FromJSON Listens where
  parseJSON = JSON.withObject "Listens" $ \o ->
    do
      payload <-
        o .: "payload"

      fmap Listens (payload .: "listens")


-- An implementation for freer-effects. Re-interpret a sequence of API calls
-- 'send's for those calls.
instance Member ListenBrainzAPICall effs => MonadListenBrainz (Eff effs) where
  liftListenBrainz f = iterM (join . lowerCoyoneda . hoistCoyoneda send) f


-- | Eliminate 'ListenBrainzAPICall' effects by performing HTTP calls.
--
-- This implementation uses extensible effects as provided by @freer-effects@.
runListenBrainzEff
  :: (Member IO effs, Member (Exc ServantError) effs)
  => Manager -> Eff (ListenBrainzAPICall ': effs) a -> Eff effs a
runListenBrainzEff m =
  handleRelay
    return
    (\op next ->
       do
         res <-
           send (performAPICall m op)

         case res of
           Left e ->
             throwError e

           Right ok ->
             next ok)


-- | A monad transformer to interpret ListenBrainz API calls as HTTP calls.
newtype ListenBrainzT m a = ListenBrainzT (ReaderT Manager m a)
  deriving (Monad, Applicative, Functor, MonadIO, MTL.MonadTrans)


-- | Run ListenBrainz calls using a traditional monad transformer stack.
runListenBrainzT :: ListenBrainzT m a -> Manager -> m a
runListenBrainzT (ListenBrainzT (ReaderT m)) = m


instance (MonadIO m, MTL.MonadError ServantError m) => MonadListenBrainz (ListenBrainzT m) where
  liftListenBrainz =
    iterM (join . lowerCoyoneda . hoistCoyoneda go)

    where
      go :: (MonadIO m, MTL.MonadError ServantError m) => ListenBrainzAPICall a -> ListenBrainzT m a
      go apiCall =
        do
          m <-
            ListenBrainzT ask

          res <-
            liftIO
              (performAPICall m apiCall)

          case res of
            Left e ->
              MTL.lift (MTL.throwError e)

            Right ok ->
              return ok


baseUrl :: BaseUrl
baseUrl =
  BaseUrl
  { baseUrlScheme = Https
  , baseUrlHost = "listenbrainz.org"
  , baseUrlPort = 443
  , baseUrlPath = ""
  }


newtype UserToken = UserToken Text
  deriving (IsString)
