module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , FrontendModel
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import DiscordApi exposing (Id, Message, MessageId, User, UserId)
import Set exposing (Set)
import Time exposing (Month)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key }


type alias BackendModel =
    { errors : List String
    , lastDiscordBadStatus : Maybe Time.Posix
    , lastMessageId : Maybe (DiscordApi.Id MessageId)
    , lastGetUsersId : Int
    , users : Maybe (Set UserId)
    , botUserId : Maybe (Id UserId)
    }


type alias UserId =
    String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOp
    | CreatedMessage (Result String ())
    | GotMessages (Maybe (DiscordApi.Id MessageId)) (Result String (List Message)) Time.Posix
    | UpdateLoop Time.Posix
    | GotUsers Int (Result String (List User)) Time.Posix
    | GotBotUser (Result String User)


type ToFrontend
    = NoOpToFrontend
