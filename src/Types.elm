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
import DiscordApi exposing (GuildId, GuildMember, Id, Message, MessageId, User)
import Set exposing (Set)
import Time exposing (Month)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key }


type alias BackendModel =
    { errors : List String
    , lastDiscordBadStatus : Maybe Time.Posix
    , lastMessageId : Maybe (DiscordApi.Id MessageId)
    , users : Maybe (List (Id DiscordApi.UserId))
    , botUserId : Maybe (Id DiscordApi.UserId)
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOp
    | CreatedMessage (Result String ())
    | CreatedReaction (Result String ())
    | GotMessages (Maybe (DiscordApi.Id MessageId)) (Result String (List Message)) Time.Posix
    | UpdateLoop Time.Posix
    | GotUsers (Result String (List GuildMember)) Time.Posix
    | GotBotUser (Result String User)


type ToFrontend
    = NoOpToFrontend
