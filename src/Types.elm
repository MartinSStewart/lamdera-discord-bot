module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BotToken(..)
    , FrontendModel
    , FrontendMsg(..)
    , ToBackend(..)
    , ToFrontend(..)
    )

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Time exposing (Month)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key }


type alias BackendModel =
    { errors : List String
    , lastDiscordBadStatus : Maybe Time.Posix
    , lastMessageId : Maybe MessageId
    }


type BotToken
    = BotToken String


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOp
    | CreatedMessage (Result String ())
    | GotMessages (Maybe MessageId) (Result String (List DiscordMessage))
    | GotMessagesWithTime (Maybe MessageId) (Result String (List DiscordMessage)) Time.Posix
    | CheckForNewMessagesAndUsers Time.Posix


type ToFrontend
    = NoOpToFrontend
