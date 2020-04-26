module Types exposing
    ( BackendModel
    , BackendMsg(..)
    , BotToken(..)
    , ChannelId(..)
    , DiscordMessage
    , FrontendModel
    , FrontendMsg(..)
    , MessageId(..)
    , ToBackend(..)
    , ToFrontend(..)
    , UserId(..)
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


type MessageId
    = MessageId String


type UserId
    = UserId String


type BotToken
    = BotToken String


type ChannelId
    = ChannelId String


type alias DiscordMessage =
    { id : MessageId
    , content : String
    , authorId : UserId
    , authorName : String
    , isBot : Bool
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
    | GotMessages (Maybe MessageId) (Result String (List DiscordMessage))
    | GotMessagesWithTime (Maybe MessageId) (Result String (List DiscordMessage)) Time.Posix
    | CheckForNewMessages Time.Posix


type ToFrontend
    = NoOpToFrontend
