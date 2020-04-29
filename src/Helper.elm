module Helper exposing
    ( addError
    , authorization
    , createMessage
    , getLatestMessage
    , getMessagesAfter
    , getMessagesDecoder
    , httpGet
    , httpPost
    , isValidMessage
    , resolver
    )

import Environment
import Http
import Json.Decode as JD
import Json.Encode as JE
import Task exposing (Task)
import Time
import Types exposing (BackendModel, BackendMsg(..), BotToken(..), ChannelId(..), DiscordMessage, GuildId(..), MessageId(..), UserId(..))


addError : String -> { a | errors : List String } -> { a | errors : List String }
addError error model =
    { model | errors = error :: model.errors }


isValidMessage : DiscordMessage -> Bool
isValidMessage message =
    message.authorId /= Environment.botId && not message.isBot
