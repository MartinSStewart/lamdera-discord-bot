module Helper exposing
    ( addError
    , authorization
    , createMessage
    , discordMessageDecoder
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
import Types exposing (BackendModel, BackendMsg(..), BotToken(..), ChannelId(..), DiscordMessage, MessageId(..), UserId(..))


messagesUrl =
    let
        (ChannelId channelId_) =
            Environment.channelId
    in
    Environment.discordApiUrl ++ "channels/" ++ channelId_ ++ "/messages"


createMessage : String -> Cmd BackendMsg
createMessage content =
    httpPost
        (JD.succeed ())
        messagesUrl
        (JE.object [ ( "content", JE.string content ) ])
        |> Task.attempt CreatedMessage


getMessagesDecoder : JD.Decoder (List DiscordMessage)
getMessagesDecoder =
    JD.list discordMessageDecoder


getLatestMessage : Task String (List DiscordMessage)
getLatestMessage =
    httpGet
        getMessagesDecoder
        (messagesUrl ++ "?limit=1")


getMessagesAfter : MessageId -> Task String (List DiscordMessage)
getMessagesAfter (MessageId messageId) =
    httpGet
        getMessagesDecoder
        (messagesUrl ++ "?limit=100&after=" ++ messageId)


authorization : Http.Header
authorization =
    let
        (BotToken botToken_) =
            Environment.botToken
    in
    Http.header "Authorization" ("Bot " ++ botToken_)


httpGet : JD.Decoder a -> String -> Task String a
httpGet decoder url =
    Http.task
        { method = "GET"
        , headers = [ authorization ]
        , url = url
        , resolver = Http.stringResolver (resolver decoder)
        , body = Http.emptyBody
        , timeout = Nothing
        }


httpPost : JD.Decoder a -> String -> JE.Value -> Task String a
httpPost decoder url body =
    Http.task
        { method = "POST"
        , headers = [ authorization ]
        , url = url
        , resolver = Http.stringResolver (resolver decoder)
        , body = Http.jsonBody body
        , timeout = Nothing
        }


resolver decoder response =
    case response of
        Http.BadUrl_ badUrl ->
            Err ("Bad url " ++ badUrl)

        Http.Timeout_ ->
            Err "Timeout"

        Http.NetworkError_ ->
            Err "Network error"

        Http.BadStatus_ metadata body ->
            "Bad status " ++ String.fromInt metadata.statusCode ++ "      " ++ body |> Err

        Http.GoodStatus_ _ body ->
            case JD.decodeString decoder body of
                Ok data ->
                    Ok data

                Err error ->
                    JD.errorToString error |> Err


discordMessageDecoder : JD.Decoder DiscordMessage
discordMessageDecoder =
    JD.map5 DiscordMessage
        (JD.field "id" JD.string |> JD.map MessageId)
        (JD.field "content" JD.string)
        (JD.at [ "author", "id" ] JD.string |> JD.map UserId)
        (JD.at [ "author", "username" ] JD.string)
        (JD.oneOf [ JD.field "bot" JD.bool, JD.succeed False ])


addError : String -> { a | errors : List String } -> { a | errors : List String }
addError error model =
    { model | errors = error :: model.errors }


isValidMessage : DiscordMessage -> Bool
isValidMessage message =
    message.authorId /= Environment.botId && not message.isBot
