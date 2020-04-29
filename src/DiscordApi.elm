module DiscordApi exposing (listGuildMembers)

import Environment
import Http
import Iso8601
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE
import Task exposing (Task)
import Time
import Types exposing (BotToken(..))


authorization : Http.Header
authorization =
    let
        (BotToken botToken_) =
            Environment.botToken
    in
    Http.header "Authorization" ("Bot " ++ botToken_)


getUsersUrl : Snowflake GuildId -> String
getUsersUrl (Snowflake guildId) =
    discordApiUrl ++ "/guilds/" ++ guildId ++ "/members"


getUsers : Task String (List Message)
getUsers =
    httpGet
        getUsersDecoder
        getUsersUrl


messagesUrl : Snowflake ChannelId -> String
messagesUrl (Snowflake channelId) =
    discordApiUrl ++ "channels/" ++ channelId ++ "/messages"


createMessage : Snowflake ChannelId -> String -> Task Http.Error ()
createMessage channelId content =
    httpPost
        (JD.succeed ())
        (messagesUrl channelId)
        (JE.object [ ( "content", JE.string content ) ])


getLatestMessage : Snowflake ChannelId -> Task String (List Message)
getLatestMessage =
    httpGet
        (JD.list decodeMessage)
        (messagesUrl ++ "?limit=1")


getMessagesAfter : Snowflake ChannelId -> MessageId -> Task String (List Message)
getMessagesAfter channelId (MessageId messageId) =
    httpGet
        (JD.list decodeMessage)
        (messagesUrl channelId ++ "?limit=100&after=" ++ messageId)


discordApiUrl : String
discordApiUrl =
    "https://discordapp.com/api/"


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


listGuildMembers : GuildId -> Task Http.Error (List User)
listGuildMembers guildId =
    httpGet


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


type OptionalData a
    = Present a
    | Missing


type alias GuildMember =
    { user : OptionalData User
    , nickname : Maybe String
    , roles : List (Snowflake RoleId)
    , joinedAt : Time.Posix
    , premiumSince : OptionalData (Maybe Time.Posix)
    , deaf : Bool
    , mute : Bool
    }


decodeOptionalData : String -> JD.Decoder a -> JD.Decoder (OptionalData a)
decodeOptionalData field decoder =
    JD.oneOf
        [ JD.field field decoder |> JD.map Present
        , JD.field field (JD.fail ("Incorrect data for field: " ++ field))
        , JD.succeed Missing
        ]


type Snowflake idType
    = Snowflake String


type MessageId
    = MessageId Never


type UserId
    = UserId Never


type RoleId
    = RoleId Never


type ChannelId
    = ChannelId Never


type GuildId
    = GuildId Never


type WebhookId
    = WebhookId Never


type alias Message =
    { id : Snowflake MessageId
    , channelId : Snowflake ChannelId
    , guildId : Snowflake GuildId
    , author : User

    -- member field is excluded
    , content : String
    , timestamp : Time.Posix
    , editedTimestamp : Maybe Time.Posix
    , tts : Bool
    , mentionEveryone : Bool

    -- mentions field is excluded
    , mentionRoles : List (Snowflake RoleId)

    -- mention_channels field is excluded
    , attachments : List Attachment

    -- embeds field is excluded
    , reactions : OptionalData (List Reaction)

    -- nonce field is excluded
    , pinned : Bool
    , webhookId : Snowflake WebhookId
    , type_ : Int

    -- activity field is excluded
    -- application field is excluded
    -- message_reference field is excluded
    , flags : OptionalData Int
    }


type AttachmentId
    = AttachmentId Never


decodeSnowflake : JD.Decoder (Snowflake idType)
decodeSnowflake =
    JD.map Snowflake JD.string


decodeMessage : JD.Decoder Message
decodeMessage =
    JD.succeed Message
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "channel_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "guild_id" decodeSnowflake)
        |> JD.andMap (JD.field "author" decodeUser)
        |> JD.andMap (JD.field "content" JD.string)
        |> JD.andMap (JD.field "timestamp" Iso8601.decoder)
        |> JD.andMap (JD.field "edited_timestamp" (JD.nullable Iso8601.decoder))
        |> JD.andMap (JD.field "tts" JD.bool)
        |> JD.andMap (JD.field "mention_everyone" JD.bool)
        |> JD.andMap (JD.field "mention_roles" (JD.list decodeSnowflake))
        |> JD.andMap (JD.field "attachments" (JD.list decodeAttachment))
        |> JD.andMap (decodeOptionalData "reactions" (JD.list decodeReaction))
        |> JD.andMap (JD.field "pinned" JD.bool)
        |> JD.andMap (JD.field "webhook_id" decodeSnowflake)
        |> JD.andMap (JD.field "type" JD.int)
        |> JD.andMap (decodeOptionalData "flags" JD.int)


type alias User =
    { id : Snowflake UserId
    , username : String
    , discriminator : Int
    , avatar : Maybe String
    , bot : OptionalData Bool
    , system : OptionalData Bool
    , mfaEnabled : OptionalData Bool
    , locale : OptionalData String
    , verified : OptionalData Bool
    , email : OptionalData (Maybe String)
    , flags : OptionalData Int
    , premiumType : OptionalData Int
    , publicFlags : OptionalData Int
    }


decodeUser : JD.Decoder User
decodeUser =
    JD.succeed User
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "username" JD.string)
        |> JD.andMap
            (JD.field "discriminator" JD.string
                |> JD.andThen
                    (\text ->
                        case String.toFloat text of
                            Just value ->
                                JD.succeed value

                            Nothing ->
                                JD.fail "Invalid discriminator"
                    )
            )
        |> JD.andMap (JD.field "avatar" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "bot" JD.bool)
        |> JD.andMap (decodeOptionalData "system" JD.bool)
        |> JD.andMap (decodeOptionalData "mfaEnabled" JD.bool)
        |> JD.andMap (decodeOptionalData "locale" JD.string)
        |> JD.andMap (decodeOptionalData "verified" JD.bool)
        |> JD.andMap (decodeOptionalData "email" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "flags" JD.int)
        |> JD.andMap (decodeOptionalData "premiumType" JD.int)
        |> JD.andMap (decodeOptionalData "publicFlags" JD.int)


type alias Attachment =
    { id : Snowflake AttachmentId
    , filename : String
    , size : Int
    , url : String
    , proxyUrl : String
    , height : Maybe Int
    , width : Maybe Int
    }


decodeAttachment : JD.Decoder Attachment
decodeAttachment =
    JD.succeed Attachment
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "filename" JD.string)
        |> JD.andMap (JD.field "size" JD.int)
        |> JD.andMap (decodeOptionalData "url" JD.string)
        |> JD.andMap (decodeOptionalData "proxyUrl" JD.string)
        |> JD.andMap (decodeOptionalData "height" (JD.nullable JD.int))
        |> JD.andMap (decodeOptionalData "width" (JD.nullable JD.int))


type alias Reaction =
    { count : Int
    , me : Bool
    , emoji : Emoji
    }


type EmojiId
    = EmojiId Never


type alias Emoji =
    { id : Snowflake EmojiId
    , name : Maybe String
    , roles : List (Snowflake RoleId)
    , user : OptionalData User
    , requireColors : OptionalData Bool
    , managed : OptionalData Bool
    , animated : OptionalData Bool
    , available : OptionalData Bool
    }


decodeReaction : JD.Decoder Reaction
decodeReaction =
    JD.succeed Attachment
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "filename" JD.string)
        |> JD.andMap (JD.field "size" JD.int)
        |> JD.andMap (decodeOptionalData "url" JD.string)
        |> JD.andMap (decodeOptionalData "proxyUrl" JD.string)
        |> JD.andMap (decodeOptionalData "height" (JD.nullable JD.int))
        |> JD.andMap (decodeOptionalData "width" (JD.nullable JD.int))
