module DiscordApi exposing
    ( Attachment
    , AttachmentId
    , BotToken
    , ChannelId
    , Emoji
    , EmojiId
    , GuildId
    , Id(..)
    , Message
    , MessageId
    , OptionalData(..)
    , Reaction
    , RoleId
    , User
    , UserId
    , WebhookId
    , botToken
    , createMessage
    , createReaction
    , getCurrentUser
    , getLatestMessage
    , getMessagesAfter
    , getUsers
    )

import Http
import Iso8601
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE
import Task exposing (Task)
import Time
import Url.Builder exposing (QueryParameter)


type BotToken
    = BotToken String


botToken : String -> BotToken
botToken botTokenText =
    BotToken botTokenText


authorization : BotToken -> Http.Header
authorization (BotToken botToken_) =
    Http.header "Authorization" ("Bot " ++ botToken_)


getUsersUrl : Id GuildId -> String
getUsersUrl (Id guildId) =
    discordApiUrl ++ Url.Builder.absolute [ "guilds", guildId, "members" ] []


getUsers : BotToken -> Id GuildId -> Task String (List User)
getUsers botToken_ guildId =
    httpGet
        botToken_
        (JD.list decodeUser)
        (getUsersUrl guildId)


messagesUrl : Id ChannelId -> List QueryParameter -> String
messagesUrl (Id channelId) query =
    discordApiUrl
        ++ Url.Builder.absolute [ "channels", channelId, "messages" ] query


createMessage : BotToken -> Id ChannelId -> String -> Task String ()
createMessage botToken_ channelId content =
    httpPost
        botToken_
        (JD.succeed ())
        (messagesUrl channelId [])
        (JE.object [ ( "content", JE.string content ) ])


getLatestMessage : BotToken -> Id ChannelId -> Task String (List Message)
getLatestMessage botToken_ channelId =
    httpGet
        botToken_
        (JD.list decodeMessage)
        (messagesUrl channelId [ Url.Builder.int "limit" 1 ])


getMessagesAfter : BotToken -> Id ChannelId -> Id MessageId -> Task String (List Message)
getMessagesAfter botToken_ channelId (Id messageId) =
    httpGet
        botToken_
        (JD.list decodeMessage)
        (messagesUrl channelId [ Url.Builder.int "limit" 100, Url.Builder.string "after" messageId ])


createReactionUrl : Id ChannelId -> Id MessageId -> String -> Id EmojiId -> String
createReactionUrl (Id channelId) (Id messageId) emojiName (Id emojiId) =
    discordApiUrl
        ++ Url.Builder.absolute
            [ "channels", channelId, "messages", messageId, "reactions", emojiName ++ ":" ++ emojiId, "@me" ]
            []


createReaction : BotToken -> Id ChannelId -> Id MessageId -> String -> Id EmojiId -> Task String ()
createReaction botToken_ channelId messageId emojiName emojiId =
    httpPut botToken_
        (JD.succeed ())
        (createReactionUrl channelId messageId emojiName emojiId)
        (JE.object [])


getCurrentUser : BotToken -> Task String User
getCurrentUser botToken_ =
    httpGet
        botToken_
        decodeUser
        (discordApiUrl ++ Url.Builder.absolute [ "users", "@me" ] [])


discordApiUrl : String
discordApiUrl =
    "https://discordapp.com/api/"


httpPost : BotToken -> JD.Decoder a -> String -> JE.Value -> Task String a
httpPost botToken_ decoder url body =
    Http.task
        { method = "POST"
        , headers = [ authorization botToken_ ]
        , url = url
        , resolver = Http.stringResolver (resolver decoder)
        , body = Http.jsonBody body
        , timeout = Nothing
        }


httpPut : BotToken -> JD.Decoder a -> String -> JE.Value -> Task String a
httpPut botToken_ decoder url body =
    Http.task
        { method = "PUT"
        , headers = [ authorization botToken_ ]
        , url = url
        , resolver = Http.stringResolver (resolver decoder)
        , body = Http.jsonBody body
        , timeout = Nothing
        }


httpGet : BotToken -> JD.Decoder a -> String -> Task String a
httpGet botToken_ decoder url =
    Http.task
        { method = "GET"
        , headers = [ authorization botToken_ ]
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
    = Included a
    | Missing


type alias GuildMember =
    { user : OptionalData User
    , nickname : Maybe String
    , roles : List (Id RoleId)
    , joinedAt : Time.Posix
    , premiumSince : OptionalData (Maybe Time.Posix)
    , deaf : Bool
    , mute : Bool
    }


decodeOptionalData : String -> JD.Decoder a -> JD.Decoder (OptionalData a)
decodeOptionalData field decoder =
    JD.oneOf
        [ JD.field field decoder |> JD.map Included
        , JD.field field (JD.fail ("Incorrect data for field: " ++ field))
        , JD.succeed Missing
        ]


{-| In Discord's documentation these are called snowflakes. Id is much quicker to write though.
-}
type Id idType
    = Id String


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


type AttachmentId
    = AttachmentId Never


type EmojiId
    = EmojiId Never


type alias Message =
    { id : Id MessageId
    , channelId : Id ChannelId
    , guildId : OptionalData (Id GuildId)
    , author : User

    -- member field is excluded
    , content : String
    , timestamp : Time.Posix
    , editedTimestamp : Maybe Time.Posix
    , textToSpeech : Bool
    , mentionEveryone : Bool

    -- mentions field is excluded
    , mentionRoles : List (Id RoleId)

    -- mention_channels field is excluded
    , attachments : List Attachment

    -- embeds field is excluded
    , reactions : OptionalData (List Reaction)

    -- nonce field is excluded
    , pinned : Bool
    , webhookId : OptionalData (Id WebhookId)
    , type_ : Int

    -- activity field is excluded
    -- application field is excluded
    -- message_reference field is excluded
    , flags : OptionalData Int
    }


decodeSnowflake : JD.Decoder (Id idType)
decodeSnowflake =
    JD.map Id JD.string


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
        |> JD.andMap (decodeOptionalData "webhook_id" decodeSnowflake)
        |> JD.andMap (JD.field "type" JD.int)
        |> JD.andMap (decodeOptionalData "flags" JD.int)


type alias User =
    { id : Id UserId
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
                        case String.toInt text of
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
    { id : Id AttachmentId
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
        |> JD.andMap (JD.field "url" JD.string)
        |> JD.andMap (JD.field "proxyUrl" JD.string)
        |> JD.andMap (JD.field "height" (JD.nullable JD.int))
        |> JD.andMap (JD.field "width" (JD.nullable JD.int))


type alias Reaction =
    { count : Int
    , me : Bool
    , emoji : Emoji
    }


type alias Emoji =
    { id : Id EmojiId
    , name : Maybe String
    , roles : List (Id RoleId)
    , user : OptionalData User
    , requireColors : OptionalData Bool
    , managed : OptionalData Bool
    , animated : OptionalData Bool
    , available : OptionalData Bool
    }


decodeReaction : JD.Decoder Reaction
decodeReaction =
    JD.succeed Reaction
        |> JD.andMap (JD.field "count" JD.int)
        |> JD.andMap (JD.field "me" JD.bool)
        |> JD.andMap (JD.field "emoji" decodeEmoji)


decodeEmoji : JD.Decoder Emoji
decodeEmoji =
    JD.succeed Emoji
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" (JD.nullable JD.string))
        |> JD.andMap (JD.field "roles" (JD.list decodeSnowflake))
        |> JD.andMap (decodeOptionalData "user" decodeUser)
        |> JD.andMap (decodeOptionalData "requireColors" JD.bool)
        |> JD.andMap (decodeOptionalData "managed" JD.bool)
        |> JD.andMap (decodeOptionalData "animated" JD.bool)
        |> JD.andMap (decodeOptionalData "available" JD.bool)
