module DiscordApi exposing
    ( Attachment
    , AttachmentId
    , BotToken
    , Channel
    , ChannelId
    , Emoji
    , EmojiId
    , Guild
    , GuildId
    , GuildMember
    , Id(..)
    , Message
    , MessageId
    , OptionalData(..)
    , PartialGuild
    , Permissions
    , Reaction
    , RoleId
    , User
    , UserId
    , WebhookId
    , botToken
    , createMessage
    , createReaction
    , getChannel
    , getCurrentUser
    , getCurrentUserGuilds
    , getLatestMessage
    , getMessagesAfter
    , getUsers
    )

{-| The beginnings of an Elm package...

Useful Discord links:

  - API documentation: <https://discordapp.com/developers/docs/intro>
  - Create bot invites: <https://discordapi.com/permissions.html>

-}

import Binary
import Duration exposing (Seconds)
import Http
import Iso8601
import Json.Decode as JD
import Json.Decode.Extra as JD
import Json.Encode as JE
import Quantity exposing (Quantity(..), Rate)
import Task exposing (Task)
import Time exposing (Posix(..))
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
    Url.Builder.crossOrigin discordApiUrl [ "guilds", guildId, "members" ] []


getUsers : BotToken -> Id GuildId -> Task String (List GuildMember)
getUsers botToken_ guildId =
    httpGet
        botToken_
        (JD.list decodeGuildMember)
        (getUsersUrl guildId)


messagesUrl : Id ChannelId -> List QueryParameter -> String
messagesUrl (Id channelId) query =
    Url.Builder.crossOrigin discordApiUrl [ "channels", channelId, "messages" ] query


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


createReactionUrl : Id ChannelId -> Id MessageId -> String -> String
createReactionUrl (Id channelId) (Id messageId) emoji =
    Url.Builder.crossOrigin
        discordApiUrl
        [ "channels", channelId, "messages", messageId, "reactions", emoji, "@me" ]
        []


createReaction : BotToken -> Id ChannelId -> Id MessageId -> String -> Task String ()
createReaction botToken_ channelId messageId emoji =
    httpPut botToken_
        (JD.succeed ())
        (createReactionUrl channelId messageId emoji)
        (JE.object [])


getCurrentUser : BotToken -> Task String User
getCurrentUser botToken_ =
    httpGet
        botToken_
        decodeUser
        (Url.Builder.crossOrigin discordApiUrl [ "users", "@me" ] [])


getCurrentUserGuilds : BotToken -> Task String (List PartialGuild)
getCurrentUserGuilds botToken_ =
    httpGet
        botToken_
        (JD.list decodePartialGuild)
        (Url.Builder.crossOrigin discordApiUrl [ "users", "@me", "guilds" ] [])


getChannel : BotToken -> Id ChannelId -> Task String Channel
getChannel botToken_ (Id channelId) =
    httpGet botToken_ decodeChannel (Url.Builder.crossOrigin discordApiUrl [ "channels", channelId ] [])


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
            case JD.decodeString decoder (Debug.log "" body) of
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


decodeGuildMember : JD.Decoder GuildMember
decodeGuildMember =
    JD.succeed GuildMember
        |> JD.andMap (decodeOptionalData "user" decodeUser)
        |> JD.andMap (JD.field "nick" (JD.nullable JD.string))
        |> JD.andMap (JD.field "roles" (JD.list decodeSnowflake))
        |> JD.andMap (JD.field "joined_at" Iso8601.decoder)
        |> JD.andMap (decodeOptionalData "premium_since" (JD.nullable Iso8601.decoder))
        |> JD.andMap (JD.field "deaf" JD.bool)
        |> JD.andMap (JD.field "mute" JD.bool)


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


type ApplicationId
    = ApplicationId Never


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


type alias PartialGuild =
    { id : Id GuildId
    , name : String
    , icon : Maybe String
    , owner : Bool
    , permissions : Permissions
    }


decodePartialGuild : JD.Decoder PartialGuild
decodePartialGuild =
    JD.succeed PartialGuild
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" JD.string)
        |> JD.andMap (JD.field "icon" (JD.nullable JD.string))
        |> JD.andMap (JD.field "owner" JD.bool)
        |> JD.andMap (JD.field "permissions" decodePermissions)


type alias Guild =
    { id : Id GuildId
    , name : String
    , icon : Maybe String
    , splash : Maybe String
    , discoverySplash : Maybe String
    , owner : OptionalData Bool
    , ownerId : Id UserId
    , permissions : OptionalData Permissions
    , region : String
    , afkChannelId : Maybe (Id ChannelId)
    , afkTimeout : Quantity Int Seconds
    , embedEnabled : OptionalData Bool
    , embedChannelId : OptionalData (Maybe (Id ChannelId))
    , verificationLevel : Int
    , defaultMessageNotifications : Int
    , explicitContentFilter : Int

    -- roles field excluded
    , emojis : List Emoji
    , features : List String
    , mfaLevel : Int
    , applicationId : Maybe (Id ApplicationId)
    , widgetEnabled : OptionalData Bool
    , widgetChannelId : OptionalData (Maybe (Id ChannelId))
    , systemChannelId : Maybe (Id ChannelId)
    , systemChannelFlags : Int
    , rulesChannelId : Maybe (Id ChannelId)
    , joinedAt : OptionalData Time.Posix
    , large : OptionalData Bool
    , unavailable : OptionalData Bool
    , memberCount : OptionalData Int

    -- voiceStates field excluded
    , members : OptionalData (List GuildMember)
    , channels : OptionalData (List Channel)

    -- presences field excluded
    , maxPresences : OptionalData (Maybe Int)
    , maxMembers : OptionalData Int
    , vanityUrlCode : Maybe String
    , description : Maybe String
    , banner : Maybe String
    , premiumTier : Int
    , premiumSubscriptionCount : OptionalData Int
    , preferredLocale : String
    , publicUpdatesChannelId : Maybe (Id ChannelId)
    , approximateMemberCount : OptionalData Int
    , approximatePresenceCount : OptionalData Int
    }


decodeGuild : JD.Decoder Guild
decodeGuild =
    JD.succeed Guild
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "name" JD.string)
        |> JD.andMap (JD.field "icon" (JD.nullable JD.string))
        |> JD.andMap (JD.field "splash" (JD.nullable JD.string))
        |> JD.andMap (JD.field "discovery_splash" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "owner" JD.bool)
        |> JD.andMap (JD.field "owner_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "permissions" decodePermissions)
        |> JD.andMap (JD.field "region" JD.string)
        |> JD.andMap (JD.field "afk_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "afk_timeout" (JD.map Quantity JD.int))
        |> JD.andMap (decodeOptionalData "embed_enabled" JD.bool)
        |> JD.andMap (decodeOptionalData "embed_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "verification_level" JD.int)
        |> JD.andMap (JD.field "default_message_notifications" JD.int)
        |> JD.andMap (JD.field "explicit_content_filter" JD.int)
        |> JD.andMap (JD.field "emojis" (JD.list decodeEmoji))
        |> JD.andMap (JD.field "features" (JD.list JD.string))
        |> JD.andMap (JD.field "mfa_level" JD.int)
        |> JD.andMap (JD.field "application_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "widget_enabled" JD.bool)
        |> JD.andMap (decodeOptionalData "widget_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "system_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (JD.field "system_channel_flags" JD.int)
        |> JD.andMap (JD.field "rules_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "joined_at" Iso8601.decoder)
        |> JD.andMap (decodeOptionalData "large" JD.bool)
        |> JD.andMap (decodeOptionalData "unavailable" JD.bool)
        |> JD.andMap (decodeOptionalData "member_count" JD.int)
        |> JD.andMap (decodeOptionalData "members" (JD.list decodeGuildMember))
        |> JD.andMap (decodeOptionalData "channels" (JD.list decodeChannel))
        |> JD.andMap (decodeOptionalData "max_presences" (JD.nullable JD.int))
        |> JD.andMap (decodeOptionalData "max_members" JD.int)
        |> JD.andMap (JD.field "vanity_url_code" (JD.nullable JD.string))
        |> JD.andMap (JD.field "description" (JD.nullable JD.string))
        |> JD.andMap (JD.field "banner" (JD.nullable JD.string))
        |> JD.andMap (JD.field "premium_tier" JD.int)
        |> JD.andMap (decodeOptionalData "premium_subscription_count" JD.int)
        |> JD.andMap (JD.field "preferred_locale" JD.string)
        |> JD.andMap (JD.field "public_updates_channel_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "approximate_member_count" JD.int)
        |> JD.andMap (decodeOptionalData "approximate_presence_count" JD.int)


type alias Permissions =
    { createInstantInvite : Bool
    , kickMembers : Bool
    , banMembers : Bool
    , administrator : Bool
    , manageChannels : Bool
    , manageGuild : Bool
    , addReaction : Bool
    , viewAuditLog : Bool
    , prioritySpeaker : Bool
    , stream : Bool
    , viewChannel : Bool
    , sendMessages : Bool
    , sentTextToSpeechMessages : Bool
    , manageMessages : Bool
    , embedLinks : Bool
    , attachFiles : Bool
    , readMessageHistory : Bool
    , mentionEveryone : Bool
    , useExternalEmojis : Bool
    , viewGuildInsights : Bool
    , connect : Bool
    , speak : Bool
    , muteMembers : Bool
    , deafenMembers : Bool
    , moveMembers : Bool
    , useVoiceActivityDetection : Bool
    , changeNickname : Bool
    , manageNicknames : Bool
    , manageRoles : Bool
    , manageWebhooks : Bool
    , manageEmojis : Bool
    }


decodePermissions : JD.Decoder Permissions
decodePermissions =
    JD.map
        (\value ->
            let
                permissions =
                    value |> List.singleton |> Binary.fromIntegers |> Binary.toBooleans

                getPermission position =
                    List.drop position permissions |> List.head |> Maybe.withDefault False
            in
            Permissions
                (getPermission 0)
                (getPermission 1)
                (getPermission 2)
                (getPermission 3)
                (getPermission 4)
                (getPermission 5)
                (getPermission 6)
                (getPermission 7)
                (getPermission 8)
                (getPermission 9)
                (getPermission 10)
                (getPermission 11)
                (getPermission 12)
                (getPermission 13)
                (getPermission 14)
                (getPermission 15)
                (getPermission 16)
                (getPermission 17)
                (getPermission 18)
                (getPermission 19)
                (getPermission 20)
                (getPermission 21)
                (getPermission 22)
                (getPermission 23)
                (getPermission 24)
                (getPermission 25)
                (getPermission 26)
                (getPermission 27)
                (getPermission 28)
                (getPermission 29)
                (getPermission 30)
        )
        JD.int


type Bits
    = Bits Never


type alias Channel =
    { id : Id ChannelId
    , type_ : ChannelTypes
    , guildId : OptionalData (Id GuildId)
    , position : OptionalData Int

    -- premission overwrites field excluded
    , name : OptionalData String
    , topic : OptionalData (Maybe String)
    , nsfw : OptionalData Bool
    , lastMessageId : OptionalData (Maybe (Id MessageId))
    , bitrate : OptionalData (Quantity Int (Rate Bits Seconds))
    , userLimit : OptionalData Int
    , rateLimitPerUser : OptionalData (Quantity Int Seconds)
    , recipients : OptionalData (List User)
    , icon : OptionalData (Maybe String)
    , ownerId : OptionalData (Id UserId)
    , applicationId : OptionalData (Id ApplicationId)
    , parentId : OptionalData (Maybe (Id ChannelId))
    , lastPinTimestamp : OptionalData Time.Posix
    }


decodeChannel : JD.Decoder Channel
decodeChannel =
    JD.succeed Channel
        |> JD.andMap (JD.field "id" decodeSnowflake)
        |> JD.andMap (JD.field "type" decodeChannelTypes)
        |> JD.andMap (decodeOptionalData "guild_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "position" JD.int)
        |> JD.andMap (decodeOptionalData "name" JD.string)
        |> JD.andMap (decodeOptionalData "topic" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "nsfw" JD.bool)
        |> JD.andMap (decodeOptionalData "last_message_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "bitrate" (JD.map Quantity JD.int))
        |> JD.andMap (decodeOptionalData "user_limit" JD.int)
        |> JD.andMap (decodeOptionalData "rate_limit_per_user" (JD.map Quantity JD.int))
        |> JD.andMap (decodeOptionalData "recipients" (JD.list decodeUser))
        |> JD.andMap (decodeOptionalData "icon" (JD.nullable JD.string))
        |> JD.andMap (decodeOptionalData "owner_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "application_id" decodeSnowflake)
        |> JD.andMap (decodeOptionalData "parent_id" (JD.nullable decodeSnowflake))
        |> JD.andMap (decodeOptionalData "last_pin_timestamp" Iso8601.decoder)


type ChannelTypes
    = GuildText
    | DirectMessage
    | GuildVoice
    | GroupDirectMessage
    | GuildCategory
    | GuildNews
    | GuildStore


decodeChannelTypes : JD.Decoder ChannelTypes
decodeChannelTypes =
    JD.andThen
        (\value ->
            case value of
                0 ->
                    JD.succeed GuildText

                1 ->
                    JD.succeed DirectMessage

                2 ->
                    JD.succeed GuildVoice

                3 ->
                    JD.succeed GroupDirectMessage

                4 ->
                    JD.succeed GuildCategory

                5 ->
                    JD.succeed GuildNews

                6 ->
                    JD.succeed GuildStore

                _ ->
                    JD.fail "Invalid channel type."
        )
        JD.int
