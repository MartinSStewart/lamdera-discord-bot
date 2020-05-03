module Discord exposing
    ( Authentication, botToken
    , getChannel, getMessages, MessagesRelativeTo(..), createMessage, getReactions, createReaction, deleteOwnReaction, deleteUserReaction, deleteAllReactions, deleteAllReactionsForEmoji, deleteMessage, bulkDeleteMessage, Channel, PartialChannel, ChannelId, Message, MessageId, Reaction, Attachment, AttachmentId
    , Emoji, EmojiId
    , getUsers, Guild, GuildId, GuildMember, RoleId, PartialGuild
    , Invite, InviteWithMetadata
    , getCurrentUser, getCurrentUserGuilds, User, PartialUser, UserId, Permissions
    , WebhookId
    , ChannelInviteConfig, Id(..), OptionalData(..), createChannelInvite, defaultChannelInviteConfig, editMessage, getChannelInvites
    )

{-| The beginnings of an Elm package...

Useful Discord links:

  - API documentation: <https://discordapp.com/developers/docs/intro>
  - Create bot invites: <https://discordapi.com/permissions.html>

Before starting, note that this package requires user credentials and creates tasks.
If I were evil (or my account got hacked) I could try to sneak in code that sends your Discord credentials to some other server.
For that reason it's probably a good idea to have a look at the source code and double check that it doesn't try anything sneaky!


# Authentication

@docs Authentication, botToken


# Audit Log


# Channel

@docs getChannel, getMessages, MessagesRelativeTo, createMessage, getReactions, createReaction, deleteOwnReaction, deleteUserReaction, deleteAllReactions, deleteAllReactionsForEmoji, deleteMessage, bulkDeleteMessage, Channel, PartialChannel, ChannelId, Message, MessageId, Reaction, Attachment, AttachmentId


# Emoji

@docs Emoji, EmojiId


# Guild

@docs getUsers, Guild, GuildId, GuildMember, RoleId, PartialGuild


# Invite

@docs Invite, InviteWithMetadata


# User

@docs getCurrentUser, getCurrentUserGuilds, User, PartialUser, UserId, Permissions


# Voice


# Webhook

@docs WebhookId

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


botToken : String -> Authentication
botToken botTokenText =
    BotToken botTokenText


{-| Returns a list of guild members that are members of the guild.

  - limit: Max number of members to return (1-1000)
  - after: The highest user id in the previous page

-}
getUsers : Authentication -> { guildId : Id GuildId, limit : Int, after : Maybe (Id UserId) } -> Task String (List GuildMember)
getUsers authentication { guildId, limit, after } =
    httpGet
        authentication
        (JD.list decodeGuildMember)
        [ "guilds", rawId guildId, "members" ]
        (Url.Builder.int "limit" limit
            :: (case after of
                    Just (Id after_) ->
                        [ Url.Builder.string "after" after_ ]

                    Nothing ->
                        []
               )
        )


getChannel : Authentication -> Id ChannelId -> Task String Channel
getChannel authentication (Id channelId) =
    httpGet authentication decodeChannel [ "channels", channelId ] []



-- Modify channel excluded


{-| Delete a channel, or close a private message.
Requires the `MANAGE_CHANNELS` permission for the guild.
Deleting a category does not delete its child channels; they will have their `parent_id` removed and a Channel Update Gateway event will fire for each of them.
Returns a channel object on success.
Fires a Channel Delete Gateway event.

Deleting a guild channel cannot be undone.
Use this with caution, as it is impossible to undo this action when performed on a guild channel.
In contrast, when used with a private message, it is possible to undo the action by opening a private message with the recipient again.

For Public servers, the set Rules or Guidelines channel and the Moderators-only (Public Server Updates) channel cannot be deleted.

-}
deleteChannel : Authentication -> Id ChannelId -> Task String Channel
deleteChannel authentication (Id channelId) =
    httpDelete authentication decodeChannel [ "channels", channelId ] [] (JE.string "")


{-| -}
type MessagesRelativeTo
    = Around (Id MessageId)
    | Before (Id MessageId)
    | After (Id MessageId)
    | MostRecent


{-| Returns the messages for a channel.
If operating on a guild channel, this endpoint requires the `VIEW_CHANNEL` permission to be present on the current user.
If the current user is missing the `READ_MESSAGE_HISTORY` permission in the channel then this will return no messages (since they cannot read the message history).

  - channelId: The channel to get messages from
  - limit: Max number of messages to return (1-100)
  - relativeTo: Relative to which message should we retrieve messages?
    Or should we get the most recent messages?

-}
getMessages : Authentication -> { channelId : Id ChannelId, limit : Int, relativeTo : MessagesRelativeTo } -> Task String (List Message)
getMessages authentication { channelId, limit, relativeTo } =
    httpGet
        authentication
        (JD.list decodeMessage)
        [ "channels", rawId channelId, "messages" ]
        (Url.Builder.int "limit" limit
            :: (case relativeTo of
                    Around (Id messageId) ->
                        [ Url.Builder.string "around" messageId ]

                    Before (Id messageId) ->
                        [ Url.Builder.string "around" messageId ]

                    After (Id messageId) ->
                        [ Url.Builder.string "around" messageId ]

                    MostRecent ->
                        []
               )
        )


{-| Returns a specific message in the channel.
If operating on a guild channel, this endpoint requires the `READ_MESSAGE_HISTORY` permission to be present on the current user.
-}
getMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String (List Message)
getMessage authentication { channelId, messageId } =
    httpGet
        authentication
        (JD.list decodeMessage)
        [ "channels", rawId channelId, "messages", rawId messageId ]
        []


{-| Before using this endpoint, you must connect to and identify with a gateway at least once.

Discord may strip certain characters from message content, like invalid unicode characters or characters which cause unexpected message formatting.
If you are passing user-generated strings into message content, consider sanitizing the data to prevent unexpected behavior and utilizing `allowed_mentions` to prevent unexpected mentions.

Post a message to a guild text or DM channel.
If operating on a guild channel, this endpoint requires the `SEND_MESSAGES` permission to be present on the current user.
If the tts field is set to true, the `SEND_TTS_MESSAGES` permission is required for the message to be spoken.
Returns a message object. Fires a Message Create Gateway event.
See message formatting for more information on how to properly format messages.

The maximum request size when sending a message is 8MB.

-}
createMessage : Authentication -> { channelId : Id ChannelId, content : String } -> Task String ()
createMessage authentication { channelId, content } =
    httpPost
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages" ]
        []
        (JE.object [ ( "content", JE.string content ) ])


{-| Create a reaction for the message.
This endpoint requires the `READ_MESSAGE_HISTORY` permission to be present on the current user.
Additionally, if nobody else has reacted to the message using this emoji, this endpoint requires the `ADD_REACTIONS` permission to be present on the current user.
-}
createReaction : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String } -> Task String ()
createReaction authentication { channelId, messageId, emoji } =
    httpPut
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId, "reactions", emoji, "@me" ]
        []
        (JE.object [])


{-| Delete a reaction the current user has made for the message.
-}
deleteOwnReaction : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String } -> Task String ()
deleteOwnReaction authentication { channelId, messageId, emoji } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId, "reactions", emoji, "@me" ]
        []
        (JE.object [])


{-| Deletes another user's reaction.
This endpoint requires the `MANAGE_MESSAGES` permission to be present on the current user.
-}
deleteUserReaction :
    Authentication
    -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String, userId : Id UserId }
    -> Task String ()
deleteUserReaction authentication { channelId, messageId, emoji, userId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId, "reactions", emoji, rawId userId ]
        []
        (JE.object [])


{-| Get a list of users that reacted with this emoji.
-}
getReactions : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String } -> Task String ()
getReactions authentication { channelId, messageId, emoji } =
    httpGet
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId, "reactions", emoji ]
        [ Url.Builder.int "limit" 100 ]


{-| Deletes all reactions on a message.
This endpoint requires the `MANAGE_MESSAGES` permission to be present on the current user.
-}
deleteAllReactions : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String ()
deleteAllReactions authentication { channelId, messageId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId, "reactions" ]
        []
        (JE.object [])


{-| Deletes all the reactions for a given emoji on a message.
This endpoint requires the `MANAGE_MESSAGES` permission to be present on the current user.
-}
deleteAllReactionsForEmoji :
    Authentication
    -> { channelId : Id ChannelId, messageId : Id MessageId, emoji : String }
    -> Task String ()
deleteAllReactionsForEmoji authentication { channelId, messageId, emoji } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId, "reactions", emoji ]
        []
        (JE.object [])


{-| Edit a previously sent message. The fields content can only be edited by the original message author.
The content field can have a maximum of 2000 characters.
-}
editMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId, content : String } -> Task String ()
editMessage authentication { channelId, messageId, content } =
    httpPatch
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId ]
        [ Url.Builder.string "content" content ]
        (JE.object [])


{-| Delete a message.
If operating on a guild channel and trying to delete a message that was not sent by the current user, this endpoint requires the `MANAGE_MESSAGES` permission.
-}
deleteMessage : Authentication -> { channelId : Id ChannelId, messageId : Id MessageId } -> Task String ()
deleteMessage authentication { channelId, messageId } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", rawId messageId ]
        []
        (JE.object [])


{-| Delete multiple messages in a single request.
This endpoint can only be used on guild channels and requires the `MANAGE_MESSAGES` permission.
Any message IDs given that do not exist or are invalid will count towards the minimum and maximum message count (currently 2 and 100 respectively).

This endpoint will not delete messages older than 2 weeks, and will fail with a 400 BAD REQUEST if any message provided is older than that or if any duplicate message IDs are provided.

-}
bulkDeleteMessage :
    Authentication
    ->
        { channelId : Id ChannelId
        , firstMessage : Id MessageId
        , secondMessage : Id MessageId
        , restOfMessages : List (Id MessageId)
        }
    -> Task String ()
bulkDeleteMessage authentication { channelId, firstMessage, secondMessage, restOfMessages } =
    httpDelete
        authentication
        (JD.succeed ())
        [ "channels", rawId channelId, "messages", "bulk-delete" ]
        []
        (JE.list JE.string (rawId firstMessage :: rawId secondMessage :: List.map rawId restOfMessages))



-- Edit Channel Permissions excluded


{-| Returns a list of invites for the channel.
Only usable for guild channels. Requires the `MANAGE_CHANNELS` permission.
-}
getChannelInvites : Authentication -> Id ChannelId -> Task String (List InviteWithMetadata)
getChannelInvites authentication channelId =
    httpGet
        authentication
        (JD.list decodeInviteWithMetadata)
        [ "channels", rawId channelId, "invites" ]
        []


{-| -maxAge: Duration of invite in before it expires. `Nothing` means it never expires.
-maxUsers: Max number of uses. `Nothing` means it has unlimited uses.
-temporaryMembership: Whether this invite only grants temporary membership.
-unique: If true, don't try to reuse a similar invite (useful for creating many unique one time use invites).
-targetUser: The target user id for this invite.
-}
type alias ChannelInviteConfig =
    { maxAge : Maybe (Quantity Int Seconds)
    , maxUses : Maybe Int
    , temporaryMembership : Bool
    , unique : Bool
    , targetUser : Maybe (Id UserId)
    }


{-| Default invite settings. Can be used an unlimited number of times but expires after 1 day.
-}
defaultChannelInviteConfig : ChannelInviteConfig
defaultChannelInviteConfig =
    { maxAge = Just (Quantity.round Duration.day)
    , maxUses = Nothing
    , temporaryMembership = False
    , unique = False
    , targetUser = Nothing
    }


{-| Create a new invite object for the channel. Only usable for guild channels.
Requires the `CREATE_INSTANT_INVITE` permission.
-}
createChannelInvite :
    Authentication
    -> Id ChannelId
    -> ChannelInviteConfig
    -> Task String Invite
createChannelInvite authentication channelId { maxAge, maxUses, temporaryMembership, unique, targetUser } =
    httpPost
        authentication
        decodeInvite
        [ "channels", rawId channelId, "invites" ]
        []
        (JE.object
            (( "max_age"
             , case maxAge of
                Just (Quantity maxAge_) ->
                    max 1 maxAge_ |> JE.int

                Nothing ->
                    JE.int 0
             )
                :: ( "max_uses"
                   , case maxUses of
                        Just maxUses_ ->
                            max 1 maxUses_ |> JE.int

                        Nothing ->
                            JE.int 0
                   )
                :: ( "temporary", JE.bool temporaryMembership )
                :: ( "unique", JE.bool unique )
                :: (case targetUser of
                        Just (Id targetUserId) ->
                            [ ( "target_user", JE.string targetUserId ) ]

                        Nothing ->
                            []
                   )
            )
        )


type alias Invite =
    { code : InviteCode
    , guild : OptionalData PartialGuild
    , channel : PartialChannel
    , inviter : OptionalData User
    , targetUser : OptionalData PartialUser
    , targetUserType : OptionalData Int
    , approximatePresenceCount : OptionalData Int
    , approximateMemberCount : OptionalData Int
    }


type alias InviteWithMetadata =
    { code : InviteCode
    , guild : OptionalData PartialGuild
    , channel : PartialChannel
    , inviter : OptionalData User
    , targetUser : OptionalData PartialUser
    , targetUserType : OptionalData Int
    , approximatePresenceCount : OptionalData Int
    , approximateMemberCount : OptionalData Int
    , uses : Int
    , maxUses : Int
    , maxAge : Maybe (Quantity Int Seconds)
    , temporaryMembership : Bool
    , createdAt : Time.Posix
    }


decodeInviteCode : JD.Decoder InviteCode
decodeInviteCode =
    JD.map InviteCode JD.string


decodeInvite : JD.Decoder Invite
decodeInvite =
    JD.map8 Invite
        (JD.field "code" decodeInviteCode)
        (decodeOptionalData "guild" decodePartialGuild)
        (JD.field "channel" decodePartialChannel)
        (decodeOptionalData "inviter" decodeUser)
        (decodeOptionalData "target_user" decodePartialUser)
        (decodeOptionalData "target_user_type" JD.int)
        (decodeOptionalData "approximate_presence_count" JD.int)
        (decodeOptionalData "approximate_member_count" JD.int)


decodeInviteWithMetadata : JD.Decoder InviteWithMetadata
decodeInviteWithMetadata =
    JD.succeed InviteWithMetadata
        |> JD.andMap (JD.field "code" decodeInviteCode)
        |> JD.andMap (decodeOptionalData "guild" decodePartialGuild)
        |> JD.andMap (JD.field "channel" decodePartialChannel)
        |> JD.andMap (decodeOptionalData "inviter" decodeUser)
        |> JD.andMap (decodeOptionalData "target_user" decodePartialUser)
        |> JD.andMap (decodeOptionalData "target_user_type" JD.int)
        |> JD.andMap (decodeOptionalData "approximate_presence_count" JD.int)
        |> JD.andMap (decodeOptionalData "approximate_member_count" JD.int)
        |> JD.andMap (JD.field "uses" JD.int)
        |> JD.andMap (JD.field "max_uses" JD.int)
        |> JD.andMap
            (JD.field "max_age"
                (JD.map
                    (\value ->
                        if value == 0 then
                            Nothing

                        else
                            Just (Quantity value)
                    )
                    JD.int
                )
            )
        |> JD.andMap (JD.field "temporary" JD.bool)
        |> JD.andMap (JD.field "created_at" Iso8601.decoder)


type alias PartialChannel =
    { id : Id ChannelId
    , name : String
    , type_ : ChannelType
    }


decodePartialChannel : JD.Decoder PartialChannel
decodePartialChannel =
    JD.map3 PartialChannel
        (JD.field "id" decodeSnowflake)
        (JD.field "name" JD.string)
        (JD.field "type" decodeChannelType)


type alias PartialUser =
    { id : Id UserId
    , username : String
    , avatar : Maybe String
    , discriminator : Int
    }


decodePartialUser : JD.Decoder PartialUser
decodePartialUser =
    JD.map4 PartialUser
        (JD.field "id" decodeSnowflake)
        (JD.field "username" JD.string)
        (JD.field "avatar" (JD.nullable JD.string))
        (JD.field "discriminator" decodeDiscriminator)


decodeDiscriminator : JD.Decoder Int
decodeDiscriminator =
    JD.andThen
        (\text ->
            case String.toInt text of
                Just value ->
                    JD.succeed value

                Nothing ->
                    JD.fail "Invalid discriminator"
        )
        JD.string


getCurrentUser : Authentication -> Task String User
getCurrentUser authentication =
    httpGet
        authentication
        decodeUser
        [ "users", "@me" ]
        []


getCurrentUserGuilds : Authentication -> Task String (List PartialGuild)
getCurrentUserGuilds authentication =
    httpGet
        authentication
        (JD.list decodePartialGuild)
        [ "users", "@me", "guilds" ]
        []


discordApiUrl : String
discordApiUrl =
    "https://discordapp.com/api/"


type Authentication
    = BotToken String


rawId : Id idType -> String
rawId (Id id) =
    id


authorization : Authentication -> Http.Header
authorization (BotToken authentication) =
    Http.header "Authorization" ("Bot " ++ authentication)


httpPost : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpPost authentication decoder path queryParameters body =
    http authentication "POST" decoder path queryParameters (Http.jsonBody body)


httpPut : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpPut authentication decoder path queryParameters body =
    http authentication "PUT" decoder path queryParameters (Http.jsonBody body)


httpPatch : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpPatch authentication decoder path queryParameters body =
    http authentication "DELETE" decoder path queryParameters (Http.jsonBody body)


httpDelete : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> JE.Value -> Task String a
httpDelete authentication decoder path queryParameters body =
    http authentication "DELETE" decoder path queryParameters (Http.jsonBody body)


httpGet : Authentication -> JD.Decoder a -> List String -> List QueryParameter -> Task String a
httpGet authentication decoder path queryParameters =
    http authentication "GET" decoder path queryParameters Http.emptyBody


http : Authentication -> String -> JD.Decoder a -> List String -> List QueryParameter -> Http.Body -> Task String a
http authentication requestType decoder path queryParameters body =
    Http.task
        { method = requestType
        , headers = [ authorization authentication ]
        , url = Url.Builder.crossOrigin discordApiUrl path queryParameters
        , resolver = Http.stringResolver (resolver decoder)
        , body = body
        , timeout = Nothing
        }


resolver : JD.Decoder a -> Http.Response String -> Result String a
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


type InviteCode
    = InviteCode String


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
        |> JD.andMap (JD.field "discriminator" decodeDiscriminator)
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
    , type_ : ChannelType
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
        |> JD.andMap (JD.field "type" decodeChannelType)
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


type ChannelType
    = GuildText
    | DirectMessage
    | GuildVoice
    | GroupDirectMessage
    | GuildCategory
    | GuildNews
    | GuildStore


decodeChannelType : JD.Decoder ChannelType
decodeChannelType =
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
