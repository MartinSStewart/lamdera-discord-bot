module Env exposing (..)

{-| Authentication code for your bot. Not to be confused with Client ID and Client Secret.
-}

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.

import Discord exposing (Authentication, ChannelId)
import UInt64


botToken_ : String
botToken_ =
    ""


botToken : Authentication
botToken =
    Discord.botToken botToken_


channelId_ : String
channelId_ =
    "0"


{-| Channel ID that the bot checks for messages in.
Currently the bot is limited to only checking for messages in a single channel.
-}
channelId : Discord.Id ChannelId
channelId =
    UInt64.fromString channelId_ |> Maybe.withDefault UInt64.zero |> Discord.Id
