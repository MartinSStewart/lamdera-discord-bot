module Environment exposing (botToken, channelId)

{-| -}

import Discord exposing (BotToken, ChannelId, GuildId, Id, UserId)


{-| Authentication code for your bot. Not to be confused with Client ID and Client Secret.
-}
botToken : BotToken
botToken =
    Discord.botToken ""


{-| Channel ID that the bot checks for messages in.
Currently the bot is limited to only checking for messages in a single channel.
-}
channelId : Discord.Id ChannelId
channelId =
    Discord.Id ""
