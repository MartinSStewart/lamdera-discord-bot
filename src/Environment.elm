module Environment exposing (botId, botToken, channelId, guildId)

{-| -}

import DiscordApi exposing (BotToken, ChannelId, GuildId, Id, UserId)


{-| Authentication code for your bot. Not to be confused with Client ID and Client Secret.
-}
botToken : BotToken
botToken =
    DiscordApi.botToken ""


{-| Channel ID that the bot checks for messages in.
Currently the bot is limited to only checking for messages in a single channel.
-}
channelId : DiscordApi.Id ChannelId
channelId =
    DiscordApi.Id ""


guildId : DiscordApi.Id GuildId
guildId =
    DiscordApi.Id ""


botId : Id UserId
botId =
    Id ""
