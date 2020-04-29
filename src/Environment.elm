module Environment exposing (botId, botToken, channelId, discordApiUrl, guildId)

{-| -}

import Types exposing (BotToken(..), ChannelId(..), GuildId(..), UserId(..))


{-| Authentication code for your bot. Not to be confused with Client ID and Client Secret.
-}
botToken : BotToken
botToken =
    BotToken ""


{-| Channel ID that the bot checks for messages in.
Currently the bot is limited to only checking for messages in a single channel.
-}
channelId : ChannelId
channelId =
    ChannelId ""


guildId : GuildId
guildId =
    GuildId ""


botId : UserId
botId =
    UserId ""
