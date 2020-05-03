module Backend exposing (app, init, subscriptions, update, updateFromFrontend)

import Discord exposing (MessagesRelativeTo(..), OptionalData(..))
import Environment
import Helper
import Lamdera exposing (ClientId, SessionId)
import Task exposing (Task)
import Time exposing (Month(..))
import Time.Extra as Time
import Types exposing (..)


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions _ =
    Sub.batch
        [ Time.every 4000 UpdateLoop
        ]


updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )



---- MODEL ----


init : ( BackendModel, Cmd BackendMsg )
init =
    ( { errors = []
      , lastDiscordBadStatus = Nothing
      , lastMessageId = Nothing
      , users = Nothing
      , botUserId = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CreatedMessage result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    ( Helper.addError error model, Cmd.none )

        CreatedReaction result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    ( Helper.addError error model, Cmd.none )

        GotMessages lastMessageId result time ->
            case result of
                Ok messages ->
                    let
                        ( newUsers, cmd ) =
                            case model.users of
                                Just users ->
                                    messages
                                        |> List.foldl
                                            (\message ( users_, cmds ) ->
                                                if List.any ((==) message.author.id) users_ then
                                                    ( users_, cmds )

                                                else
                                                    ( message.author.id :: users_
                                                    , Cmd.batch
                                                        [ cmds
                                                        , Task.attempt CreatedReaction (waveReaction message.id)
                                                        ]
                                                    )
                                            )
                                            ( users, Cmd.none )
                                        |> Tuple.mapFirst Just

                                Nothing ->
                                    ( Nothing, Cmd.none )
                    in
                    ( { model | lastMessageId = lastMessageId, users = newUsers }, cmd )

                Err error ->
                    ( Helper.addError error { model | lastDiscordBadStatus = Just time }, Cmd.none )

        UpdateLoop time ->
            let
                getMessages =
                    case model.lastMessageId of
                        Just lastMessageId_ ->
                            Discord.getMessages
                                Environment.botToken
                                { channelId = Environment.channelId, limit = 100, relativeTo = After lastMessageId_ }
                                |> Helper.taskAttemptWithTime (GotMessages (Just lastMessageId_))

                        Nothing ->
                            Discord.getMessages
                                Environment.botToken
                                { channelId = Environment.channelId, limit = 1, relativeTo = MostRecent }
                                |> Helper.taskAttemptWithTime (GotMessages Nothing)

                getUsers =
                    case model.users of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            Discord.getChannel Environment.botToken Environment.channelId
                                |> Task.andThen
                                    (\channel ->
                                        case channel.guildId of
                                            Included guildId ->
                                                Discord.getUsers
                                                    Environment.botToken
                                                    { guildId = guildId, limit = 100, after = Nothing }

                                            Missing ->
                                                Task.fail "Failed to get guild."
                                    )
                                |> Helper.taskAttemptWithTime GotUsers

                getBotUser =
                    if model.botUserId == Nothing then
                        Discord.getCurrentUser Environment.botToken |> Task.attempt GotBotUser

                    else
                        Cmd.none
            in
            ( model
            , Cmd.batch
                [ getUsers
                , getBotUser
                , case model.lastDiscordBadStatus of
                    Just lastBadStatus ->
                        if lastBadStatus |> Time.addSeconds 6 |> Time.compare time |> (==) GT then
                            getMessages

                        else
                            Cmd.none

                    Nothing ->
                        getMessages
                ]
            )

        GotUsers result time ->
            case result of
                Ok users ->
                    let
                        newUsers =
                            users
                                |> List.filterMap
                                    (\guildMember ->
                                        case guildMember.user of
                                            Included user ->
                                                Just user.id

                                            Missing ->
                                                Nothing
                                    )
                    in
                    case model.users of
                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( { model | users = Just newUsers }, Cmd.none )

                Err error ->
                    ( Helper.addError error { model | lastDiscordBadStatus = Just time }, Cmd.none )

        GotBotUser result ->
            case result of
                Ok botUser ->
                    ( { model | botUserId = Just botUser.id }, Cmd.none )

                Err error ->
                    ( Helper.addError error model, Cmd.none )


waveReaction : Discord.Id Discord.MessageId -> Task String ()
waveReaction messageId =
    Discord.createReaction
        Environment.botToken
        { channelId = Environment.channelId, messageId = messageId, emoji = "👋" }
