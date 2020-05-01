module Backend exposing (app, init, subscriptions, update, updateFromFrontend)

import DiscordApi
import Environment
import Helper
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import Set
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
      , lastGetUsersId = 0
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

        GotMessages lastMessageId result time ->
            case result of
                Ok messages ->
                    ( { model | lastMessageId = lastMessageId }, Cmd.none )

                Err error ->
                    ( Helper.addError error { model | lastDiscordBadStatus = Just time }, Cmd.none )

        UpdateLoop time ->
            let
                getMessages =
                    case model.lastMessageId of
                        Just lastMessageId_ ->
                            DiscordApi.getMessagesAfter
                                Environment.botToken
                                Environment.channelId
                                lastMessageId_
                                |> Helper.taskAttemptWithTime (GotMessages (Just lastMessageId_))

                        Nothing ->
                            DiscordApi.getLatestMessage
                                Environment.botToken
                                Environment.channelId
                                |> Helper.taskAttemptWithTime (GotMessages Nothing)

                getUsers =
                    DiscordApi.getUsers Environment.botToken Environment.guildId
                        |> Helper.taskAttemptWithTime (GotUsers (model.lastGetUsersId + 1))

                getBotUser =
                    if model.botUserId == Nothing then
                        DiscordApi.getCurrentUser Environment.botToken |> Task.attempt GotBotUser

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

        GotUsers getUsersId result time ->
            case result of
                Ok users ->
                    let
                        newUsers =
                            users |> List.map (.id >> (\(DiscordApi.Id id) -> id)) |> Set.fromList
                    in
                    if getUsersId > model.lastGetUsersId then
                        case model.users of
                            Just oldUsers ->
                                --let
                                --    greetings =
                                --        Set.diff newUsers oldUsers
                                --            |> Set.toList
                                --            |> List.filterMap
                                --                (\userId ->
                                --                    case List.find (.id >> (==) (DiscordApi.Id userId)) users of
                                --                        Just user ->
                                --                            DiscordApi.createReaction
                                --                                Environment.botToken
                                --                                Environment.channelId
                                --                                "wave"
                                --                                (DiscordApi.Id "df7ba0f4020ca70048a0226d1dfa73f6")
                                --                                |> Task.attempt CreatedMessage
                                --                                |> Just
                                --
                                --                        Nothing ->
                                --                            --This should never happen
                                --                            Nothing
                                --                )
                                --in
                                ( { model | lastGetUsersId = getUsersId, users = Just (Set.union oldUsers newUsers) }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model | lastGetUsersId = getUsersId, users = Just newUsers }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Err error ->
                    ( Helper.addError error { model | lastDiscordBadStatus = Just time }, Cmd.none )

        GotBotUser result ->
            case result of
                Ok botUser ->
                    ( { model | botUserId = Just botUser.id }, Cmd.none )

                Err error ->
                    ( Helper.addError error model, Cmd.none )


waveReaction : DiscordApi.Id DiscordApi.MessageId -> Task String ()
waveReaction messageId =
    DiscordApi.createReaction
        Environment.botToken
        Environment.channelId
        messageId
        "wave"
        (DiscordApi.Id "df7ba0f4020ca70048a0226d1dfa73f6")
