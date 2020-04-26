module Backend exposing (app, init, subscriptions, update, updateFromFrontend)

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
        [ Time.every 4000 CheckForNewMessages
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

        GotMessages lastMessageId result ->
            ( model, Time.now |> Task.perform (GotMessagesWithTime lastMessageId result) )

        GotMessagesWithTime lastMessageId result time ->
            case result of
                Ok messages ->
                    ( { model | lastMessageId = lastMessageId }, Cmd.none )

                Err error ->
                    ( Helper.addError error { model | lastDiscordBadStatus = Just time }, Cmd.none )

        CheckForNewMessages time ->
            let
                getMessages =
                    case model.lastMessageId of
                        Just lastMessageId_ ->
                            Helper.getMessagesAfter lastMessageId_
                                |> Task.attempt (GotMessages (Just lastMessageId_))

                        Nothing ->
                            Helper.getLatestMessage |> Task.attempt (GotMessages Nothing)
            in
            ( model
            , case model.lastDiscordBadStatus of
                Just lastBadStatus ->
                    if lastBadStatus |> Time.addSeconds 6 |> Time.compare time |> (==) GT then
                        getMessages

                    else
                        Cmd.none

                Nothing ->
                    getMessages
            )
