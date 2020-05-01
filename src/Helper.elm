module Helper exposing
    ( addError
    , isValidMessage
    , taskAttemptWithTime
    , taskMerge
    )

import DiscordApi
import Environment
import Task exposing (Task)
import Time


addError : String -> { a | errors : List String } -> { a | errors : List String }
addError error model =
    { model | errors = error :: model.errors }


isValidMessage : DiscordApi.Message -> Bool
isValidMessage message =
    message.author.bot /= DiscordApi.Included True


taskMerge : Task a a -> Cmd a
taskMerge task =
    task
        |> Task.attempt
            (\result ->
                case result of
                    Ok ok ->
                        ok

                    Err err ->
                        err
            )


taskAttemptWithTime : (Result e a -> Time.Posix -> msg) -> Task e a -> Cmd msg
taskAttemptWithTime msgWithTime task =
    task
        |> Task.andThen (\ok -> Time.now |> Task.map (msgWithTime (Ok ok)))
        |> Task.onError (\error -> Time.now |> Task.map (msgWithTime (Err error)))
        |> taskMerge
