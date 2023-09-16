port module Page.Chat.ChatServer exposing
    ( connect, Error(..)
    , portNames
    )

{-|

@docs connect, Error

@docs portNames

-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import Page.Chat.Message as Message exposing (ActiveUser, Message)
import Tepa exposing (PortRequest, PortResponse, Promise)
import Tepa.Stream as Stream exposing (Stream)


port page_chat_chatServer_events_request : PortRequest a


port page_chat_chatServer_events_response : PortResponse a


port page_chat_chatServer_events_cancel : PortRequest a


{-| -}
connect : Promise m (Stream (Result Error Message))
connect =
    Tepa.customPortStream
        { request = page_chat_chatServer_events_request
        , response = page_chat_chatServer_events_response
        , cancel = page_chat_chatServer_events_cancel
        , portName = portNames.connect
        , requestBody = JE.null
        }
        |> Tepa.map
            (Stream.filterMap
                (\v ->
                    case JD.decodeValue eventDecoder v of
                        Ok res ->
                            Just res

                        Err _ ->
                            Nothing
                )
            )


portNames :
    { connect : String
    }
portNames =
    { connect = "connect"
    }


eventDecoder : Decoder (Result Error Message)
eventDecoder =
    JD.field "message" JD.string
        |> JD.andThen
            (\res ->
                case res of
                    "ReceiveMessage" ->
                        JD.field "payload" messageDecoder
                            |> JD.map Ok

                    "ConnectionError" ->
                        JD.field "error" JD.string
                            |> JD.andThen
                                (\error ->
                                    case error of
                                        "LoginRequired" ->
                                            JD.succeed <| Err LoginRequired

                                        _ ->
                                            JD.succeed <| Err FatalError
                                )

                    "Disconnected" ->
                        JD.succeed <| Err Disconnected

                    "FatalError" ->
                        JD.succeed <| Err FatalError

                    _ ->
                        -- Just ignore
                        JD.fail "Invalid result"
            )


messageDecoder : Decoder Message
messageDecoder =
    JD.field "event" JD.string
        |> JD.andThen
            (\event ->
                case event of
                    "UserEntered" ->
                        JD.map2
                            (\user activeUsers ->
                                Message.UserEntered
                                    { user = user
                                    , activeUsers = activeUsers
                                    }
                            )
                            (JD.field "user" userDecoder)
                            (JD.field "active-users" <| JD.list userDecoder)

                    "UserLeft" ->
                        JD.map2
                            (\user activeUsers ->
                                Message.UserLeft
                                    { user = user
                                    , activeUsers = activeUsers
                                    }
                            )
                            (JD.field "user" userDecoder)
                            (JD.field "active-users" <| JD.list userDecoder)

                    "UserMessage" ->
                        JD.map2
                            (\user value ->
                                Message.UserMessage
                                    { user = user
                                    , message = value
                                    }
                            )
                            (JD.field "user" userDecoder)
                            (JD.field "value" JD.string)

                    _ ->
                        -- Just ignore
                        JD.fail "Invalid message"
            )


userDecoder : Decoder ActiveUser
userDecoder =
    JD.map2 ActiveUser
        (JD.field "display-name" JD.string)
        (JD.field "color" JD.string)


{-| -}
type Error
    = LoginRequired
    | Disconnected
    | FatalError
