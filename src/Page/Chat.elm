module Page.Chat exposing
    ( Memory
    , MemoryLink
    , MemoryBody
    , ScenarioProps
    , ScenarioSet
    , init
    , leave
    , onLoad
    , scenario
    , view
    )

{-| Chat page.

@docs Memory
@docs MemoryLink
@docs MemoryBody
@docs ScenarioProps
@docs ScenarioSet
@docs init
@docs leave
@docs onLoad
@docs scenario
@docs view

-}

import App.Bucket exposing (Bucket)
import App.Dom as RawDom
import App.Flags exposing (Flags)
import App.Path as Path
import App.Session exposing (Profile)
import AppUrl
import Dict
import Expect exposing (Expectation)
import Json.Encode as JE exposing (Value)
import Page.Chat.ChatServer as ChatServer
import Page.Chat.Message as Message exposing (ActiveUser, Message)
import Page.Chat.NewMessage as NewMessage
import String.Multiline exposing (here)
import Tepa exposing (Document, Layer, Promise, ViewContext)
import Tepa.Html as Html exposing (Html)
import Tepa.HtmlSelector as Selector
import Tepa.Mixin as Mixin exposing (Mixin)
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Stream as Stream
import Test.Html.Event as HtmlEvent
import Test.Html.Event.Extra as HtmlEvent
import Test.Html.Query as Query
import Widget.Header as Header
import Widget.Toast as Toast



-- Memory


{-| Page memory.
-}
type alias Memory =
    Tepa.LayerMemory MemoryLink MemoryBody


{-| Pointer to the external memory.
-}
type alias MemoryLink =
    { profile : Profile
    , toast : Toast.Memory
    }


{-| Memory area for this page.
-}
type alias MemoryBody =
    { activeUsers : List ActiveUser
    , messages : List Message -- reversed
    , newMessageForm : NewMessageFormMemory
    }


{-| -}
init : Promise m MemoryBody
init =
    Tepa.succeed
        { activeUsers = []
        , messages = []
        , newMessageForm =
            { isBusy = False
            , showError = False
            }
        }


{-| Procedure for releasing resources, saving scroll position, and so on.
-}
leave : Promise Memory ()
leave =
    Tepa.none



-- View


{-| -}
view :
    Flags
    ->
        Layer
            { link : MemoryLink
            , body : MemoryBody
            }
    -> Document
view _ =
    Tepa.layerView <|
        \context ->
            { title = "Sample App | Chat"
            , body =
                [ let
                    bodyContext =
                        Tepa.mapViewContext .body context

                    linkContext =
                        Tepa.mapViewContext .link context
                  in
                  Html.div
                    [ localClass "page"
                    ]
                    [ Header.view
                        (localClass "header")
                    , Html.div
                        [ localClass "body"
                        ]
                        [ activeUsersView bodyContext.state.activeUsers
                        , messageFieldView bodyContext.state.messages
                        , Toast.view
                            (Tepa.mapViewContext .toast linkContext)
                        ]
                    , Html.div
                        [ localClass "footer"
                        ]
                        [ newMessageFormView
                            (Tepa.mapViewContext .newMessageForm bodyContext)
                        ]
                    ]
                ]
            }


activeUsersView : List ActiveUser -> Html
activeUsersView activeUsers =
    Html.div
        [ localClass "activeUsers"
        ]
        [ Html.div
            [ localClass "activeUsers_header"
            ]
            [ Html.text "Active Users"
            ]
        , Html.div
            [ localClass "activeUsers_body"
            ]
            (List.map
                (\activeUser ->
                    Html.div
                        [ localClass "activeUsers_body_item"
                        , Mixin.style "--active-user-color" activeUser.color
                        ]
                        [ Html.text activeUser.displayName
                        ]
                )
                activeUsers
            )
        ]


messageFieldView : List Message -> Html
messageFieldView messages =
    Html.div
        [ localClass "messageField"
        ]
        [ Html.div
            [ localClass "messageField_body"
            ]
            (List.reverse messages
                |> List.map renderMessage
            )
        ]


renderMessage : Message -> Html
renderMessage message =
    case message of
        Message.UserEntered param ->
            Html.div
                [ localClass "messageField_body_item"
                , localClass "messageField_body_item-userEntered"
                ]
                [ Html.span
                    [ localClass "messageField_body_item_user"
                    , Mixin.style "--active-user-color" param.user.color
                    ]
                    [ Html.text param.user.displayName
                    ]
                , Html.span
                    [ localClass "messageField_body_item_text"
                    ]
                    [ Html.text " has entered."
                    ]
                ]

        Message.UserMessage param ->
            Html.div
                [ localClass "messageField_body_item"
                , localClass "messageField_body_item-userMessage"
                ]
                [ Html.span
                    [ localClass "messageField_body_item_user"
                    , Mixin.style "--active-user-color" param.user.color
                    ]
                    [ Html.text param.user.displayName
                    ]
                , Html.span
                    [ localClass "messageField_body_item_text"
                    ]
                    [ Html.text <| ": " ++ param.message
                    ]
                ]

        Message.UserLeft param ->
            Html.div
                [ localClass "messageField_body_item"
                , localClass "messageField_body_item-userLeft"
                ]
                [ Html.span
                    [ localClass "messageField_body_item_user"
                    , Mixin.style "--active-user-color" param.user.color
                    ]
                    [ Html.text param.user.displayName
                    ]
                , Html.span
                    [ localClass "messageField_body_item_text"
                    ]
                    [ Html.text " has left."
                    ]
                ]



-- -- NewMessageForm


type alias NewMessageFormMemory =
    { isBusy : Bool
    , showError : Bool
    }


newMessageFormView : ViewContext NewMessageFormMemory -> Html
newMessageFormView { state, setKey, values, layerId } =
    let
        errors =
            NewMessage.toFormErrors values
    in
    Html.div
        [ localClass "newMessageForm"
        , Mixin.boolAttribute "aria-invalid"
            (state.showError && not (List.isEmpty errors))
        ]
        [ Html.node "textarea"
            [ localClass "newMessageForm_control_body"
            , domId
                { layerId = layerId
                , key = NewMessage.keys.body
                }
            , setKey NewMessage.keys.body
            , Mixin.boolAttribute "aria-invalid" <|
                state.showError
                    && List.member NewMessage.BodyRequired errors
            ]
            []
        , if state.showError && List.length errors > 0 then
            Html.div
                [ localClass "newMessageForm_errorField"
                ]
                (List.map
                    (\err ->
                        Html.div
                            [ localClass "newMessageForm_errorField_error"
                            ]
                            [ Html.text <| NewMessage.displayFormError err
                            ]
                    )
                    errors
                )

          else
            Html.text ""
        , Html.node "button"
            [ localClass "newMessageForm_control_submit"
            , Mixin.attribute "type" "button"
            , Mixin.boolAttribute "aria-busy" state.isBusy
            , Mixin.boolAttribute "aria-disabled" <| state.showError && not (List.isEmpty errors)
            , setKey NewMessage.keys.submit
            ]
            [ Html.text "Submit"
            ]
        ]



-- Procedures
-- -- Initialization


{-| -}
onLoad : Bucket -> Promise Memory ()
onLoad bucket =
    -- Main Procedures
    Tepa.syncAll
        [ chatEventHandler bucket
        , newMessageFormProcedure
        ]


chatEventHandler : Bucket -> Promise Memory ()
chatEventHandler bucket =
    Tepa.bind ChatServer.connect <|
        \eventStream ->
            [ Stream.while
                (\event ->
                    case event of
                        Ok (Message.UserEntered param) ->
                            [ Tepa.modifyBody <|
                                \m ->
                                    { m
                                        | activeUsers = param.activeUsers
                                        , messages = Message.UserEntered param :: m.messages
                                    }
                            ]

                        Ok (Message.UserLeft param) ->
                            [ Tepa.modifyBody <|
                                \m ->
                                    { m
                                        | activeUsers = param.activeUsers
                                        , messages = Message.UserLeft param :: m.messages
                                    }
                            ]

                        Ok (Message.UserMessage param) ->
                            [ Tepa.modifyBody <|
                                \m ->
                                    { m
                                        | messages = Message.UserMessage param :: m.messages
                                    }
                            ]

                        Err ChatServer.LoginRequired ->
                            [ Nav.pushPath bucket.key
                                { path =
                                    [ Path.prefix
                                    , "login"
                                    ]
                                , queryParameters =
                                    Dict.fromList
                                        [ ( "back"
                                          , [ AppUrl.toString bucket.requestPath
                                            ]
                                          )
                                        ]
                                , fragment = Nothing
                                }
                            ]

                        Err ChatServer.Disconnected ->
                            [ Toast.pushError
                                (here """
                            Network error, please check your network and reload page.
                            """)
                                |> runToastPromise
                                |> Tepa.void
                            ]

                        Err ChatServer.FatalError ->
                            [ Nav.reload
                            ]
                )
                eventStream
            ]


newMessageFormProcedure : Promise Memory ()
newMessageFormProcedure =
    -- IGNORE TCO
    let
        modifyNewMessageForm f =
            Tepa.modifyBody <|
                \m ->
                    { m
                        | newMessageForm = f m.newMessageForm
                    }
    in
    Tepa.bind
        (Tepa.awaitViewEvent
            { key = NewMessage.keys.submit
            , type_ = "click"
            }
        )
    <|
        \_ ->
            [ modifyNewMessageForm <|
                \m -> { m | isBusy = True, showError = True }
            , Tepa.bind Tepa.getValues <|
                \form ->
                    case NewMessage.fromForm form of
                        Err _ ->
                            [ modifyNewMessageForm <|
                                \m ->
                                    { m
                                        | isBusy = False
                                    }
                            , Tepa.lazy <| \_ -> newMessageFormProcedure
                            ]

                        Ok newMessage ->
                            [ Tepa.bind (NewMessage.request newMessage) <|
                                \response ->
                                    case response of
                                        NewMessage.GoodResponse resp ->
                                            [ modifyNewMessageForm <| \m -> { m | isBusy = False, showError = False }
                                            , setDomValue NewMessage.keys.body ""
                                            , Tepa.modifyBody <|
                                                \m -> { m | messages = resp :: m.messages }
                                            , Tepa.lazy <| \_ -> newMessageFormProcedure
                                            ]

                                        NewMessage.Disconnected ->
                                            [ Toast.pushError
                                                (here """
                                        Network error, please check your network and reload page.
                                        """)
                                                |> runToastPromise
                                                |> Tepa.void
                                            ]

                                        NewMessage.FatalError ->
                                            [ Nav.reload
                                            ]
                            ]
            ]



-- Toast


runToastPromise :
    Promise Toast.Memory a
    -> Promise Memory a
runToastPromise =
    Tepa.liftMemory
        { get = .toast
        , set = \toast m -> { m | toast = toast }
        }
        >> Tepa.onLink



-- Scenario


{-| -}
type alias ScenarioSet m =
    { layer : Layer m -> Maybe (Layer MemoryBody)
    , changeNewMessageFormBody :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , clickSubmitMessage :
        Scenario.Markup -> Scenario Flags m
    , receiveChatServerEvent :
        Value
        -> Scenario.Markup
        -> Scenario Flags m
    , receiveNewMessageResp :
        Value
        -> Scenario.Markup
        -> Scenario Flags m
    , expectAvailable :
        Scenario.Markup -> Scenario Flags m
    , expectRequestHandshake :
        Scenario.Markup -> Scenario Flags m
    , expectActiveUsers :
        { users : List ActiveUser
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectNewMessageBodyIsCleared :
        Scenario.Markup
        -> Scenario Flags m
    , expectNumOfMessageFieldItems :
        Int
        -> Scenario.Markup
        -> Scenario Flags m
    , expectUserEnteredMessage :
        { user : ActiveUser
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectUserMessage :
        { user : ActiveUser
        , value : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectUserLeftMessage :
        { user : ActiveUser
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectSubmitMessageButtonIsBusy :
        Bool -> Scenario.Markup -> Scenario Flags m
    }


{-| -}
type alias ScenarioProps m =
    { querySelf : Layer m -> Maybe (Layer MemoryBody)
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m -> ScenarioSet m
scenario props =
    { layer = props.querySelf
    , changeNewMessageFormBody = changeNewMessageFormBody props
    , clickSubmitMessage = clickSubmitMessage props
    , receiveChatServerEvent = receiveChatServerEvent props
    , receiveNewMessageResp = receiveNewMessageResp props
    , expectAvailable = expectAvailable props
    , expectRequestHandshake = expectRequestHandshake props
    , expectActiveUsers = expectActiveUsers props
    , expectNewMessageBodyIsCleared = expectNewMessageBodyIsCleared props
    , expectNumOfMessageFieldItems = expectNumOfMessageFieldItems props
    , expectUserEnteredMessage = expectUserEnteredMessage props
    , expectUserMessage = expectUserMessage props
    , expectUserLeftMessage = expectUserLeftMessage props
    , expectSubmitMessageButtonIsBusy = expectSubmitMessageButtonIsBusy props
    }


changeNewMessageFormBody : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario Flags m
changeNewMessageFormBody props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "newMessageForm_control_body"
                ]
        , operation = HtmlEvent.change value
        }


clickSubmitMessage : ScenarioProps m -> Scenario.Markup -> Scenario Flags m
clickSubmitMessage props markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "newMessageForm_control_submit"
                , Selector.attribute "aria-disabled" "false"
                , Selector.attribute "aria-busy" "false"
                ]
        , operation =
            HtmlEvent.click
        }


receiveChatServerEvent :
    ScenarioProps m
    -> Value
    -> Scenario.Markup
    -> Scenario Flags m
receiveChatServerEvent props payload markup =
    let
        event =
            JE.object
                [ ( "message", JE.string "ReceiveMessage" )
                , ( "payload", payload )
                ]
    in
    Scenario.portResponse props.session
        markup
        { layer = props.querySelf
        , response =
            \request ->
                if request.name == ChatServer.portNames.connect then
                    Just event

                else
                    Nothing
        }


receiveNewMessageResp :
    ScenarioProps m
    -> Value
    -> Scenario.Markup
    -> Scenario Flags m
receiveNewMessageResp props event markup =
    Scenario.portResponse props.session
        markup
        { layer = props.querySelf
        , response =
            \request ->
                if request.name == NewMessage.portName then
                    Just event

                else
                    Nothing
        }


receiveOverwriteValueResponse : ScenarioProps m -> Scenario.Markup -> Scenario Flags m
receiveOverwriteValueResponse props markup =
    Scenario.portResponse props.session
        markup
        { layer = props.querySelf
        , response =
            \request ->
                if request.name == RawDom.portNames.overwriteValue then
                    Just <|
                        JE.object
                            [ ( "result", JE.string "Success" )
                            ]

                else
                    Nothing
        }


expectRequestHandshake : ScenarioProps m -> Scenario.Markup -> Scenario Flags m
expectRequestHandshake props markup =
    Scenario.expectPortRequest props.session
        markup
        { layer =
            props.querySelf
        , expectation =
            List.filter
                (\request -> request.name == ChatServer.portNames.connect)
                >> List.length
                >> Expect.greaterThan 0
        }


expectAvailable : ScenarioProps m -> Scenario.Markup -> Scenario Flags m
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectActiveUsers :
    ScenarioProps m
    ->
        { users : List ActiveUser
        }
    -> Scenario.Markup
    -> Scenario Flags m
expectActiveUsers props { users } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.findAll
                        [ localClassSelector "activeUsers_body_item"
                        ]
                    |> exactMatch
                        (List.map
                            (\user field ->
                                field
                                    |> Query.has
                                        [ Selector.exactText user.displayName

                                        -- Not compatible with Mixin
                                        -- We are planning to create original Virtual DOM library.
                                        -- , Selector.style "--active-user-color" user.color
                                        ]
                            )
                            users
                        )
        }


exactMatch : List (Query.Single msg -> Expectation) -> Query.Multiple msg -> Expectation
exactMatch expects multi =
    Expect.all
        [ \_ ->
            multi
                |> Query.count (Expect.equal (List.length expects))
        , \_ ->
            Expect.all
                (List.indexedMap
                    (\n expect () ->
                        multi
                            |> Query.index n
                            |> expect
                    )
                    expects
                )
                ()
        ]
        ()


expectNewMessageBodyIsCleared :
    ScenarioProps m
    -> Scenario.Markup
    -> Scenario Flags m
expectNewMessageBodyIsCleared props markup =
    Scenario.sequence
        [ receiveOverwriteValueResponse props
            (markup
                |> Scenario.modifyContent
                    (String.append "(Port response)")
                |> Scenario.hide True
            )
        , expectNewMessageBody props { value = "" } markup
        ]


expectNewMessageBody :
    ScenarioProps m
    -> { value : String }
    -> Scenario.Markup
    -> Scenario Flags m
expectNewMessageBody props { value } markup =
    Scenario.expectValues props.session
        markup
        { layer = props.querySelf
        , expectation =
            \dict ->
                Dict.get NewMessage.keys.body dict
                    |> Expect.equal (Just value)
        }


expectNumOfMessageFieldItems :
    ScenarioProps m
    -> Int
    -> Scenario.Markup
    -> Scenario Flags m
expectNumOfMessageFieldItems props n markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.findAll
                        [ localClassSelector "messageField_body_item"
                        ]
                    |> Query.count (Expect.equal n)
        }


expectUserEnteredMessage :
    ScenarioProps m
    ->
        { user : ActiveUser
        }
    -> Scenario.Markup
    -> Scenario Flags m
expectUserEnteredMessage props { user } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.findAll
                        [ localClassSelector "messageField_body_item-userEntered"
                        , Selector.containing
                            [ localClassSelector "messageField_body_item_user"
                            , Selector.text user.displayName

                            -- , Selector.style "--active-user-color" user.color
                            ]
                        , Selector.containing
                            [ localClassSelector "messageField_body_item_text"
                            , Selector.text " has entered."
                            ]
                        ]
                    |> Query.count (Expect.greaterThan 0)
        }


expectUserLeftMessage :
    ScenarioProps m
    ->
        { user : ActiveUser
        }
    -> Scenario.Markup
    -> Scenario Flags m
expectUserLeftMessage props { user } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.findAll
                        [ localClassSelector "messageField_body_item-userLeft"
                        , Selector.containing
                            [ localClassSelector "messageField_body_item_user"
                            , Selector.exactText user.displayName

                            -- , Selector.style "--active-user-color" user.color
                            ]
                        , Selector.containing
                            [ localClassSelector "messageField_body_item_text"
                            , Selector.exactText " has left."
                            ]
                        ]
                    |> Query.count (Expect.greaterThan 0)
        }


expectUserMessage :
    ScenarioProps m
    ->
        { user : ActiveUser
        , value : String
        }
    -> Scenario.Markup
    -> Scenario Flags m
expectUserMessage props { user, value } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.findAll
                        [ localClassSelector "messageField_body_item-userMessage"
                        , Selector.containing
                            [ localClassSelector "messageField_body_item_user"
                            , Selector.exactText user.displayName

                            -- , Selector.style "--active-user-color" user.color
                            ]
                        , Selector.containing
                            [ localClassSelector "messageField_body_item_text"
                            , Selector.exactText <| ": " ++ value
                            ]
                        ]
                    |> Query.count (Expect.greaterThan 0)
        }


expectSubmitMessageButtonIsBusy :
    ScenarioProps m
    -> Bool
    -> Scenario.Markup
    -> Scenario Flags m
expectSubmitMessageButtonIsBusy props isBusy markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "newMessageForm_control_submit"
                        ]
                    |> Query.has
                        [ Selector.boolAttribute "aria-busy" isBusy
                        ]
        }



-- Helper promises


setDomValue : String -> String -> Promise m ()
setDomValue key value =
    Tepa.bind Tepa.currentLayerId <|
        \layerId ->
            [ RawDom.overwriteValue
                (domIdString { layerId = layerId, key = key })
                value
                |> Tepa.void
            , Tepa.setValue key value
            ]



-- Helper functions


localClass : String -> Mixin
localClass name =
    Mixin.class (pagePrefix ++ name)


localClassSelector : String -> Selector.Selector
localClassSelector name =
    Selector.class (pagePrefix ++ name)


domId : { layerId : String, key : String } -> Mixin
domId props =
    Mixin.id (domIdString props)


domIdString : { layerId : String, key : String } -> String
domIdString { layerId, key } =
    layerId ++ "--" ++ key


pagePrefix : String
pagePrefix =
    "page_chat--"
