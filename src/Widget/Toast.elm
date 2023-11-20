module Widget.Toast exposing
    ( Memory
    , init
    , view
    , ClosedBy(..)
    , pushWarning
    , pushError
    , scenario
    , ScenarioSet
    , ScenarioProps
    , toastTimeout
    , toastFadeOutDuration
    )

{-| Widget for toast popup.


# Core

@docs Memory
@docs init
@docs view
@docs ClosedBy


# Methods

@docs pushWarning
@docs pushError


# Scenario

@docs scenario
@docs ScenarioSet
@docs ScenarioProps


# Constants

@docs toastTimeout
@docs toastFadeOutDuration

-}

import App.Flags exposing (Flags)
import App.ZIndex as ZIndex
import Expect
import Tepa exposing (Layer, Promise)
import Tepa.Html as Html exposing (Html)
import Tepa.HtmlSelector as Selector
import Tepa.Mixin as Mixin exposing (Mixin)
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Stream as Stream
import Tepa.Time as Time
import Test.Html.Event as HtmlEvent
import Test.Html.Query as HtmlQuery



-- Constants


{-| How long it takes for the toast pop-up to start disappearing.
-}
toastTimeout : Int
toastTimeout =
    10000


{-| The duration of the effect for disappearing a toast item.
-}
toastFadeOutDuration : Int
toastFadeOutDuration =
    350



-- Memory


{-| -}
type Memory
    = Memory Memory_


type alias Memory_ =
    { items : List ToastItemMemory -- reversed
    , nextItemId : Int
    }


type MessageType
    = ErrorMessage
    | WarningMessage


messageTypeCode : MessageType -> String
messageTypeCode type_ =
    case type_ of
        ErrorMessage ->
            "error"

        WarningMessage ->
            "warning"


{-| -}
init : Promise m Memory
init =
    Tepa.succeed <|
        Memory
            { items = []
            , nextItemId = 0
            }


{-| Represents the reason why the popup is closed.

  - `ClosedByUser`: Uesr clicked the close button
  - `ClosedByTimeout`: The popup was timed out

-}
type ClosedBy
    = ClosedByUser
    | ClosedByTimeout



-- Methods


{-| Show warning message.
This promise blocks subsequent processes untill the item is closed.
-}
pushWarning : String -> Promise Memory ClosedBy
pushWarning =
    pushItem WarningMessage


{-| Show error message.
This promise blocks subsequent processes untill the item is closed.
-}
pushError : String -> Promise Memory ClosedBy
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Promise Memory ClosedBy
pushItem type_ str =
    Tepa.bindAndThen Tepa.currentState <|
        \(Memory curr) ->
            let
                newItemId =
                    curr.nextItemId
            in
            (Tepa.modify <|
                \(Memory m) ->
                    Memory
                        { m
                            | items =
                                { id = newItemId
                                , isHidden = False
                                , messageType = type_
                                , content = str
                                }
                                    :: m.items
                            , nextItemId = m.nextItemId + 1
                        }
            )
                |> Tepa.andThen
                    (\_ ->
                        toastItemProcedure
                            { itemId = newItemId
                            }
                    )



-- ToastItem


type alias ToastItemMemory =
    { id : Int
    , isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedure :
    { itemId : Int
    }
    -> Promise Memory ClosedBy
toastItemProcedure param =
    let
        modifyToastItem : (ToastItemMemory -> ToastItemMemory) -> Promise Memory ()
        modifyToastItem f =
            Tepa.modify <|
                \(Memory m) ->
                    Memory
                        { m
                            | items =
                                List.map
                                    (\item ->
                                        if item.id == param.itemId then
                                            f item

                                        else
                                            item
                                    )
                                    m.items
                        }
    in
    Tepa.viewEventStream
        { key = (keys { itemId = param.itemId }).toastItemClose
        , type_ = "click"
        }
        |> Tepa.andThen
            (Stream.awaitFirstWithTimeout toastTimeout)
        |> Tepa.map
            (\ma ->
                case ma of
                    Nothing ->
                        ClosedByTimeout

                    Just () ->
                        ClosedByUser
            )
        |> Tepa.andThen
            (\closedBy ->
                Tepa.sequence
                    [ modifyToastItem
                        (\m -> { m | isHidden = True })
                    , Time.sleep toastFadeOutDuration
                    , Tepa.modify <|
                        \(Memory m) ->
                            Memory
                                { m | items = List.filter (\item -> item.id /= param.itemId) m.items }
                    ]
                    |> Tepa.map (\_ -> closedBy)
            )



-- View


{-| -}
view :
    { state : Memory
    }
    -> Tepa.ViewContext
    -> Html
view param context =
    let
        (Memory state) =
            param.state
    in
    Html.keyed "div"
        [ localClass "toast"
        , Mixin.style "--zindex" <| String.fromInt ZIndex.toast
        ]
        (List.reverse state.items
            |> List.map
                (\item ->
                    ( String.fromInt item.id
                    , toastItemView
                        { form = item
                        }
                        context
                    )
                )
        )


toastItemView :
    { form : ToastItemMemory
    }
    -> Tepa.ViewContext
    -> Html
toastItemView param { setKey } =
    Html.div
        [ localClass "toast_item"
        , localClass <| "toast_item-" ++ messageTypeCode param.form.messageType
        , Mixin.attribute "role" "dialog"
        , Mixin.boolAttribute "aria-hidden" param.form.isHidden
        , Mixin.style "--disappearing-duration" (String.fromInt toastFadeOutDuration ++ "ms")
        ]
        [ Html.div
            [ localClass "toast_item_body"
            ]
            [ Html.text param.form.content
            ]
        , Html.button
            [ localClass "toast_item_close"
            , Mixin.attribute "type" "button"
            , setKey (keys { itemId = param.form.id }).toastItemClose
            ]
            [ Html.text "×"
            ]
        ]


keys :
    { itemId : Int
    }
    ->
        { toastItemClose : String
        }
keys { itemId } =
    { toastItemClose = "toastItemClose-" ++ String.fromInt itemId
    }



-- Scenario


{-| -}
type alias ScenarioSet m =
    { expectWarningMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectErrorMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectDisappearingWarningMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectDisappearingErrorMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , expectNoWarningMessages : Scenario.Markup -> Scenario Flags m
    , expectNoErrorMessages : Scenario.Markup -> Scenario Flags m
    , expectNoMessages : Scenario.Markup -> Scenario Flags m
    , closeWarningsByMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    , closeErrorsByMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario Flags m
    }


{-| -}
type alias ScenarioProps m =
    { querySelf : Layer m -> Maybe (Layer Memory)
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m -> ScenarioSet m
scenario props =
    { expectWarningMessage = expectMessage props WarningMessage
    , expectErrorMessage = expectMessage props ErrorMessage
    , expectDisappearingWarningMessage = expectDisappearingMessage props WarningMessage
    , expectDisappearingErrorMessage = expectDisappearingMessage props ErrorMessage
    , expectNoWarningMessages =
        expectNoMessages props
            ("toast_item-" ++ messageTypeCode WarningMessage)
    , expectNoErrorMessages =
        expectNoMessages props
            ("toast_item-" ++ messageTypeCode ErrorMessage)
    , expectNoMessages =
        expectNoMessages props
            "toast_item"
    , closeWarningsByMessage =
        closeByMessage props
            WarningMessage
    , closeErrorsByMessage =
        closeByMessage props
            ErrorMessage
    }


expectMessage :
    ScenarioProps m
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario Flags m
expectMessage props messageType { message } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector <| "toast_item-" ++ messageTypeCode messageType
                        ]
                    |> HtmlQuery.keep
                        (Selector.all
                            [ localClassSelector "toast_item_body"
                            , Selector.exactText message
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectDisappearingMessage :
    ScenarioProps m
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario Flags m
expectDisappearingMessage props messageType { message } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector <| "toast_item-" ++ messageTypeCode messageType
                        , Selector.attribute "aria-hidden" "true"
                        ]
                    |> HtmlQuery.keep
                        (Selector.all
                            [ localClassSelector "toast_item_body"
                            , Selector.exactText message
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectNoMessages :
    ScenarioProps m
    -> String
    -> Scenario.Markup
    -> Scenario Flags m
expectNoMessages props itemClassname markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector itemClassname
                        ]
                    |> HtmlQuery.count (Expect.equal 0)
        }


closeByMessage :
    ScenarioProps m
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario Flags m
closeByMessage props messageType { message } markup =
    Scenario.userOperation props.session
        markup
        { query =
            HtmlQuery.find
                [ localClassSelector "toast_item"
                , localClassSelector <|
                    "toast_item-"
                        ++ messageTypeCode messageType
                , Selector.attribute "aria-hidden" "false"
                , Selector.containing
                    [ localClassSelector "toast_item_body"
                    , Selector.exactText message
                    ]
                ]
                >> HtmlQuery.children
                    [ localClassSelector "toast_item_close"
                    ]
                >> HtmlQuery.first
        , operation = HtmlEvent.click
        }



-- Helper functions


localClass : String -> Mixin
localClass name =
    Mixin.class (classPrefix ++ name)


localClassSelector : String -> Selector.Selector
localClassSelector name =
    Selector.class (classPrefix ++ name)


classPrefix : String
classPrefix =
    "widget_toast--"
