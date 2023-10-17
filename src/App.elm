module App exposing
    ( Memory
    , Page(..)
    , ScenarioSet
    , main
    , props
    , scenario
    )

{-| Application main.

@docs Memory
@docs Page
@docs ScenarioSet
@docs main
@docs props
@docs scenario

-}

import App.Bucket exposing (Bucket)
import App.FetchProfile as FetchProfile
import App.Flags as Flags exposing (Flags)
import App.Path as Path
import App.Session as Session exposing (Session)
import AppUrl exposing (AppUrl)
import Dict
import Json.Encode exposing (Value)
import Page.Chat
import Page.Home
import Page.Login
import Page.NotFound
import Tepa exposing (Document, Layer, NavKey, Promise)
import Tepa.Html as Html
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Random as Random
import Tepa.Scenario as Scenario exposing (Scenario)
import Widget.Toast as Toast



-- App


{-| -}
main : Tepa.Program Flags Memory
main =
    Tepa.application props


{-| -}
props : Tepa.ApplicationProps Flags Memory
props =
    { init = init
    , onLoad = onLoad
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    , view = view
    , initView = initView
    }



-- Memory


{-| -}
type alias Memory =
    { session : Session
    , page : Page
    }


init : Value -> Promise () ( Flags, Memory )
init rawFlags =
    Tepa.bindAndThen2
        Toast.init
        (Random.request Session.randomLuckyHay)
    <|
        \toast luckyHay ->
            Tepa.succeed
                ( Flags.fromValue rawFlags
                , { session =
                        { toast = toast
                        , luckyHay = luckyHay
                        , mprofile = Nothing
                        }
                  , page = PageLoading
                  }
                )



-- Page


{-| -}
type Page
    = PageLoading
    | PageNotFound (Layer Page.NotFound.MemoryBody)
    | PageLogin (Layer Page.Login.MemoryBody)
    | PageHome (Layer Page.Home.MemoryBody)
    | PageChat (Layer Page.Chat.MemoryBody)


onPageNotFoundLayer : Promise Page.NotFound.Memory () -> Promise Memory (Tepa.ResultOnLayer ())
onPageNotFoundLayer =
    Tepa.onLayer
        { getLink =
            \{ session } ->
                Just
                    { toast = session.toast
                    }
        , setLink =
            \link ({ session } as m) ->
                { m
                    | session = { session | toast = link.toast }
                }
        , getBody =
            \m ->
                case m.page of
                    PageNotFound layerBody ->
                        Just layerBody

                    _ ->
                        Nothing
        , setBody =
            \layerBody m ->
                { m | page = PageNotFound layerBody }
        }


getPageLoginLayer : Memory -> Maybe (Layer Page.Login.MemoryBody)
getPageLoginLayer m =
    case m.page of
        PageLogin layer ->
            Just layer

        _ ->
            Nothing


onPageLoginLayer : Promise Page.Login.Memory () -> Promise Memory (Tepa.ResultOnLayer ())
onPageLoginLayer =
    Tepa.onLayer
        { getLink =
            \{ session } ->
                Just
                    { mprofile = session.mprofile
                    , toast = session.toast
                    }
        , setLink =
            \link ({ session } as m) ->
                { m
                    | session =
                        { session
                            | mprofile = link.mprofile
                            , toast = link.toast
                        }
                }
        , getBody = getPageLoginLayer
        , setBody =
            \layerBody m ->
                { m | page = PageLogin layerBody }
        }


getPageHomeLayer : Memory -> Maybe (Layer Page.Home.MemoryBody)
getPageHomeLayer m =
    case m.page of
        PageHome layer ->
            Just layer

        _ ->
            Nothing


onPageHomeLayer : Promise Page.Home.Memory () -> Promise Memory (Tepa.ResultOnLayer ())
onPageHomeLayer =
    Tepa.onLayer
        { getLink =
            \{ session } ->
                Maybe.map
                    (\profile ->
                        { profile = profile
                        , luckyHay = session.luckyHay
                        , toast = session.toast
                        }
                    )
                    session.mprofile
        , setLink =
            \link ({ session } as m) ->
                { m
                    | session =
                        { session
                            | mprofile = Just link.profile
                            , luckyHay = link.luckyHay
                            , toast = link.toast
                        }
                }
        , getBody = getPageHomeLayer
        , setBody =
            \layerBody m ->
                { m | page = PageHome layerBody }
        }


getPageChatLayer : Memory -> Maybe (Layer Page.Chat.MemoryBody)
getPageChatLayer m =
    case m.page of
        PageChat layer ->
            Just layer

        _ ->
            Nothing


onPageChatLayer : Promise Page.Chat.Memory () -> Promise Memory (Tepa.ResultOnLayer ())
onPageChatLayer =
    Tepa.onLayer
        { getLink =
            \{ session } ->
                Maybe.map
                    (\profile ->
                        { profile = profile
                        , toast = session.toast
                        }
                    )
                    session.mprofile
        , setLink =
            \link ({ session } as m) ->
                { m
                    | session =
                        { session
                            | mprofile = Just link.profile
                            , toast = link.toast
                        }
                }
        , getBody = getPageChatLayer
        , setBody =
            \layerBody m ->
                { m | page = PageChat layerBody }
        }



-- View


initView : Document
initView =
    pageLoadingView


view : Flags -> Memory -> Document
view flags state =
    let
        unexpectedState =
            { title = "Sample App"
            , body = [ Html.text "Unexpected error. Please contact us." ]
            }
    in
    case ( state.page, state.session.mprofile ) of
        ( PageLoading, _ ) ->
            pageLoadingView

        ( PageNotFound pageNotFound, _ ) ->
            Page.NotFound.view flags
                (Tepa.mapLayer
                    (\body ->
                        { link =
                            { toast = state.session.toast
                            }
                        , body = body
                        }
                    )
                    pageNotFound
                )

        ( PageLogin pageLogin, _ ) ->
            Page.Login.view flags
                (Tepa.mapLayer
                    (\body ->
                        { link =
                            { mprofile = state.session.mprofile
                            , toast = state.session.toast
                            }
                        , body = body
                        }
                    )
                    pageLogin
                )

        ( PageHome pageHome, Just profile ) ->
            Page.Home.view flags
                (Tepa.mapLayer
                    (\body ->
                        { link =
                            { profile = profile
                            , luckyHay = state.session.luckyHay
                            , toast = state.session.toast
                            }
                        , body = body
                        }
                    )
                    pageHome
                )

        ( PageHome _, Nothing ) ->
            unexpectedState

        ( PageChat pageChat, Just profile ) ->
            Page.Chat.view flags
                (Tepa.mapLayer
                    (\body ->
                        { link =
                            { profile = profile
                            , toast = state.session.toast
                            }
                        , body = body
                        }
                    )
                    pageChat
                )

        ( PageChat _, Nothing ) ->
            unexpectedState



-- -- PageLoading


pageLoadingView : Document
pageLoadingView =
    { title = "Sample App"
    , body = [ Html.text "Loading..." ]
    }



-- Procedures
-- -- Initialization


{-| -}
onLoad : Flags -> AppUrl -> NavKey -> Promise Memory ()
onLoad flags url key =
    pageProcedure
        { flags = flags
        , requestPath = url
        , key = key
        }


onUrlChange : Flags -> AppUrl -> NavKey -> Promise Memory ()
onUrlChange flags newUrl key =
    Tepa.sequence
        [ Tepa.syncAll
            [ Page.NotFound.leave
                |> onPageNotFoundLayer
                |> Tepa.void
            , Page.Login.leave
                |> onPageLoginLayer
                |> Tepa.void
            , Page.Home.leave
                |> onPageHomeLayer
                |> Tepa.void
            , Page.Chat.leave
                |> onPageChatLayer
                |> Tepa.void
            ]
        , pageProcedure
            { flags = flags
            , requestPath = newUrl
            , key = key
            }
        ]


assertLayerExist :
    { layerName : String
    }
    -> Promise m (Tepa.ResultOnLayer ())
    -> Promise m ()
assertLayerExist { layerName } =
    Tepa.andThen
        (\res ->
            case res of
                Tepa.SucceedOnLayer () ->
                    Tepa.succeed ()

                Tepa.BodyExpired ->
                    Tepa.succeed ()

                Tepa.LinkExpired ->
                    Tepa.assertionError <| "LinkExpired on " ++ layerName
        )


onUrlRequest : Flags -> Tepa.UrlRequest -> NavKey -> Promise Memory ()
onUrlRequest _ urlRequest key =
    case urlRequest of
        Tepa.InternalPath url ->
            Nav.pushPath key url

        Tepa.ExternalPage href ->
            Nav.load href



-- -- Page Controller


pageProcedure : Bucket -> Promise Memory ()
pageProcedure bucket =
    -- IGNORE TCO
    Tepa.bind Tepa.currentState <|
        \curr ->
            case ( curr.session.mprofile, Path.body bucket.requestPath ) of
                ( _, Just [ "login" ] ) ->
                    -- Users can access login page without sessions.
                    [ Tepa.bind
                        (Page.Login.init
                            |> Tepa.andThen Tepa.newLayer
                        )
                      <|
                        \layerBody ->
                            [ Tepa.modify <| \m -> { m | page = PageLogin layerBody }
                            , Page.Login.onLoad bucket
                                |> onPageLoginLayer
                                |> assertLayerExist
                                    { layerName = "PageLogin"
                                    }
                            ]
                    ]

                ( Nothing, _ ) ->
                    [ Tepa.bind FetchProfile.request <|
                        \response ->
                            let
                                onError =
                                    [ Nav.pushPath bucket.key
                                        { path = [ Path.prefix, "login" ]
                                        , queryParameters =
                                            Dict.fromList
                                                [ ( "back", [ AppUrl.toString bucket.requestPath ] )
                                                ]
                                        , fragment = Nothing
                                        }
                                    ]
                            in
                            case response of
                                FetchProfile.LoginRequiredResponse ->
                                    onError

                                FetchProfile.FatalErrorResponse ->
                                    onError

                                FetchProfile.TemporaryErrorResponse ->
                                    onError

                                FetchProfile.GoodResponse resp ->
                                    [ Tepa.modify <|
                                        \({ session } as m) ->
                                            { m
                                                | session = { session | mprofile = Just resp.profile }
                                            }
                                    , Tepa.lazy <| \_ -> pageProcedure bucket
                                    ]
                    ]

                ( Just _, Just [] ) ->
                    [ Tepa.bind
                        (Page.Home.init
                            |> Tepa.andThen Tepa.newLayer
                        )
                      <|
                        \layerBody ->
                            [ Tepa.modify <| \m -> { m | page = PageHome layerBody }
                            , Page.Home.onLoad bucket
                                |> onPageHomeLayer
                                |> assertLayerExist
                                    { layerName = "PageHome"
                                    }
                            ]
                    ]

                ( Just _, Just [ "chat" ] ) ->
                    [ Tepa.bind
                        (Page.Chat.init
                            |> Tepa.andThen Tepa.newLayer
                        )
                      <|
                        \layerBody ->
                            [ Tepa.modify <| \m -> { m | page = PageChat layerBody }
                            , Page.Chat.onLoad bucket
                                |> onPageChatLayer
                                |> assertLayerExist
                                    { layerName = "PageChat"
                                    }
                            ]
                    ]

                ( Just _, _ ) ->
                    [ Tepa.bind
                        (Page.NotFound.init
                            |> Tepa.andThen Tepa.newLayer
                        )
                      <|
                        \layerBody ->
                            [ Tepa.modify <| \m -> { m | page = PageNotFound layerBody }
                            , Page.NotFound.onLoad bucket
                                |> onPageNotFoundLayer
                                |> assertLayerExist
                                    { layerName = "PageNotFound"
                                    }
                            ]
                    ]



-- Scenario


{-| -}
type alias ScenarioSet =
    { login : Page.Login.ScenarioSet Memory
    , home : Page.Home.ScenarioSet Memory
    , chat : Page.Chat.ScenarioSet Memory
    , toast : Toast.ScenarioSet Memory
    , app :
        { receiveProfile :
            (() -> Maybe ( Http.Metadata, String ))
            -> Scenario.Markup
            -> Scenario Flags Memory
        , receiveRandomLuckyHay :
            { value : Session.LuckyHay
            }
            -> Scenario.Markup
            -> Scenario Flags Memory
        , fetchProfileEndpoint :
            { method : String
            , url : String
            }
        }
    }


{-| -}
scenario : Scenario.Session -> ScenarioSet
scenario session =
    { login =
        Page.Login.scenario
            { querySelf =
                Scenario.appLayer
                    |> Scenario.childLayer getPageLoginLayer
            , session = session
            }
    , home =
        Page.Home.scenario
            { querySelf =
                Scenario.appLayer
                    |> Scenario.childLayer getPageHomeLayer
            , session = session
            }
    , chat =
        Page.Chat.scenario
            { querySelf =
                Scenario.appLayer
                    |> Scenario.childLayer getPageChatLayer
            , session = session
            }
    , toast =
        Toast.scenario
            { querySelf =
                Tepa.mapLayer (\m -> m.session.toast) >> Just
            , session = session
            }
    , app =
        { receiveProfile = receiveProfile session
        , receiveRandomLuckyHay = receiveRandomLuckyHay session
        , fetchProfileEndpoint =
            { method = FetchProfile.method
            , url = FetchProfile.endpointUrl
            }
        }
    }


receiveProfile : Scenario.Session -> (() -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario Flags Memory
receiveProfile session toResponse markup =
    Scenario.httpResponse session
        markup
        { layer = Just
        , response =
            \rawRequest ->
                if rawRequest.url == FetchProfile.endpointUrl then
                    toResponse ()

                else
                    Nothing
        }


receiveRandomLuckyHay : Scenario.Session -> { value : Session.LuckyHay } -> Scenario.Markup -> Scenario Flags Memory
receiveRandomLuckyHay session { value } markup =
    Scenario.randomResponse session
        markup
        { layer = Just
        , spec = Session.randomLuckyHay
        , response = value
        }
