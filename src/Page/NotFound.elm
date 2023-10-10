module Page.NotFound exposing
    ( Memory
    , MemoryLink
    , MemoryBody
    , init
    , leave
    , onLoad
    , view
    )

{-| Not found page.

@docs Memory
@docs MemoryLink
@docs MemoryBody
@docs init
@docs leave
@docs onLoad
@docs view

-}

import App.Bucket exposing (Bucket)
import App.Flags exposing (Flags)
import App.Path as Path
import AppUrl
import Dict
import Tepa exposing (Document, Layer, Promise)
import Tepa.Html as Html
import Tepa.Mixin as Mixin exposing (Mixin)
import Widget.Toast as Toast


{-| Page memory.

    - link: Pointer to the external memory.
    - body: Memory area for this page.

-}
type alias Memory =
    { link : MemoryLink
    , body : MemoryBody
    }


{-| -}
type alias MemoryLink =
    { toast : Toast.Memory
    }


{-| -}
type alias MemoryBody =
    {}


{-| -}
init : Promise m MemoryBody
init =
    Tepa.succeed
        {}


{-| Procedure for releasing resources, saving scroll position, and so on.
-}
leave : Promise Memory ()
leave =
    Tepa.none



-- View


{-| -}
view : Flags -> Layer Memory -> Document
view _ =
    Tepa.layerView <|
        \_ ->
            { title = "Sample App | Not Found"
            , body =
                [ Html.div
                    [ localClass "page"
                    ]
                    [ Html.div
                        [ localClass "mainMessage"
                        ]
                        [ Html.text "Page Not Found."
                        ]
                    , Html.a
                        [ Mixin.attribute "href" <|
                            AppUrl.toString
                                { path =
                                    [ Path.prefix
                                    ]
                                , queryParameters = Dict.empty
                                , fragment = Nothing
                                }
                        , localClass "homeLink"
                        ]
                        [ Html.text "Home"
                        ]
                    ]
                ]
            }



-- Procedures


{-| -}
onLoad : Bucket -> Promise Memory ()
onLoad _ =
    Tepa.none



-- Helper functions


localClass : String -> Mixin
localClass name =
    Mixin.class (pagePrefix ++ name)


pagePrefix : String
pagePrefix =
    "page_notFound--"
