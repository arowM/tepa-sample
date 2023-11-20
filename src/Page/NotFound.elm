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
import App.Path as Path
import AppUrl
import Dict
import Tepa exposing (Document, LayerMemory, Promise)
import Tepa.Html as Html
import Tepa.Mixin as Mixin exposing (Mixin)
import Widget.Toast as Toast


{-| Page memory.
-}
type alias Memory =
    LayerMemory MemoryLink MemoryBody


{-| Pointer to the external memory.
-}
type alias MemoryLink =
    { toast : Toast.Memory
    }


{-| Memory area for this page.
-}
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
view : Document
view =
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
