module App.Bucket exposing (Bucket)

{-| Data to be bucket relayed within the app

@docs Bucket

-}

import App.Flags exposing (Flags)
import AppUrl exposing (AppUrl)
import Tepa exposing (NavKey)


{-| -}
type alias Bucket =
    { flags : Flags
    , requestPath : AppUrl
    , key : NavKey
    }
