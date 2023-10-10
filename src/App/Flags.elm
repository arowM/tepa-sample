module App.Flags exposing
    ( Flags
    , fromValue
    )

{-| Flags

@docs Flags
@docs fromValue

-}

import Json.Encode exposing (Value)


{-| -}
type alias Flags =
    {}


{-| -}
fromValue : Value -> Flags
fromValue _ =
    {}
