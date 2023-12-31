module App.Session exposing
    ( Session
    , Profile
    , LuckyHay(..)
    , randomLuckyHay
    )

{-|

@docs Session
@docs Profile
@docs LuckyHay
@docs randomLuckyHay

-}

import Tepa.Random as Random
import Widget.Toast as Toast


{-| Application-wide state.
-}
type alias Session =
    { toast : Toast.Memory
    , luckyHay : LuckyHay
    , mprofile : Maybe Profile
    }


{-| Data for logged in user.
-}
type alias Profile =
    { id : String
    , name : String
    }


{-| Random lucky grass hay.
-}
type LuckyHay
    = LuckyHayTimothy
    | LuckyHayOat
    | LuckyHayAlfalfa
    | LuckyHayOrchard
    | LuckyHayBermuda


{-| Standard spec for random lucky grass hay.
-}
randomLuckyHay : Random.Spec LuckyHay
randomLuckyHay =
    Random.uniform "LuckyHay"
        LuckyHayTimothy
        [ LuckyHayOat
        , LuckyHayAlfalfa
        , LuckyHayOrchard
        , LuckyHayBermuda
        ]
