port module ScenarioWorker exposing (Flags, Memory, main)

import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Scenario exposing (MarkupConfig, sections)
import Tepa exposing (Program, Promise)
import Tepa.Scenario as Scenario


main : Program Flags Memory
main =
    Tepa.headless
        { init =
            \rawFlags ->
                Tepa.succeed ( JD.decodeValue flagsDecoder rawFlags, {} )
        , onLoad = onLoad
        }


type alias Memory =
    {}


port output_request : Tepa.PortRequest msg


port output_response : Tepa.PortResponse msg


type alias Flags =
    Result JD.Error MarkupConfig


flagsDecoder : JD.Decoder MarkupConfig
flagsDecoder =
    JD.map MarkupConfig <|
        JD.field "dev" JD.bool


onLoad : Flags -> Promise Memory ()
onLoad flags =
    case flags of
        Err err ->
            output <|
                JE.object
                    [ ( "error", JE.string <| JD.errorToString err )
                    ]

        Ok config ->
            let
                result =
                    Scenario.toMarkdown
                        { title = "Sample scenario"
                        , sections = sections config
                        , config = Scenario.en_US
                        }
            in
            case result of
                Ok res ->
                    output <|
                        JE.object
                            [ ( "value", JE.string res )
                            ]

                Err err ->
                    output <|
                        JE.object
                            [ ( "error", JE.string <| Scenario.stringifyInvalidMarkup err ) ]


output : Value -> Promise Memory ()
output v =
    Tepa.portRequest
        { request = output_request
        , response = output_response
        , portName = "output"
        , requestBody = v
        }
        |> Tepa.void
