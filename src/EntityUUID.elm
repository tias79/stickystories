module EntityUUID exposing(Model, init, generate, T, toJson, toString, decoder, nil)

import Random
import UUID
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra


type alias Model =
    {
        seed : Random.Seed
    }


type alias T = UUID.UUID


init : Int -> Model
init seed =
    {
        seed = Random.initialSeed seed
    }


generate : Model -> (T, Model)
generate model = 
    let
        (uuid, newSeed) = Random.step (UUID.generator) model.seed
    in
        (uuid, {model | seed = newSeed})


toJson : T -> Encode.Value
toJson uuid = Encode.string <| UUID.toString uuid


decoder : Decode.Decoder T
decoder =
    Decode.string
        |> Decode.andThen (DecodeExtra.fromResult << UUID.fromString)


toString : T -> String
toString uuid = UUID.toString uuid


nil : T
nil = UUID.nil