module EntityUUID exposing(Model, init, generate, T, toJson)

import Random
import UUID
import Json.Encode as Encode


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