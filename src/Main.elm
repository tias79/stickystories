module Main exposing (..)

import Browser
import Model exposing (Model, Msg, init, update)
import View exposing (view)


main : Program ( Int, Int ) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always <| Model.receivePort Model.ReceiveDocument
        }
