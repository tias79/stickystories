module Main exposing (..)

import Browser

import Model exposing (init, update, Model, Msg)
import View exposing (view)

main : Program (Int, Int) Model Msg
main =
    Browser.element
        { view = view
        , init = init 
        , update = update
        , subscriptions = always <| Model.receivePort Model.ReceiveDocument
        }    

