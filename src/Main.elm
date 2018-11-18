module Main exposing (..)

import Browser

import Model exposing (init, update, Model, Msg)
import View exposing (view)

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
