module USTask exposing (T, new, Model, init)


import Board as Board


type alias Id = Int


type alias T =
    { id : Id
    , description : String
    , stage : Board.Stage
    , active : Bool
    }


type alias Model =
    { nextTaskId : Id
    }


init : Model
init =
    { nextTaskId = 1 }


new : Model -> ( Model, T )
new model =
    ( { model
        | nextTaskId = model.nextTaskId + 1
      }
    , { id = model.nextTaskId
      , description = ""
      , stage = Board.ToDo
      , active = False
      }
    )