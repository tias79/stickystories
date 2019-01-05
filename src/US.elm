module US exposing (BoardStage(..), Model, NewTaskState(..), Task, US, USStage(..), filterUsTasks, init, newTask, newUS, usBoardStage, encode)

import Json.Encode as Encode

type NewTaskState
    = Active Bool
    | NotAllowed


type alias US =
    { id : Int
    , name : String
    , description : String
    , tasks : List Task
    , newTaskState : NewTaskState
    , stage : USStage
    }


type alias Task =
    { id : Int
    , description : String
    , stage : BoardStage
    , active : Bool
    }


type alias Model =
    { nextUserStoryId : Int
    , nextTaskId : Int
    }


init : Model
init =
    { nextUserStoryId = 1, nextTaskId = 1 }


newUS : Model -> ( Model, US )
newUS model =
    ( { model
        | nextUserStoryId = model.nextUserStoryId + 1
      }
    , { id = model.nextUserStoryId
      , name = ""
      , description = ""
      , tasks = []
      , stage = Backlog
      , newTaskState = Active False
      }
    )


newTask : Model -> ( Model, Task )
newTask model =
    ( { model
        | nextTaskId = model.nextTaskId + 1
      }
    , { id = model.nextTaskId
      , description = ""
      , stage = ToDo
      , active = False
      }
    )


type BoardStage
    = ToDo
    | InProgress
    | Done


type USStage
    = Backlog
    | Board


usBoardStage : US -> BoardStage
usBoardStage us =
    if List.length us.tasks == 0 then
        ToDo

    else
            case us.tasks
                |> List.filter (\task -> task.stage == Done)
                |> List.length
                |> (==) (List.length us.tasks) of
                True -> Done
                False -> ToDo


filterUsTasks : US -> BoardStage -> US
filterUsTasks us stage =
    { us | tasks = List.filter (\task -> task.stage == stage) us.tasks }


encode : US -> Encode.Value
encode us = Encode.object [ ("id", Encode.int us.id), ("name", Encode.string us.name), ("description", Encode.string us.description)]