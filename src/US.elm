module US exposing (Id, Model, NewTaskState(..), T, USStage(..), filterUsTasks, init, newUS, usBoardStage)

import Board as Board
import USTask as Task
import Json.Encode as Encode


type NewTaskState
    = Active Bool
    | NotAllowed


type alias Id = Int


type alias T =
    { id : Id
    , name : String
    , description : String
    , tasks : List Task.T
    , newTaskState : NewTaskState
    , stage : USStage
    }


type alias Model =
    { nextUserStoryId : Id
    }


init : Model
init =
    { nextUserStoryId = 1 }


newUS : Model -> ( Model, T )
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








type USStage
    = Backlog
    | Board


usBoardStage : T -> Board.Stage
usBoardStage us =
    if List.length us.tasks == 0 then
        Board.ToDo

    else
        case us.tasks
            |> List.filter (\task -> task.stage == Board.Done)
            |> List.length
            |> (==) (List.length us.tasks) of
            True -> Board.Done
            False -> Board.ToDo


filterUsTasks : T -> Board.Stage -> T
filterUsTasks us stage =
    { us | tasks = List.filter (\task -> task.stage == stage) us.tasks }