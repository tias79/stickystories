module USTask exposing (Id, T, new, Model, init, count, move, boardStage, updateBoardStage, updateDescription, updateActive, tasks, toJson)


import Board as Board
import US as US
import Set exposing (Set)
import List.Extra
import Json.Encode as Encode


type alias Id = Int


type alias T =
    { id : Id
    , description : String
    , stage : Board.Stage
    , active : Bool
    , usId : US.Id
    }


type alias Model =
    {
        nextTaskId : Id,
        tasks : List T
    }


init : Model
init =
    {
        nextTaskId = 1,
        tasks = []
    }


new : Model -> US.Id -> (T, Model)
new model usId =
    let
        newTask = { id = model.nextTaskId
                , description = ""
                , stage = Board.ToDo
                , active = False
                , usId = usId
            }
    in
        ( newTask, { model
            | nextTaskId = model.nextTaskId + 1
            , tasks = model.tasks ++
                [ newTask ]
        } )


count : Model -> US.Id -> Int
count model usId =
    List.length <| List.filter (\task -> task.usId == usId) model.tasks


move : Model -> Id -> US.Id-> Model
move model taskId targetUSId =
    ( { model 
        | tasks =
            List.Extra.updateIf
                (\task -> task.id == taskId)
                (\task -> { task | usId = targetUSId } )
                model.tasks
    } )


boardStage : List T -> Board.Stage
boardStage list =
    if List.length list == 0 then
        Board.ToDo

    else
        case list
            |> List.filter (\task -> task.stage == Board.Done)
            |> List.length
            |> (==) (List.length list) of
            True -> Board.Done
            False -> Board.ToDo


updateBoardStage : Model -> US.Id -> Board.Stage -> Model
updateBoardStage model usId stage = { model | tasks = List.Extra.updateIf (\task -> task.usId == usId) (\task -> { task | stage = stage}) model.tasks }


update : Model -> Id -> (T -> T) -> (Maybe T, Model)
update model taskId modifier =
    let
        matchingTask = List.head (List.filter (\task -> task.id == taskId) model.tasks)
        newTasks = List.map (\task -> if task.id == taskId then modifier task else task) model.tasks
    in
        ( matchingTask, { model | tasks = newTasks } )


updateDescription : Model -> Id -> String -> (Maybe T, Model)
updateDescription model taskId description = update model taskId (\task -> { task | description = description })


updateActive : Model -> Id -> Bool -> (Maybe T, Model)
updateActive model taskId active = update model taskId (\task -> { task | active = active })


tasks : Model -> US.Id -> List T 
tasks model usId =
        model.tasks
            |> List.filter (\task -> task.usId == usId)


toJson : T -> Encode.Value
toJson task = Encode.object [
        ("id", Encode.int task.id),
        ("description", Encode.string task.description),
        ("stage", case task.stage of
            Board.ToDo -> Encode.string "ToDo"
            Board.InProgress -> Encode.string "InProgress"
            Board.Done -> Encode.string "Done"),
        ("usId", Encode.int task.usId)
        ]