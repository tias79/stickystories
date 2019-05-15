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


new : Model -> US.Id -> Model
new model usId =
    ( { model
        | nextTaskId = model.nextTaskId + 1
        , tasks = model.tasks ++
            [{ id = model.nextTaskId
                , description = ""
                , stage = Board.ToDo
                , active = False
                , usId = usId
            }]
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


update : Model -> Id -> (T -> T) -> Model
update model taskId modifier =
    ( { model | tasks = List.Extra.updateIf
                        (\task -> task.id == taskId)
                        modifier
                        model.tasks
    } )


updateDescription : Model -> Id -> String -> Model
updateDescription model taskId description = update model taskId (\task -> { task | description = description })


updateActive : Model -> Id -> Bool -> Model
updateActive model taskId active = update model taskId (\task -> { task | active = active })


tasks : Model -> US.Id -> List T 
tasks model usId =
        model.tasks
            |> List.filter (\task -> task.usId == usId)


encode : Maybe T -> T -> Maybe T -> Encode.Value
encode prevTask curTask nextTask =
    Encode.object [
        ("id", Encode.int curTask.id),
        ("prevId", case prevTask of
            Nothing -> Encode.int -1
            Just task -> Encode.int task.id),
        ("nextId", case nextTask of
            Nothing -> Encode.int -1
            Just task -> Encode.int task.id),
        ("description", Encode.string curTask.description),
        ("stage", case curTask.stage of
            Board.ToDo -> Encode.string "ToDo"
            Board.InProgress -> Encode.string "InProgress"
            Board.Done -> Encode.string "Done"),
        ("usId", Encode.int curTask.usId)
        ]


toJson : Model -> Encode.Value
toJson model = toJsonRec Nothing model.tasks |> Encode.list (\x -> x)


toJsonRec : Maybe T -> List T -> List Encode.Value
toJsonRec prevUs tasklist =
    let
        curUs = List.head tasklist
        tail = Maybe.withDefault [] (List.tail tasklist)
        nextUs = tail |> List.head
    in    
        case curUs of
            Nothing -> []
            Just us -> [ encode prevUs us nextUs ] ++ toJsonRec curUs tail