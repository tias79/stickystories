module USTask exposing (Id, T, new, Model, init, count, move, boardStage, updateBoardStage, updateTaskBoardStage, updateDescription, updateActive, tasks, toDeltaJson)


import Board as Board
import US as US
import Set exposing (Set)
import List.Extra
import Json.Encode as Encode
import EntityUUID
import Dict


type alias Id = EntityUUID.T


type alias T =
    { id : Id
    , description : String
    , stage : Board.Stage
    , active : Bool
    , usId : US.Id
    }


type alias Model =
    {
        uuidModel : EntityUUID.Model,
        tasks : Dict.Dict String T
    }


init : Int -> Model
init initialSeed =
    {
        uuidModel = EntityUUID.init initialSeed,
        tasks = Dict.empty
    }


new : Model -> US.Id -> (T, Model)
new model usId =
    let
        (newUUID, newUUIDModel) = EntityUUID.generate model.uuidModel
        newTask = { id = newUUID
                , description = ""
                , stage = Board.ToDo
                , active = False
                , usId = usId
            }
    in
        ( newTask, { model
            | uuidModel = newUUIDModel
            , tasks = Dict.insert (EntityUUID.toString newTask.id) newTask model.tasks
        } )


count : Model -> US.Id -> Int
count model usId = Dict.size model.tasks


move : Model -> Id -> US.Id -> Model
move model taskId targetUSId =
    update model taskId (\task -> { task | usId = targetUSId })
        |> Tuple.second


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
updateBoardStage model usId stage = { model | tasks = Dict.map (\_ task -> if task.usId == usId then { task | stage = stage} else task) model.tasks }


updateTaskBoardStage : Model -> Id -> Board.Stage -> Model
updateTaskBoardStage model taskId stage = update model taskId (\task -> { task | stage = stage })
        |> Tuple.second


update : Model -> Id -> (T -> T) -> (Maybe T, Model)
update model taskId modifier =
    let
        uuid = (EntityUUID.toString taskId)
        maybeTask = Dict.get uuid model.tasks
            |> Maybe.map modifier
    in
        (maybeTask, case maybeTask of
            Just task -> { model | tasks = Dict.insert uuid task model.tasks }
            Nothing -> model)


updateDescription : Model -> Id -> String -> (Maybe T, Model)
updateDescription model taskId description = update model taskId (\task -> { task | description = description })


updateActive : Model -> Id -> Bool -> (Maybe T, Model)
updateActive model taskId active = update model taskId (\task -> { task | active = active })


tasks : Model -> US.Id -> List T 
tasks model usId =
        Dict.values model.tasks
            |> List.filter (\task -> task.usId == usId)


toDeltaJson : Model -> T -> Encode.Value
toDeltaJson model task =
        let 
            maybeOrigUs = Dict.get (EntityUUID.toString task.id) model.tasks
            id = ("id", EntityUUID.toJson task.id)
            description = ("description", Encode.string task.description)
            stage = ("stage", case task.stage of
                Board.ToDo -> Encode.string "ToDo"
                Board.InProgress -> Encode.string "InProgress"
                Board.Done -> Encode.string "Done")
        in
            case maybeOrigUs of
                Nothing ->
                    Encode.object [ id, description, stage ]
                Just origUs ->
                    Encode.object (
                        [ id ]
                        ++ if task.description /= origUs.description then [description] else []
                        ++ if task.stage /= origUs.stage then [stage] else []
                        )