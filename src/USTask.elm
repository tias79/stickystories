module USTask exposing (Id, T, new, Model, init, count, move, boardStage, updateBoardStage, updateTaskBoardStage, updateDescription, updateActive, tasks, toDeltaJson, fromJson)


import Board as Board
import US as US
import Set exposing (Set)
import List.Extra
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
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


maybeToList : Maybe a -> List a
maybeToList maybe = case maybe of
    Nothing -> []
    Just x -> x :: []


count : Model -> US.Id -> Int
count model usId = Dict.size model.tasks


move : Model -> Id -> US.Id -> (List T, Model)
move model taskId targetUSId =
    update model taskId (\task -> { task | usId = targetUSId })


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


insert : Dict.Dict String T -> List T -> Dict.Dict String T
insert dict values = 
    let
        maybeTask = List.head values
        tail = List.tail values
    in 
        case values of
            [] -> dict
            h::t -> insert (Dict.insert (EntityUUID.toString h.id) h dict) t


updateBoardStage : Model -> US.Id -> Board.Stage -> (List T, Model)
updateBoardStage model usId stage = 
    let
        filteredTasks = model.tasks |> Dict.filter (\_ task -> task.usId == usId) |> Dict.values
    in
        (filteredTasks, { model | tasks = Dict.map (\_ task -> if task.usId == usId then { task | stage = stage} else task) model.tasks })


updateTaskBoardStage : Model -> Id -> Board.Stage -> Model
updateTaskBoardStage model taskId stage = update model taskId (\task -> { task | stage = stage })
        |> Tuple.second


update : Model -> Id -> (T -> T) -> (List T, Model)
update model taskId modifier =
    let
        uuid = (EntityUUID.toString taskId)
        updatedTasks = Dict.get uuid model.tasks
            |> Maybe.map modifier
            |> maybeToList
    in
        (updatedTasks, { model | tasks = insert model.tasks updatedTasks })


updateDescription : Model -> Id -> String -> (List T, Model)
updateDescription model taskId description = update model taskId (\task -> { task | description = description })


updateActive : Model -> Id -> Bool -> (List T, Model)
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


stageDecoder : Decode.Decoder Board.Stage
stageDecoder = Decode.string
        |> Decode.map
        (\str ->
            case str of
                "InProgress" -> Board.InProgress
                "Done" -> Board.Done
                _ -> Board.ToDo)

taskDecoder : T -> Decode.Decoder T
taskDecoder template =
    Decode.map5
        T
        (Decode.field "id" EntityUUID.decoder)
        (Decode.oneOf [(Decode.field "description" Decode.string), (Decode.succeed template.description)])
        (Decode.oneOf [(Decode.field "stage" stageDecoder), (Decode.succeed template.stage)])
        (Decode.succeed False)
        (Decode.oneOf [(Decode.field "usId" EntityUUID.decoder), (Decode.succeed template.usId)])


fromJson : Model -> EntityUUID.T -> Encode.Value -> Model
fromJson model id json = 
    let
        templateTaskMaybe = Dict.get (EntityUUID.toString id) model.tasks
    in
        case templateTaskMaybe of
            Just templateTask ->
                let
                    maybeNewTask = Decode.decodeValue (taskDecoder templateTask) json
                in
                    case maybeNewTask of
                        Err _ -> model
                        Ok newUS ->
                            let
                                uuid = (EntityUUID.toString newUS.id)
                                maybeUs = Dict.get uuid model.tasks
                            in                
                                case maybeUs of
                                    Nothing -> 
                                            { model
                                                | tasks = Dict.insert uuid newUS model.tasks
                                            }
                                    Just us ->
                                        { model | tasks = Dict.insert uuid us model.tasks }
            Nothing -> model