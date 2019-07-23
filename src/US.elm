module US exposing (Id, Model, NewTaskState(..), T, USStage(..), init, new, updateName, updateDescription, updateUSStage, updateNewTaskState, move, filter, count, toDeltaJson, fromJson)

import Board as Board
import Set exposing (Set)
import List.Extra
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Extra as DecodeExtra
import Random
import EntityUUID
import Dict


type NewTaskState
    = Active Bool
    | NotAllowed


type alias Id = EntityUUID.T


type alias T =
    { id : Id
    , name : String
    , description : String
    , newTaskState : NewTaskState
    , stage : USStage
    , order : Float
    }


type alias Model =
    {
        uuidModel : EntityUUID.Model,
        userStories : Dict.Dict String T
    }


init : Int -> Model
init seed =
    {
        uuidModel = EntityUUID.init seed,
        userStories = Dict.empty
    }


new : Model -> (T, Model)
new model =
    let
        (newUUID, newUUIDModel) = EntityUUID.generate model.uuidModel
        newUS = { id = newUUID
                , name = ""
                , description = ""
                , stage = Backlog
                , newTaskState = Active False
                , order = case List.sortBy .order (Dict.values model.userStories) |> List.reverse |> List.head of
                    Just us -> us.order + 10.0 |> floor |> toFloat
                    Nothing -> 10.0
            }
    in
        ( newUS, { model
            | userStories = Dict.insert (EntityUUID.toString newUS.id) newUS model.userStories
            , uuidModel = newUUIDModel
        })


update : Model -> Id -> (T -> T) -> (List T, Model)
update model usId modifier =
    let
        uuid = (EntityUUID.toString usId)
        maybeUs = Dict.get uuid model.userStories
            |> Maybe.map modifier
    in
        case maybeUs of
            Just us -> ([us], { model | userStories = Dict.insert uuid us model.userStories })
            Nothing -> ([], model)


updateName : Model -> Id -> String -> (List T, Model)
updateName model usId name = update model (Debug.log "updateName usId" usId) (\us -> { us | name = name })


updateDescription : Model -> Id -> String -> (List T, Model)
updateDescription model usId description = update model (Debug.log "updateDescription usId" usId) (\us -> { us | description = description })


updateUSStage : Model -> Id -> USStage -> (List T, Model)
updateUSStage model usId stage = update model (Debug.log "updateUSStage usId" usId) (\us -> { us | stage = stage })


updateNewTaskState : Model -> Id -> NewTaskState -> Model
updateNewTaskState model usId state = Tuple.second <| update model (Debug.log "newTaskState usId" usId) (\us -> { us | newTaskState = state })


type USStage
    = Backlog
    | Board


move : Model -> Id -> Id -> (List T, Model)
move model srcId targetId =
    let
        src = Dict.get (EntityUUID.toString srcId) model.userStories
        target = Dict.get (EntityUUID.toString targetId) model.userStories
    in
        if srcId == targetId then
            ([], model)
        else
            case src of
                Nothing -> ([], model)
                Just srcUs ->
                    case target of
                        Nothing -> ([], model)
                        Just targetUs ->
                            let
                                movingUp = srcUs.order > targetUs.order
                                targetOrder = targetUs.order
                                selectedUs = List.filter (\u -> if movingUp then u.order < targetOrder else u.order > targetOrder) (Dict.values model.userStories)
                                                    |> List.sortBy .order
                                adjacentOrder = (if movingUp then selectedUs else List.reverse selectedUs)
                                                    |> List.head
                                                    |> Maybe.map .order
                                                    |> Maybe.withDefault (if movingUp then 0 else targetOrder + 10)
                                newOrder = targetOrder + (adjacentOrder - targetOrder) / 2
                                updatedSrcUS = {srcUs | order = newOrder}
                            in
                                ([updatedSrcUS], { model | userStories = Dict.insert (EntityUUID.toString srcId) updatedSrcUS model.userStories})


filter : Model -> USStage -> List T
filter model stage = Dict.values model.userStories
                        |> List.filter (\us -> us.stage == stage)
                        |> List.sortBy .order


count : Model -> Int
count model =
    Dict.size model.userStories


toDeltaJson : Model -> T -> Encode.Value
toDeltaJson model us =
        let 
            maybeOrigUs = Dict.get (EntityUUID.toString us.id) model.userStories
            id = ("id", EntityUUID.toJson us.id)
            name = ("name", Encode.string us.name)
            description = ("description", Encode.string us.description)
            stage = ("stage", case us.stage of
                    Backlog -> Encode.string "Backlog"
                    Board -> Encode.string "Board")
            order = ("order", Encode.float us.order)
        in
            case maybeOrigUs of
                Nothing ->
                    Encode.object [ id, name, description, stage, order ]
                Just origUs ->
                    Encode.object (
                        [ id ]
                        ++ if us.name /= origUs.name then [name] else []
                        ++ if us.description /= origUs.description then [description] else []
                        ++ if (Debug.log "us.stage" us.stage) /= (Debug.log "origUs.stage" origUs.stage) then [stage] else []
                        ++ if us.order /= origUs.order then [order] else []
                        )


stageDecoder : Decode.Decoder USStage
stageDecoder = Decode.string
        |> Decode.map
        (\str ->
            case str of
                "Board" -> Board
                _ -> Backlog)


usDecoder : T -> Decode.Decoder T
usDecoder template =
    Decode.map6
        T
        (Decode.field "id" EntityUUID.decoder)
        (Decode.oneOf [(Decode.field "name" Decode.string), (Decode.succeed template.name)])
        (Decode.oneOf [(Decode.field "description" Decode.string), (Decode.succeed template.description)])
        (Decode.succeed (Active False))
        (Decode.oneOf [(Decode.field "stage" stageDecoder), (Decode.succeed template.stage)])
        (Decode.oneOf [(Decode.field "order" Decode.float), (Decode.succeed template.order)])


fromJson : Model -> EntityUUID.T -> Encode.Value -> Model
fromJson model id json = 
    let
        templateUS = case Dict.get (EntityUUID.toString id) model.userStories of
            Just us -> us
            Nothing -> Tuple.first (new model)
        maybeNewUS = Decode.decodeValue (usDecoder templateUS) json
    in
        case maybeNewUS of
            Err error -> Debug.log (Decode.errorToString error) model
            Ok newUS ->
                let
                    uuid = (EntityUUID.toString newUS.id)
                in                
                    { model
                        | userStories = Dict.insert uuid newUS model.userStories
                    }