module US exposing (Id, Model, NewTaskState(..), T, USStage(..), init, new, updateName, updateDescription, updateUSStage, updateNewTaskState, move, filter, count, toJson)

import Board as Board
import Set exposing (Set)
import List.Extra
import Json.Encode as Encode
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


update : Model -> Id -> (T -> T) -> (Maybe T, Model)
update model usId modifier =
    let
        uuid = (EntityUUID.toString usId)
        maybeUs = Dict.get uuid model.userStories
    in
        (maybeUs, case maybeUs of
            Just us -> { model | userStories = Dict.insert uuid (modifier us) model.userStories }
            Nothing -> model)


updateName : Model -> Id -> String -> (Maybe T, Model)
updateName model usId name = update model usId (\us -> { us | name = name })


updateDescription : Model -> Id -> String -> (Maybe T, Model)
updateDescription model usId description = update model usId (\us -> { us | description = description })


updateUSStage : Model -> Id -> USStage -> (Maybe T, Model)
updateUSStage model usId stage = update model usId (\us -> { us | stage = stage })


updateNewTaskState : Model -> Id -> NewTaskState -> Model
updateNewTaskState model usId state = Tuple.second <| update model usId (\us -> { us | newTaskState = state })


type USStage
    = Backlog
    | Board


move : Model -> Id -> Id -> Model
move model srcId targetId =
    let
        src = Dict.get (EntityUUID.toString srcId) model.userStories
        target = Dict.get (EntityUUID.toString targetId) model.userStories
    in
        if srcId == targetId then
            model
        else
            case src of
                Nothing -> model
                Just srcUs ->
                    case target of
                        Nothing -> model
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
                            in
                                { model | userStories = Dict.insert (EntityUUID.toString srcId) {srcUs | order = newOrder} model.userStories}


filter : Model -> USStage -> List T
filter model stage = Dict.values model.userStories
                        |> List.filter (\us -> us.stage == stage)
                        |> List.sortBy .order 


count : Model -> Int
count model =
    Dict.size model.userStories


toJson : T -> Encode.Value
toJson us = Encode.object [
        ("id", EntityUUID.toJson us.id),
        ("name", Encode.string us.name),
        ("description", Encode.string us.description),
        ("stage", case us.stage of
            Backlog -> Encode.string "Backlog"
            Board -> Encode.string "Board"),
        ("order", Encode.float us.order)
        ]