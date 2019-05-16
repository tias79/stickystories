module US exposing (Id, Model, NewTaskState(..), T, USStage(..), init, new, updateName, updateDescription, updateUSStage, updateNewTaskState, move, filter, count, toJson)

import Board as Board
import Set exposing (Set)
import List.Extra
import Json.Encode as Encode


type NewTaskState
    = Active Bool
    | NotAllowed


type alias Id = Int


type alias T =
    { id : Id
    , name : String
    , description : String
    , newTaskState : NewTaskState
    , stage : USStage
    }


type alias Model =
    {
        nextUserStoryId : Id,
        userStories : List T
    }


init : Model
init =
    {
        nextUserStoryId = 1,
        userStories = []
    }


new : Model -> (T, Model)
new model =
    let
        newUS = { id = model.nextUserStoryId
                , name = ""
                , description = ""
                , stage = Backlog
                , newTaskState = Active False
            }
    in
        ( newUS, { model
            | nextUserStoryId = model.nextUserStoryId + 1
            , userStories = model.userStories ++ [ newUS ]
        })


update : Model -> Id -> (T -> T) -> (Maybe T, Model)
update model usId modifier =
    let
        matchingUS = List.head (List.filter (\us -> us.id == usId) model.userStories)
        newUserStories = List.map (\us -> if us.id == usId then modifier us else us) model.userStories
    in
        ( matchingUS, { model | userStories = newUserStories } )


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
        targetMaybe =
            List.Extra.find (\x -> x.id == targetId) model.userStories
    in
    if srcId == targetId then
        model

    else
        { model | userStories =
        model.userStories
            |> List.filter (\us -> us.id /= srcId)
            |> List.map
                (\us ->
                    if us.id == targetId then
                        model.userStories
                            |> List.filter (\x -> x.id == srcId || x.id == targetId)
                            |> List.reverse

                    else
                        [ us ]
                )
            |> List.concat
            |> List.Extra.updateIf (\us -> us.id == srcId)
                (\srcUs ->
                    case targetMaybe of
                        Nothing ->
                            srcUs

                        Just targetUs ->
                            { srcUs | stage = targetUs.stage }
                )
        }


filter : Model -> USStage -> List T
filter model stage = model.userStories |> List.filter (\us -> us.stage == stage)


count : Model -> Int
count model =
    List.length model.userStories


toJson : T -> Encode.Value
toJson us = Encode.object [
        ("id", Encode.int us.id),
        ("name", Encode.string us.name),
        ("description", Encode.string us.description),
        ("stage", case us.stage of
            Backlog -> Encode.string "Backlog"
            Board -> Encode.string "Board")
        ]