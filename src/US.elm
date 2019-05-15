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


new : Model -> Model
new model =
    ( { model
        | nextUserStoryId = model.nextUserStoryId + 1
        , userStories = model.userStories ++
            [{ id = model.nextUserStoryId
                , name = ""
                , description = ""
                , stage = Backlog
                , newTaskState = Active False
            }]
    })


update : Model -> Id -> (T -> T) -> Model
update model usId modifier =
    ( { model | userStories = List.Extra.updateIf
                        (\us -> us.id == usId)
                        modifier
                        model.userStories
    } )


updateName : Model -> Id -> String -> Model
updateName model usId name = update model usId (\us -> { us | name = name })


updateDescription : Model -> Id -> String -> Model
updateDescription model usId description = update model usId (\us -> { us | description = description })


updateUSStage : Model -> Id -> USStage -> Model
updateUSStage model usId stage = update model usId (\us -> { us | stage = stage })


updateNewTaskState : Model -> Id -> NewTaskState -> Model
updateNewTaskState model usId state = update model usId (\us -> { us | newTaskState = state })


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


encode : Maybe T -> T -> Maybe T -> Encode.Value
encode prevUs curUs nextUs =
    Encode.object [
        ("id", Encode.int curUs.id),
        ("prevId", case prevUs of
            Nothing -> Encode.int -1
            Just us -> Encode.int us.id),
        ("nextId", case nextUs of
            Nothing -> Encode.int -1
            Just us -> Encode.int us.id),
        ("name", Encode.string curUs.name),
        ("description", Encode.string curUs.description),
        ("stage", case curUs.stage of
            Backlog -> Encode.string "Backlog"
            Board -> Encode.string "Board")
        ]


toJson : Model -> Encode.Value
toJson model = toJsonRec Nothing model.userStories |> Encode.list (\x -> x)


toJsonRec : Maybe T -> List T -> List Encode.Value
toJsonRec prevUs userStories =
    let
        curUs = List.head userStories
        tail = Maybe.withDefault [] (List.tail userStories)
        nextUs = tail |> List.head
    in    
        case curUs of
            Nothing -> []
            Just us -> [ encode prevUs us nextUs ] ++ toJsonRec curUs tail