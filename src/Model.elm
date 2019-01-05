module Model exposing (DragSource(..), DropTarget(..), Model, Msg(..), UIState(..), init, update)

import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import Json.Encode as Encode
import List.Extra
import US as US
import Store as Store


type UIState
    = Board
    | Backlog


type TaskDropTarget
    = TaskDropOnUS US.US
    | TaskDropOnBoard US.US US.BoardStage


type DragSource
    = DragUS US.US
    | DragTask US.Task


type DropTarget
    = UserStoryDrop US.US
    | BoardDrop US.US US.BoardStage
    | BacklogDrop
    | NewLaneDrop


type alias Model =
    { doneTask1ZIndex : Int
    , doneTask2ZIndex : Int
    , doneTask3ZIndex : Int
    , dragDropUserStory : DragDrop.Model DragSource DropTarget
    , uiState : UIState
    , userStories : List US.US
    , usModel : US.Model
    , input : String
    }


type Msg
    = DragDropUserStory (DragDrop.Msg DragSource DropTarget)
    | UpdateUIState UIState
    | NewUserStory
    | SaveUSTitleInput US.US String
    | SaveUSDescriptionInput US.US String
    | SaveTaskDescriptionInput US.Task String
    | NewTask US.US
    | TaskActive US.Task Bool
    | NewTaskActive US.US Bool


init : ( Model, Cmd Msg )
init =
    ( { doneTask1ZIndex = 1
      , doneTask2ZIndex = 2
      , doneTask3ZIndex = 3
      , dragDropUserStory = DragDrop.init
      , uiState = Board
      , userStories = []
      , usModel = US.init
      , input = ""
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragDropUserStory msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDropUserStory
            in
            ( { model
                | dragDropUserStory = model_
                , userStories =
                    case result of
                        Nothing ->
                            model.userStories

                        Just ( src, target, _ ) ->
                            case target of
                                UserStoryDrop targetUS ->
                                    case src of
                                        DragUS srcUS ->
                                            moveUserStory model.userStories srcUS targetUS

                                        DragTask task ->
                                            moveTask model.userStories task targetUS

                                BoardDrop targetUS newStage ->
                                    case src of
                                        DragUS srcUS ->
                                            let
                                                updateBoard =
                                                    moveUserStory model.userStories srcUS targetUS
                                                        |> List.Extra.updateIf
                                                            (\us -> us.id == srcUS.id)
                                                            (\us -> { us | tasks = List.map (\task -> { task | stage = newStage }) us.tasks })
                                            in
                                            case newStage of
                                                US.ToDo ->
                                                    updateBoard

                                                US.InProgress ->
                                                    model.userStories

                                                US.Done ->
                                                    updateBoard

                                        DragTask draggedTask ->
                                            model.userStories
                                                |> List.Extra.updateIf (\us -> us.id == targetUS.id)
                                                    (\us ->
                                                        { us
                                                            | tasks =
                                                                List.Extra.updateIf (\task -> task.id == draggedTask.id)
                                                                    (\task -> { task | stage = newStage })
                                                                    us.tasks
                                                        }
                                                    )

                                NewLaneDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            List.filter (\us -> us.id /= srcUS.id) model.userStories
                                                ++ [ { srcUS | stage = US.Board } ]

                                        DragTask task ->
                                            model.userStories

                                BacklogDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            List.filter (\us -> us.id /= srcUS.id) model.userStories
                                                ++ [ { srcUS | stage = US.Backlog } ]

                                        DragTask task ->
                                            model.userStories
              }
            , Cmd.none
            )

        UpdateUIState state ->
            ( { model | uiState = state }, Cmd.none )

        NewUserStory ->
            let
                ( newUSModel, newUS ) =
                    US.newUS model.usModel
            in
            ( { model
                | userStories =
                    model.userStories
                        ++ [ newUS
                           ]
                , usModel = newUSModel
              }
            , Cmd.none
            )

        SaveUSTitleInput story str ->
            let
                storyModifier = \us -> { us | name = str }
            in
                ( { model
                    | userStories = updateUserStory model.userStories story.id storyModifier
                }, Store.putUS <| storyModifier story )

        SaveUSDescriptionInput story str ->
            let
                storyModifier = \us -> { us | description = str }
            in
                ( { model
                    | userStories = updateUserStory model.userStories story.id storyModifier
                }, Store.putUS <| storyModifier story )

        SaveTaskDescriptionInput task str ->
            ( { model
                | userStories =
                    model.userStories
                        |> List.map
                            (\story ->
                                { story
                                    | tasks =
                                        List.map
                                            (\t ->
                                                { t
                                                    | description =
                                                        if task.id == t.id then
                                                            str

                                                        else
                                                            t.description
                                                }
                                            )
                                            story.tasks
                                }
                            )
              }
            , Cmd.none
            )

        NewTask story ->
            let
                ( newUSModel, newTask ) =
                    US.newTask model.usModel
            in
            ( { model
                | userStories =
                    List.Extra.updateIf
                        (\us -> us.id == story.id)
                        (\us ->
                            { us
                                | tasks = us.tasks ++ [ newTask ]
                                , newTaskState =
                                    if List.length us.tasks == 15 then
                                        US.NotAllowed

                                    else
                                        us.newTaskState
                            }
                        )
                        model.userStories
                , usModel = newUSModel
              }
            , Cmd.none
            )

        TaskActive task active ->
            ( { model
                | userStories =
                    model.userStories
                        |> List.map
                            (\us ->
                                { us
                                    | tasks =
                                        us.tasks
                                            |> List.map
                                                (\t ->
                                                    { t
                                                        | active =
                                                            if t.id == task.id then
                                                                active

                                                            else
                                                                t.active
                                                    }
                                                )
                                }
                            )
              }
            , Cmd.none
            )

        NewTaskActive story active ->
            ( { model
                | userStories =
                    model.userStories
                        |> List.map
                            (\us ->
                                { us
                                    | newTaskState =
                                        case us.newTaskState of
                                            US.Active _ ->
                                                if us.id == story.id then
                                                    US.Active active

                                                else
                                                    us.newTaskState

                                            US.NotAllowed ->
                                                us.newTaskState
                                }
                            )
              }
            , Cmd.none
            )


moveUserStory : List US.US -> US.US -> US.US -> List US.US
moveUserStory list src target =
    let
        targetMaybe =
            List.Extra.find (\x -> x.id == target.id) list
    in
    if src.id == target.id then
        list

    else
        list
            |> List.filter (\us -> us.id /= src.id)
            |> List.map
                (\us ->
                    if us.id == target.id then
                        list
                            |> List.filter (\x -> x.id == src.id || x.id == target.id)
                            |> List.reverse

                    else
                        [ us ]
                )
            |> List.concat
            |> List.Extra.updateIf (\us -> us.id == src.id)
                (\srcUs ->
                    case targetMaybe of
                        Nothing ->
                            srcUs

                        Just targetUs ->
                            { srcUs | stage = targetUs.stage }
                )


moveTask : List US.US -> US.Task -> US.US -> List US.US
moveTask list task target =
    list
        |> List.map (\us -> { us | tasks = List.filter (\t -> t.id /= task.id) us.tasks })
        |> List.map
            (\us ->
                if us.id == target.id then
                    { us | tasks = us.tasks ++ [ task ] }

                else
                    us
            )


updateUserStory : List US.US -> Int -> (US.US -> US.US) -> List US.US
updateUserStory userStories id modifier =
                    List.Extra.updateIf
                        (\us -> us.id == id)
                        modifier
                        userStories