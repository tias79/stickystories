module Model exposing (DragSource(..), DropTarget(..), Model, Msg(..), UIState(..), init, update)

import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import Json.Encode as Encode
import List.Extra
import US as US
import USTask as Task
import Store as Store
import Board as Board
import Dict exposing (Dict)
import Set exposing (Set)


type UIState
    = Board
    | Backlog


type TaskDropTarget
    = TaskDropOnUS US.T
    | TaskDropOnBoard US.T Board.Stage


type DragSource
    = DragUS US.T
    | DragTask Task.T


type DropTarget
    = UserStoryDrop US.T
    | BoardDrop US.T Board.Stage
    | BacklogDrop
    | NewLaneDrop


type alias Model =
    { dragDropUserStory : DragDrop.Model DragSource DropTarget
    , uiState : UIState
    , usModel : US.Model
    , taskModel : Task.Model
    , input : String
    }


type Msg
    = DragDropUserStory (DragDrop.Msg DragSource DropTarget)
    | UpdateUIState UIState
    | NewUserStory
    | SaveUSTitleInput US.T String
    | SaveUSDescriptionInput US.T String
    | SaveTaskDescriptionInput Task.T String
    | NewTask US.T
    | TaskActive Task.T Bool
    | NewTaskActive US.T Bool


init : ( Model, Cmd Msg )
init =
    ( { 
       dragDropUserStory = DragDrop.init
      , uiState = Board
      , usModel = US.init
      , taskModel = Task.init
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
                , usModel =
                    case result of
                        Nothing ->
                            model.usModel

                        Just ( src, target, _ ) ->
                            case target of
                                UserStoryDrop targetUS ->
                                    case src of
                                        DragUS srcUS ->
                                            US.move model.usModel srcUS.id targetUS.id

                                        DragTask task ->
                                            model.usModel

                                BoardDrop targetUS newStage ->
                                    case src of
                                        DragUS srcUS ->
                                            let
                                                updateBoard =
                                                    US.move model.usModel srcUS.id targetUS.id
                                            in
                                            case newStage of
                                                Board.ToDo ->
                                                   updateBoard

                                                Board.InProgress ->
                                                    model.usModel

                                                Board.Done ->
                                                    updateBoard

                                        DragTask draggedTask ->
                                            model.usModel

                                NewLaneDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            US.updateUSStage model.usModel srcUS.id US.Board

                                        DragTask task ->
                                            model.usModel

                                BacklogDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            US.updateUSStage model.usModel srcUS.id US.Backlog

                                        DragTask task ->
                                            model.usModel
                , taskModel =
                   case result of
                        Nothing ->
                            model.taskModel

                        Just ( src, target, _ ) ->
                            case target of
                                UserStoryDrop targetUS ->
                                    case src of
                                        DragUS srcUS ->
                                            model.taskModel

                                        DragTask task ->
                                            Task.move model.taskModel task.id targetUS.id

                                BoardDrop targetUS newStage ->
                                    case src of
                                        DragUS srcUS ->
                                            case newStage of
                                                Board.ToDo ->
                                                   Task.updateBoardStage model.taskModel srcUS.id newStage 

                                                Board.InProgress ->
                                                    model.taskModel

                                                Board.Done ->
                                                   Task.updateBoardStage model.taskModel srcUS.id newStage 

                                        DragTask draggedTask ->
                                            Task.updateBoardStage model.taskModel targetUS.id newStage

                                NewLaneDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            model.taskModel

                                        DragTask task ->
                                            model.taskModel

                                BacklogDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            model.taskModel

                                        DragTask task ->
                                            model.taskModel
              }
            , Cmd.none
            )

        UpdateUIState state ->
            ( { model | uiState = state }, Cmd.none )

        NewUserStory ->
            ( { model
                | usModel = US.new model.usModel
              }
            , Cmd.none
            )

        SaveUSTitleInput story str ->
            ( { model
                | usModel = US.updateName model.usModel story.id str
            }, Store.putUS story )

        SaveUSDescriptionInput story str ->
            ( { model
                | usModel = US.updateDescription model.usModel story.id str
            }, Store.putUS story )

        SaveTaskDescriptionInput task str ->
            ( { model
                | taskModel = Task.updateDescription model.taskModel task.id str
              }
            , Store.putTask task
            )

        NewTask story ->
            ( { model
                | taskModel = Task.new model.taskModel story.id
              }
            , Cmd.none
            )

        TaskActive task active ->
            ( { model
                | taskModel = Task.updateActive model.taskModel task.id active
              }
            , Cmd.none
            )

        NewTaskActive story active ->
            ( { model
                | 
                usModel = US.updateNewTaskState model.usModel story.id (
                                        case story.newTaskState of
                                            US.Active _ ->
                                                if story.id == story.id then
                                                    US.Active active

                                                else
                                                    story.newTaskState

                                            US.NotAllowed ->
                                                story.newTaskState)
              }
            , Cmd.none
            )