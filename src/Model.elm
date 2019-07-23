port module Model exposing (DragSource(..), DropTarget(..), Model, Msg(..), UIState(..), init, update, receivePort)

import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import US as US
import USTask as Task
import Board as Board
import Json.Encode as Encode
import Json.Decode as Decode
import EntityUUID

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
    | ReceiveDocument Encode.Value


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( usInitialSeed, taskInitialSeed ) =
    ( { 
       dragDropUserStory = DragDrop.init
      , uiState = Board
      , usModel = US.init usInitialSeed
      , taskModel = Task.init taskInitialSeed
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
                (updatedUS, newUsModel) = case result of
                        Nothing ->
                             ([], model.usModel)

                        Just ( src, target, _ ) ->
                            case Debug.log "target" target of
                                UserStoryDrop targetUS ->
                                    case src of
                                        DragUS srcUS ->
                                            US.move model.usModel srcUS.id targetUS.id

                                        DragTask task ->
                                            ([], model.usModel)

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
                                                    ([], model.usModel)

                                                Board.Done ->
                                                    updateBoard

                                        DragTask draggedTask ->
                                             ([], model.usModel)

                                NewLaneDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            US.updateUSStage model.usModel srcUS.id US.Board

                                        DragTask task ->
                                             ([], model.usModel)

                                BacklogDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            US.updateUSStage model.usModel srcUS.id US.Backlog

                                        DragTask task ->
                                             ([], model.usModel)
                (updatedTasks, newTaskModel) = case result of
                        Nothing ->
                            ([], model.taskModel)

                        Just ( src, target, _ ) ->
                            case target of
                                UserStoryDrop targetUS ->
                                    case src of
                                        DragUS srcUS ->
                                            ([], model.taskModel)

                                        DragTask task ->
                                            Task.move model.taskModel task.id targetUS.id

                                BoardDrop targetUS newStage ->
                                    case src of
                                        DragUS srcUS ->
                                            case newStage of
                                                Board.ToDo ->
                                                   Task.updateBoardStage model.taskModel srcUS.id newStage 

                                                Board.InProgress ->
                                                    ([], model.taskModel)

                                                Board.Done ->
                                                   Task.updateBoardStage model.taskModel srcUS.id newStage 

                                        DragTask draggedTask ->
                                            ([], Task.updateTaskBoardStage model.taskModel draggedTask.id newStage)

                                NewLaneDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            ([], model.taskModel)

                                        DragTask task ->
                                            ([], model.taskModel)

                                BacklogDrop ->
                                    case src of
                                        DragUS srcUS ->
                                            ([], model.taskModel)

                                        DragTask task ->
                                            ([], model.taskModel)
            in
            ( { model
                | dragDropUserStory = model_
                , usModel = newUsModel       
                , taskModel = newTaskModel
            }     
            , Cmd.batch <| List.map (putUS model.usModel) updatedUS ++ List.map (putTask model.taskModel) updatedTasks
            )

        UpdateUIState state ->
            ( { model | uiState = state }, Cmd.none )

        NewUserStory ->
            let
                (newUS, newUSModel) = US.new model.usModel
                newModel = { model | usModel = newUSModel }
            in
                ( newModel, putUS model.usModel newUS )

        SaveUSTitleInput story str ->
            let
                (updatedUS, newUSModel) = US.updateName model.usModel story.id str
                newModel = { model | usModel = newUSModel }
            in
                (newModel, Cmd.batch <| List.map (putUS model.usModel) updatedUS)

        SaveUSDescriptionInput story str ->
            let
                (updatedUS, newUSModel) = US.updateDescription model.usModel story.id str
                newModel = { model | usModel = newUSModel }
            in
                (newModel, Cmd.batch <| List.map (putUS model.usModel) updatedUS)

        SaveTaskDescriptionInput task str ->
            let
                (updatedTasks, newTaskModel) = Task.updateDescription model.taskModel task.id str
                newModel = { model | taskModel = newTaskModel }
            in
                (newModel, Cmd.batch <| List.map (putTask model.taskModel) updatedTasks)

        NewTask story ->
            let
                (newTask, newTaskModel) = Task.new model.taskModel story.id
                newModel = { model | taskModel = newTaskModel }
            in
                ( newModel, putTask model.taskModel newTask )


        TaskActive task active ->
            let
                (newTask, newTaskModel) = Task.updateActive model.taskModel task.id active
                newModel = { model | taskModel = newTaskModel }
            in
                ( newModel, Cmd.none )


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
        
        ReceiveDocument jsonValue -> 
            let
                objId = Result.withDefault EntityUUID.nil <| Decode.decodeValue (Decode.field "_id" EntityUUID.decoder) jsonValue
                objType = Result.withDefault "Unknown" <| Decode.decodeValue (Decode.field "type" Decode.string) jsonValue
                objValueResult = Decode.decodeValue (Decode.field "obj" Decode.value) jsonValue
            in
                case Debug.log "ObjType" objType of
                    "US" ->
                        case objValueResult of
                            Err _ ->
                                (model , Cmd.none)
                            Ok objValue ->
                                ({model | usModel = US.fromJson model.usModel objId objValue} , Cmd.none)
                    "Task" ->
                        case objValueResult of
                            Err _ ->
                                (model , Cmd.none)
                            Ok objValue ->
                                ({model | taskModel = Task.fromJson model.taskModel objId objValue} , Cmd.none)
                    _ ->
                        (model, Cmd.none)

port sendPort : Encode.Value -> Cmd msg

port receivePort: (Encode.Value -> msg) -> Sub msg


putUS : US.Model -> US.T -> Cmd Msg
putUS usModel us = sendPort <| Encode.object [("type", Encode.string "US"), ("obj", US.toDeltaJson usModel us)]

putTask : Task.Model -> Task.T -> Cmd Msg
putTask taskModel task = sendPort <| Encode.object [("type", Encode.string "Task"), ("obj", Task.toDeltaJson taskModel task)]