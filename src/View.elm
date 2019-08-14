module View exposing (view)

import Board as Board
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import Model exposing (Model, Msg, UIState)
import US as US
import USTask as Task


mainClass : Model -> String
mainClass model =
    case model.uiState of
        Model.Board ->
            "board"

        Model.Backlog ->
            "backlog"


toggleBacklog : Model -> Msg
toggleBacklog model =
    case model.uiState of
        Model.Board ->
            Model.UpdateUIState Model.Backlog

        Model.Backlog ->
            Model.UpdateUIState Model.Board


view : Model -> Html Msg
view model =
    let
        virtualLaneState =
            if model.uiState == Model.Backlog then
                ShowLaneDrop

            else if US.count model.usModel == 0 then
                ShowBacklogHint

            else
                HideVirtualLane
    in
    div [ id "main", class <| mainClass model ]
        [ div ([ id "backlog" ] ++ DragDrop.droppable Model.DragDropUserStory Model.BacklogDrop)
            ((US.filter model.usModel US.Backlog
                |> List.map (\story -> userStoryCard story (Task.tasks model.taskModel story.id))
             )
                ++ [ newUserStoryCard ]
            )
        , div [ class "bar", onClick <| toggleBacklog model ] [ text "BACKLOG", i [ class "button material-icons" ] [ text "chevron_left" ] ]
        , div [ id "board" ]
            ((US.filter model.usModel US.Board
                |> List.map (\us -> laneComponent us (Task.tasks model.taskModel us.id))
             )
                ++ [ virtualLaneComponent virtualLaneState ]
            )
        , div [ class "bar" ] []
        ]


laneComponent : US.T -> List Task.T -> Html Model.Msg
laneComponent us tasks =
    let
        usStage =
            Task.boardStage tasks

        filteredTasks =
            \stage -> tasks |> List.filter (\task -> task.stage == stage)
    in
    div [ class "lane" ]
        [ div ([ class "todo stage" ] ++ DragDrop.droppable Model.DragDropUserStory (Model.BoardDrop us Board.ToDo))
            (case usStage of
                Board.ToDo ->
                    [ userStoryCard us (filteredTasks Board.ToDo) ]

                _ ->
                    [ tasksContainer (filteredTasks Board.ToDo) ]
            )
        , div ([ class "inprogress stage" ] ++ DragDrop.droppable Model.DragDropUserStory (Model.BoardDrop us Board.InProgress))
            (case usStage of
                Board.InProgress ->
                    [ userStoryCard us (filteredTasks Board.InProgress) ]

                _ ->
                    [ tasksContainer (filteredTasks Board.InProgress) ]
            )
        , div ([ class "done stage" ] ++ DragDrop.droppable Model.DragDropUserStory (Model.BoardDrop us Board.Done))
            (case usStage of
                Board.Done ->
                    [ userStoryCard us (filteredTasks Board.Done) ]

                _ ->
                    [ tasksContainer (filteredTasks Board.Done) ]
            )
        ]


type VirtualLaneState
    = ShowBacklogHint
    | ShowLaneDrop
    | HideVirtualLane


virtualLaneComponent : VirtualLaneState -> Html Model.Msg
virtualLaneComponent state =
    div [ classList [ ( "lane", True ), ( "hide", state == HideVirtualLane ) ] ]
        [ div [ class "todo stage" ]
            [ newLaneUserStoryCard (state /= ShowLaneDrop)
            , backlogHintStoryCard (state /= ShowBacklogHint)
            ]
        , div [ class "inprogress stage" ] []
        , div [ class "done stage" ] []
        ]


userStoryCard : US.T -> List Task.T -> Html Model.Msg
userStoryCard story storytasks =
    let
        hover =
            (storytasks
                |> List.map
                    (\task ->
                        if task.active then
                            1

                        else
                            0
                    )
                |> List.sum
                |> (+)
                    (case story.newTaskState of
                        US.Active active ->
                            if active then
                                1

                            else
                                0

                        US.NotAllowed ->
                            0
                    )
            )
                > 0

        allowNewTask =
            if Task.boardStage storytasks /= Board.ToDo then
                False

            else
                case story.newTaskState of
                    US.Active active ->
                        True

                    US.NotAllowed ->
                        False
    in
    div
        ([ class "userstory" ]
            ++ DragDrop.draggable Model.DragDropUserStory (Model.DragUS story)
            ++ DragDrop.droppable Model.DragDropUserStory (Model.UserStoryDrop story)
        )
        [ div [ class "header" ]
            [ h1 [ class "nr" ] [ text <| "#" ]
            , input [ onInput (Model.SaveUSTitleInput story), value story.name ] []
            ]
        , textarea [ onInput (Model.SaveUSDescriptionInput story) ] [ text story.description ]
        , div [ class "tasks" ]
            (List.map (\task -> taskCard task hover) storytasks
                ++ (if allowNewTask then
                        [ newTask story hover ]

                    else
                        []
                   )
            )
        ]


taskCard : Task.T -> Bool -> Html Model.Msg
taskCard task hover =
    div
        ([ classList
            [ ( "task", True )
            , ( "active", task.active )
            , ( "hover", hover )
            ]
         , onMouseOver (Model.TaskActive task True)
         , onMouseOut (Model.TaskActive task False)
         ]
            ++ DragDrop.draggable Model.DragDropUserStory (Model.DragTask task)
        )
        [ textarea
            [ onInput (Model.SaveTaskDescriptionInput task) ]
            []
        ]


tasksContainer : List Task.T -> Html Model.Msg
tasksContainer tasks =
    div [ class "tasks" ]
        (List.map (\task -> taskCard task False) tasks)


newUserStoryCard : Html Model.Msg
newUserStoryCard =
    virtualUserStoryCard False "control_point" "Click here to create a new user story in the back log!" [ onClick Model.NewUserStory ]


newLaneUserStoryCard : Bool -> Html Model.Msg
newLaneUserStoryCard hide =
    virtualUserStoryCard hide "get_app" "Drag a user story from the back log and drop it here to make it a part of the current sprint!" (DragDrop.droppable Model.DragDropUserStory Model.NewLaneDrop)


backlogHintStoryCard : Bool -> Html Model.Msg
backlogHintStoryCard hide =
    virtualUserStoryCard hide "arrow_left" "Go to the backlog to create your first user story!" (DragDrop.droppable Model.DragDropUserStory Model.NewLaneDrop)


virtualUserStoryCard : Bool -> String -> String -> List (Attribute Msg) -> Html Model.Msg
virtualUserStoryCard hide button description attrs =
    div
        ([ classList
            [ ( "userstory", True )
            , ( "virtual", True )
            , ( "hide", hide )
            ]
         ]
            ++ attrs
        )
        [ i [ class "button material-icons" ] [ text button ]
        , p [] [ text description ]
        ]


newTask : US.T -> Bool -> Html Model.Msg
newTask story hover =
    virtualTask story
        hover
        "control_point"
        "Click to create a new task!"
        [ onMouseOver (Model.NewTaskActive story True)
        , onMouseOut (Model.NewTaskActive story False)
        ]


virtualTask : US.T -> Bool -> String -> String -> List (Attribute Msg) -> Html Model.Msg
virtualTask story hover button description attrs =
    let
        active =
            case story.newTaskState of
                US.Active x ->
                    x

                US.NotAllowed ->
                    False
    in
    div
        ([ classList
            [ ( "task", True )
            , ( "virtual", True )
            , ( "hover", hover )
            , ( "active", active )
            ]
         , onClick (Model.NewTask story)
         ]
            ++ attrs
        )
        [ i [ class "button material-icons" ] [ text button ], p [] [ text description ] ]
