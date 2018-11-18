module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html5.DragDrop as DragDrop
import Json.Decode as Json
import List.Extra
import Model exposing (Model, Msg, UIState)
import US as US
import Html.Keyed as Keyed


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
            else
                if List.length model.userStories == 0 then
                    ShowBacklogHint
                else
                    HideVirtualLane
    in
    
    div [ id "main", class <| mainClass model ]
        [ div ([ id "backlog" ] ++ DragDrop.droppable Model.DragDropUserStory Model.BacklogDrop)
            ((model.userStories
                |> List.filter (\us -> us.stage == US.Backlog)
                |> List.map (\story -> userStoryCard story)
             )
                ++ [ newUserStoryCard ]
            )
        , div [ class "bar", onClick <| toggleBacklog model ] [ text "BACKLOG", i [ class "button material-icons" ] [ text "chevron_left" ] ]
        , div [ id "board" ]
            ((model.userStories
                |> List.filter (\us -> us.stage /= US.Backlog)
                |> List.map (\us -> laneComponent us)
             )
                ++ [ virtualLaneComponent virtualLaneState ]
            )
        , div [ class "bar" ] []
        ]


laneComponent : US.US -> Html Model.Msg
laneComponent us =
    let
        usStage =
            US.usBoardStage us

        filteredUS =
            US.filterUsTasks us usStage
    in
    div [ class "lane" ]
        [ div ([ class "todo stage" ] ++ DragDrop.droppable Model.DragDropUserStory (Model.BoardDrop us US.ToDo))
            (case usStage of
                US.ToDo ->
                    [ userStoryCard (US.filterUsTasks us US.ToDo) ]

                _ ->
                    [ tasks (US.filterUsTasks us US.ToDo) ]
            )
        , div ([ class "inprogress stage" ] ++ DragDrop.droppable Model.DragDropUserStory (Model.BoardDrop us US.InProgress))
            (case usStage of
                US.InProgress ->
                    [ userStoryCard (US.filterUsTasks us US.InProgress) ]

                _ ->
                    [ tasks (US.filterUsTasks us US.InProgress) ]
            )
        , div ([ class "done stage" ] ++ DragDrop.droppable Model.DragDropUserStory (Model.BoardDrop us US.Done))
            (case usStage of
                US.Done ->
                    [ userStoryCard (US.filterUsTasks us US.Done) ]

                _ ->
                    [ tasks (US.filterUsTasks us US.Done) ]
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
            [ newLaneUserStoryCard (state /= ShowLaneDrop), backlogHintStoryCard (state /= ShowBacklogHint)
            ]
        , div [ class "inprogress stage" ] []
        , div [ class "done stage" ] []
        ]


userStoryCard : US.US -> Html Model.Msg
userStoryCard story =
    let
        hover =
            (story.tasks
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
            if US.usBoardStage story /= US.ToDo then
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
            [ h1 [ class "nr" ] [ text <| "#" ++ String.fromInt story.id ]
            , Keyed.node "h1" [ contenteditable True, on "blur" (Json.map (Model.SaveUSTitleInput story) targetTextContent) ] [ ("usname", text story.name) ]
            ]
        , Keyed.node "p" [ contenteditable True, on "blur" (Json.map (Model.SaveUSDescriptionInput story) targetTextContent) ] [ ("usdesc", text story.description) ]
        , div [ class "tasks" ]
            (List.map (\task -> taskCard task hover) story.tasks
                ++ (if allowNewTask then
                        [ newTask story hover ]

                    else
                        []
                   )
            )
        ]


taskCard : US.Task -> Bool -> Html Model.Msg
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
        [ Keyed.node "p"
            [ contenteditable True
            , on "blur" (Json.map (Model.SaveTaskDescriptionInput task) targetTextContent)
            ]
            [ ("tdesc", text task.description) ]
        ]


tasks : US.US -> Html Model.Msg
tasks us =
    div [ class "tasks" ]
        (List.map (\task -> taskCard task False) us.tasks)


targetTextContent : Json.Decoder String
targetTextContent =
    Json.at [ "target", "textContent" ] Json.string


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


newTask : US.US -> Bool -> Html Model.Msg
newTask story hover =
    virtualTask story
        hover
        "control_point"
        "Click to create a new task!"
        [ onMouseOver (Model.NewTaskActive story True)
        , onMouseOut (Model.NewTaskActive story False)
        ]


virtualTask : US.US -> Bool -> String -> String -> List (Attribute Msg) -> Html Model.Msg
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
