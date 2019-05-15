port module Todo exposing (Entry, Model, Msg(..), allCompleted, emptyModel, init, main, onEnter, setStorage, showEntries, showEntry, showFilter, showFooter, showHeader, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Json
import String


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Add
    | ChangeInput String
    | Check Int
    | Delete Int
    | CheckAll
    | ChangeFilter String
    | ClearCompleted


emptyModel =
    { entries = []
    , currentInput = "Hello world"
    , currentUid = 0
    , filter = "All"
    }


init : Maybe Model -> ( Model, Cmd msg )
init model =
    ( Maybe.withDefault emptyModel model, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, command ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, command ]
    )


type alias Entry =
    { description : String
    , uid : Int
    , isCompleted : Bool
    }


port setStorage : Model -> Cmd msg


type alias Model =
    { entries : List Entry
    , currentInput : String
    , currentUid : Int
    , filter : String
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Add ->
            let
                newEntriesList =
                    List.append model.entries
                        [ { description = model.currentInput
                          , uid = model.currentUid
                          , isCompleted = False
                          }
                        ]

                newUid =
                    model.currentUid + 1
            in
            ( { model | entries = newEntriesList, currentInput = "", currentUid = newUid }, Cmd.none )

        ChangeInput newInput ->
            ( { model | currentInput = newInput }, Cmd.none )

        Check uid ->
            let
                checkEntry entry =
                    if entry.uid == uid then
                        { entry | isCompleted = not entry.isCompleted }

                    else
                        entry
            in
            ( { model | entries = List.map checkEntry model.entries }, Cmd.none )

        Delete uid ->
            let
                removeEntry entry =
                    not (entry.uid == uid)
            in
            ( { model | entries = List.filter removeEntry model.entries }, Cmd.none )

        CheckAll ->
            let
                updateEntry entry =
                    { entry | isCompleted = not (allCompleted model) }
            in
            ( { model | entries = List.map updateEntry model.entries }, Cmd.none )

        ChangeFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        ClearCompleted ->
            ( { model | entries = List.filter (\e -> not e.isCompleted) model.entries }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section [ class "todoapp" ]
            [ showHeader model
            , showEntries
                model
            , showFooter model
            ]
        ]


showHeader : Model -> Html Msg
showHeader model =
    header [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input [ class "new-todo", value model.currentInput, onInput ChangeInput, onEnter Add ] []
        ]


showEntries : Model -> Html Msg
showEntries model =
    let
        entries =
            case model.filter of
                "Completed" ->
                    List.filter (\e -> e.isCompleted) model.entries

                "Active" ->
                    List.filter (\e -> not e.isCompleted) model.entries

                _ ->
                    model.entries
    in
    section [ class "main" ]
        [ input [ class "toggle-all", type_ "checkbox", onClick CheckAll, checked (not (allCompleted model)) ] []
        , ul [ class "todo-list" ]
            (List.map
                showEntry
                entries
            )
        ]


showEntry : Entry -> Html Msg
showEntry entry =
    li [ classList [ ( "completed", entry.isCompleted ) ] ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked entry.isCompleted, onClick (Check entry.uid) ]
                []
            , label
                []
                [ text entry.description ]
            , button [ class "destroy", onClick (Delete entry.uid) ]
                []
            ]
        ]


showFooter : Model -> Html Msg
showFooter model =
    let
        itemsLeftInTodo =
            List.filter (\e -> not e.isCompleted) model.entries
                |> List.length
                |> String.fromInt

        itemsDone =
            List.filter .isCompleted model.entries
                |> List.length
                |> String.fromInt
    in
    footer [ class "footer" ]
        [ span [ class "todo-count" ]
            [ text itemsLeftInTodo
            , text " items left"
            ]
        , ul [ class "filters" ]
            [ showFilter model "All"
            , showFilter model "Active"
            , showFilter model "Completed"
            ]
        , button [ class "clear-completed", onClick ClearCompleted ]
            [ text ("Clear completed (" ++ itemsDone ++ ")")
            ]
        ]


showFilter : Model -> String -> Html Msg
showFilter model filterType =
    li []
        [ a
            [ classList [ ( "selected", model.filter == filterType ) ], onClick (ChangeFilter filterType) ]
            [ text filterType ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter key =
            if key == 13 then
                Json.succeed msg

            else
                Json.fail "Not Enter"
    in
    on "keydown" (Json.andThen isEnter keyCode)


allCompleted : Model -> Bool
allCompleted model =
    List.all .isCompleted model.entries && model.entries /= []
