port module Todo exposing (Model, Msg(..), init, main, update, view)

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


emptyState : Model
emptyState =
    { entries = []
    , currentInput = ""
    , currentUid = 0
    }


init : Maybe Model -> ( Model, Cmd msg )
init initialModel =
    ( Maybe.withDefault emptyState initialModel, Cmd.none )


port setStorage : Model -> Cmd msg


type alias Entry =
    { uid : Int
    , description : String
    , isCompleted : Bool
    }


type alias Model =
    { entries : List Entry
    , currentInput : String
    , currentUid : Int
    }


updateWithStorage : Msg -> Model -> ( Model, Cmd msg )
updateWithStorage msg model =
    let
        ( newModel, command ) =
            update msg model
    in
    ( newModel, Cmd.batch [ command, setStorage newModel ] )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Add ->
            let
                newEntry =
                    { description = model.currentInput, uid = model.currentUid + 1, isCompleted = False }

                newEntries =
                    List.append model.entries [ newEntry ]
            in
            ( { model | entries = newEntries, currentInput = "", currentUid = model.currentUid + 1 }, Cmd.none )

        ChangeInput newInputValue ->
            ( { model | currentInput = newInputValue }, Cmd.none )

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
                filterEntry entry =
                    entry.uid /= uid
            in
            ( { model | entries = List.filter filterEntry model.entries }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section [ class "todoapp" ]
            [ showHeader model
            , showEntries model
            ]
        ]


showHeader : Model -> Html Msg
showHeader model =
    header [ class "header" ]
        [ h1 [] [ text "todos" ]
        , input
            [ class "new-todo"
            , placeholder "What needs to be done?"
            , value model.currentInput
            , onInput ChangeInput
            , onEnter Add
            ]
            []
        ]


showEntries : Model -> Html Msg
showEntries model =
    section [ class "main" ]
        [ ul [ class "todo-list" ] (List.map showEntry model.entries)
        ]


showEntry : Entry -> Html Msg
showEntry entry =
    li [ classList [ ( "completed", entry.isCompleted ) ] ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked entry.isCompleted, onClick (Check entry.uid) ] []
            , label [] [ text entry.description ]
            , button [ class "destroy", onClick (Delete entry.uid) ] []
            ]
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
