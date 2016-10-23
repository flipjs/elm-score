module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : State -> Sub Msg
subscriptions state =
    Sub.none



-- MODEL


init : ( State, Cmd Msg )
init =
    ( initialState, Cmd.none )


initialState : State
initialState =
    { players = []
    , plays = []
    , input = ""
    , playerId = Nothing
    , totalScore = 0
    }


type alias State =
    { players : List Player
    , plays : List Play
    , input : Name
    , playerId : Maybe Id
    , totalScore : Score
    }


type alias Name =
    String


type alias Id =
    Int


type alias Score =
    Int


type alias Player =
    { id : Id
    , name : Name
    , score : Score
    }


type alias Play =
    { id : Id
    , playerName : Name
    , playerId : Id
    , score : Score
    }



-- UPDATE


type Msg
    = InputName Name
    | EditPlayer Player
    | AddScore Player Score
    | DeletePlay Play
    | Cancel
    | Save


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        InputName name ->
            actionInputName state name

        EditPlayer player ->
            actionEditPlayer state player

        AddScore player score ->
            actionAddScore state player score

        DeletePlay play ->
            actionDeletePlay state play

        Cancel ->
            actionCancel state

        Save ->
            actionSave state


actionInputName : State -> Name -> ( State, Cmd Msg )
actionInputName state name =
    ( { state
        | input = name
      }
    , Cmd.none
    )


actionEditPlayer : State -> Player -> ( State, Cmd Msg )
actionEditPlayer state player =
    ( { state
        | input = player.name
        , playerId = Just player.id
      }
    , Cmd.none
    )


actionAddScore : State -> Player -> Score -> ( State, Cmd Msg )
actionAddScore state player score =
    let
        updateScore player' =
            if player'.id == player.id then
                { player' | score = player'.score + score }
            else
                player'

        players =
            List.map updateScore state.players

        play =
            Play
                (List.length state.plays)
                player.name
                player.id
                score

        plays =
            play :: state.plays

        totalScore =
            state.totalScore + score
    in
        ( { state
            | players = players
            , plays = plays
            , totalScore = totalScore
          }
        , Cmd.none
        )


actionDeletePlay : State -> Play -> ( State, Cmd Msg )
actionDeletePlay state play =
    let
        updateScore player =
            if player.id == play.playerId then
                { player | score = player.score - play.score }
            else
                player

        players =
            List.map updateScore state.players

        plays =
            state.plays
                |> List.filter (\{ id } -> id /= play.id)

        totalScore =
            state.totalScore - play.score
    in
        ( { state
            | players = players
            , plays = plays
            , totalScore = totalScore
          }
        , Cmd.none
        )


actionCancel : State -> ( State, Cmd Msg )
actionCancel state =
    ( { state
        | input = ""
        , playerId = Nothing
      }
    , Cmd.none
    )


actionSave : State -> ( State, Cmd Msg )
actionSave state =
    case state.playerId of
        Just id ->
            if state.input == "" then
                ( { state
                    | input = ""
                    , playerId = Nothing
                  }
                , Cmd.none
                )
            else
                updatePlayer state id

        Nothing ->
            if state.input == "" then
                ( state, Cmd.none )
            else
                savePlayer state


savePlayer : State -> ( State, Cmd Msg )
savePlayer state =
    let
        player =
            Player (List.length state.players) state.input 0

        players =
            player :: state.players
    in
        ( { state
            | players = players
            , input = ""
            , playerId = Nothing
          }
        , Cmd.none
        )


updatePlayer : State -> Id -> ( State, Cmd Msg )
updatePlayer state id =
    let
        updateNameInPlayers player =
            if player.id == id then
                { player | name = state.input }
            else
                player

        players =
            List.map updateNameInPlayers state.players

        updateNameInPlays play =
            if play.playerId == id then
                { play | playerName = state.input }
            else
                play

        plays =
            List.map updateNameInPlays state.plays
    in
        ( { state
            | players = players
            , plays = plays
            , input = ""
            , playerId = Nothing
          }
        , Cmd.none
        )



-- VIEW


pageTitle : Html Msg
pageTitle =
    h1 [] [ text "Scorekeeper" ]


playerForm : State -> Html Msg
playerForm state =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit Player..."
            , value state.input
            , onInput InputName
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
        ]


playersHeader : Html Msg
playersHeader =
    h3 [] [ text "Players:" ]


playerList : List Player -> Html Msg
playerList players =
    div []
        [ players
            |> List.sortBy .name
            |> List.map playerItem
            |> ul []
        ]


playerItem : Player -> Html Msg
playerItem player =
    li []
        [ text (player.name ++ " - " ++ (toString player.score) ++ " points ")
        , button [ onClick <| AddScore player 2 ] [ text "score 2 points" ]
        , button [ onClick <| AddScore player 3 ] [ text "score 3 points" ]
        , button [ onClick <| EditPlayer player ] [ text "Edit Player" ]
        ]


playsHeader : Html Msg
playsHeader =
    h3 [] [ text "Plays:" ]


playList : List Play -> Html Msg
playList plays =
    div []
        [ plays
            |> List.map playItem
            |> ul []
        ]


playItem : Play -> Html Msg
playItem play =
    li []
        [ text <| play.playerName ++ " " ++ (toString play.score) ++ " points "
        , button [ onClick <| DeletePlay play ] [ text "Delete" ]
        ]


totalScore : State -> Html Msg
totalScore state =
    h3 [] [ text <| "Total score: " ++ (toString state.totalScore) ]


view : State -> Html Msg
view state =
    div []
        [ pageTitle
        , playerForm state
        , playersHeader
        , playerList state.players
        , playsHeader
        , playList state.plays
        , totalScore state
        ]
