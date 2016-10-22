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


type alias Model =
    { players : List Player
    , plays : List Play
    , input : String
    , playerId : Maybe Int
    , totalScore : Int
    }


type alias Player =
    { id : Int
    , name : String
    , score : Int
    }


type alias Play =
    { id : Int
    , playerName : String
    , playerId : Int
    , score : Int
    }


type Msg
    = Input String
    | EditPlayer Player
    | Save
    | Cancel
    | Score Player Int
    | DeletePlay Play


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input name ->
            msgInput model name

        Cancel ->
            msgCancel model

        Save ->
            msgSave model

        EditPlayer player ->
            msgEditPlayer model player

        Score player score ->
            msgScore model player score

        DeletePlay play ->
            msgDeletePlay model play


msgInput : Model -> String -> ( Model, Cmd Msg )
msgInput model name =
    ( { model
        | input = name
      }
    , Cmd.none
    )


msgCancel : Model -> ( Model, Cmd Msg )
msgCancel model =
    ( { model
        | input = ""
        , playerId = Nothing
      }
    , Cmd.none
    )


msgSave : Model -> ( Model, Cmd Msg )
msgSave model =
    case model.playerId of
        Just id ->
            if model.input == "" then
                ( { model
                    | input = ""
                    , playerId = Nothing
                  }
                , Cmd.none
                )
            else
                updatePlayer model id

        Nothing ->
            if model.input == "" then
                ( model, Cmd.none )
            else
                savePlayer model


msgEditPlayer : Model -> Player -> ( Model, Cmd Msg )
msgEditPlayer model player =
    ( { model
        | input = player.name
        , playerId = Just player.id
      }
    , Cmd.none
    )


msgScore : Model -> Player -> Int -> ( Model, Cmd Msg )
msgScore model player score =
    let
        updateScore p =
            if p.id == player.id then
                { p | score = p.score + score }
            else
                p

        players =
            List.map updateScore model.players

        play =
            Play
                (List.length model.plays)
                player.name
                player.id
                score

        plays =
            play :: model.plays

        totalScore =
            model.totalScore + score
    in
        ( { model
            | players = players
            , plays = plays
            , totalScore = totalScore
          }
        , Cmd.none
        )


msgDeletePlay : Model -> Play -> ( Model, Cmd Msg )
msgDeletePlay model play =
    let
        plays =
            model.plays
                |> List.filter (\p -> p.id /= play.id)

        updateScore p =
            if p.id == play.playerId then
                { p | score = p.score - play.score }
            else
                p

        players =
            List.map updateScore model.players

        totalScore =
            model.totalScore - play.score
    in
        ( { model
            | players = players
            , plays = plays
            , totalScore = totalScore
          }
        , Cmd.none
        )


savePlayer : Model -> ( Model, Cmd Msg )
savePlayer model =
    let
        player =
            Player (List.length model.players) model.input 0

        players =
            player :: model.players
    in
        ( { model
            | players = players
            , input = ""
            , playerId = Nothing
          }
        , Cmd.none
        )


updatePlayer : Model -> Int -> ( Model, Cmd Msg )
updatePlayer model id =
    let
        updateName p =
            if p.id == id then
                { p | name = model.input }
            else
                p

        players =
            List.map updateName model.players

        updatePlayerName p =
            if p.playerId == id then
                { p | playerName = model.input }
            else
                p

        plays =
            List.map updatePlayerName model.plays
    in
        ( { model
            | players = players
            , plays = plays
            , input = ""
            , playerId = Nothing
          }
        , Cmd.none
        )


init : ( Model, Cmd Msg )
init =
    ( Model [] [] "" Nothing 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ pageTitle
        , playerForm model
        , playersHeader
        , playerList model.players
        , playsHeader
        , playList model.plays
        , totalScore model
        ]


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
        , button [ onClick (Score player 2) ] [ text "score 2 points" ]
        , button [ onClick (Score player 3) ] [ text "score 3 points" ]
        , button [ onClick (EditPlayer player) ] [ text "Edit Player" ]
        ]


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
        [ text (play.playerName ++ " " ++ (toString play.score) ++ " points ")
        , button [ onClick (DeletePlay play) ] [ text "Delete" ]
        ]


totalScore : Model -> Html Msg
totalScore model =
    h3 [] [ text ("Total score: " ++ (toString model.totalScore)) ]


pageTitle : Html Msg
pageTitle =
    h1 [] [ text "Scorekeeper" ]


playersHeader : Html Msg
playersHeader =
    h3 [] [ text "Players:" ]


playsHeader : Html Msg
playsHeader =
    h3 [] [ text "Plays:" ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit Player..."
            , value model.input
            , onInput Input
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
        ]
