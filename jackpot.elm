module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random
import List


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { dieFace : List Int
    , plays : Int
    , wins : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model [ 0, 0, 7 ] 0 0, Cmd.none )



-- UPDATE


type Msg
    = Roll Int
    | NewFace (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll rollAmmount ->
          let
            generateRandomRolls =
              Cmd.batch (List.repeat rollAmmount (Random.generate NewFace (Random.list 3 (Random.int 1 9))))
          in
            ( model, generateRandomRolls )

        NewFace newFaces ->
          let
            playerWon =
              checkPlayerVictory model

            addStats model =
              if playerWon then
                { model | plays = model.plays + 1, dieFace = newFaces, wins = model.wins + 1}
              else
                { model | plays = model.plays + 1, dieFace = newFaces}
          in
            ( addStats model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


getDicesFromModel : Model -> Int -> Int
getDicesFromModel model index =
    Maybe.withDefault 0 (List.head (List.drop index model.dieFace))


bigSpanStyle : Attribute msg
bigSpanStyle =
    style
        [ ( "fontSize", "50px" )
        ]


victoryText playStatus =
    p
        [ style [ ( "color", playStatus.color ) ] ]
        [ text playStatus.text ]


checkPlayerVictory model =
  let
    getDice =
        getDicesFromModel model
  in
    getDice 0 == getDice 1 && getDice 1 == getDice 2

view : Model -> Html Msg
view model =
    let
        getDice =
            getDicesFromModel model

        playerWon =
          checkPlayerVictory model

        playStatus =
            if playerWon then
                { color = "green"
                , text = "Won!"
                }
            else
                { color = "red"
                , text = "Lost!"
                }
    in
        div []
            [ span [ bigSpanStyle ] [ text (toString (getDice 0)) ]
            , span [ bigSpanStyle ] [ text (toString (getDice 1)) ]
            , span [ bigSpanStyle ] [ text (toString (getDice 2)) ]
            , victoryText playStatus
            , p [] [ text ("Total: " ++ toString model.plays) ]
            , p [] [ text ("Wins: " ++ toString model.wins) ]
            , button [ onClick (Roll 1) ] [ text "Roll" ]
            , button [ onClick (Roll 10) ] [ text "Roll 10" ]
            , button [ onClick (Roll 25) ] [ text "Roll 25" ]
            , button [ onClick (Roll 50) ] [ text "Roll 50" ]
            , button [ onClick (Roll 100) ] [ text "Roll 100" ]
            ]
