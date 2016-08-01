module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)


-- MODEL


type alias Monster =
    { hp : Int
    , maxHp : Int
    , xpReward : Int
    , goldReward : Int
    }


type alias Player =
    { xp : Int
    , gold : Int
    , clickPower : Int
    , idlePower : Int
    }


type alias Model =
    { player : Player
    , monster : Monster
    }



-- UPDATE


type Msg
    = NoOp
    | HitMonster Int


addReward : Player -> Monster -> Player
addReward player monster =
    { player
        | xp = player.xp + monster.xpReward
        , gold = player.gold + monster.goldReward
    }


damageMonster : Model -> Int -> Model
damageMonster model damage =
    let
        monster =
            model.monster

        damagedMonster =
            { monster | hp = monster.hp - damage }
    in
        { model | monster = damagedMonster }


checkDeadMonster : Model -> Model
checkDeadMonster model =
    let
        currentMonster =
            model.monster

        monsterIsDead =
            currentMonster.hp <= 0

        newMonster =
            { hp = 10
            , maxHp = 10
            , xpReward = 1
            , goldReward = 2
            }
    in
        if monsterIsDead then
            { model
                | player = addReward model.player currentMonster
                , monster = newMonster
            }
        else
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Msg: " msg of
        NoOp ->
            model ! []

        HitMonster damage ->
            let
                damagedMonster =
                    damageMonster model damage
            in
                checkDeadMonster damagedMonster ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text <| "Monster: " ++ (toString model.monster.hp ++ " / " ++ toString model.monster.maxHp) ]
        , p [] [ text <| "XP: " ++ (toString model.player.xp) ]
        , p [] [ text <| "Gold: " ++ (toString model.player.gold) ]
        , button [ onClick (HitMonster 1) ] [ text "Hit!" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


initialPlayerModel : Model
initialPlayerModel =
    let
        player =
            { xp = 0
            , gold = 0
            , clickPower = 1
            , idlePower = 1
            }

        monster =
            { hp = 10
            , maxHp = 10
            , xpReward = 1
            , goldReward = 2
            }
    in
        Model player monster


init : ( Model, Cmd Msg )
init =
    initialPlayerModel ! []


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
