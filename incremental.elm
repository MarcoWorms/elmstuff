module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time


-- MODEL


type alias Monster =
    { hp : Int
    , maxHp : Int
    , xpReward : Int
    , goldReward : Int
    , rebornStonesReward : Int
    }


type alias Player =
    { xp : Int
    , gold : Int
    , clickPower : Int
    , idlePower : Int
    , rebornStones : Int
    , rebornStonesToAdd : Int
    }


type alias Model =
    { player : Player
    , monster : Monster
    , level : Int
    }



-- UPDATE


type Msg
    = NoOp
    | HitMonster
    | Reborn
    | Tick Float


addReward : Player -> Monster -> Player
addReward player monster =
    { player
        | xp = player.xp + monster.xpReward
        , gold = player.gold + monster.goldReward
        , rebornStonesToAdd = monster.rebornStonesReward
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


newMonster : Int -> Monster
newMonster levelMultiplier =
    { hp = 100 * levelMultiplier
    , maxHp = 100 * levelMultiplier
    , xpReward = 1 * levelMultiplier
    , goldReward = 3 * levelMultiplier
    , rebornStonesReward = 1 * levelMultiplier // 10
    }


getLevel xp =
    let
        xpTable =
            [ 0, 5, 15, 30, 50, 80, 140, 220, 320, 450, 600 ]
    in
        (List.filter (\x -> x <= xp) xpTable) |> List.length


checkDeadMonster : Model -> Model
checkDeadMonster model =
    let
        currentMonster =
            model.monster

        monsterIsDead =
            currentMonster.hp <= 0

        newPlayer =
            addReward model.player currentMonster
    in
        if monsterIsDead then
            { model
                | player = newPlayer
                , level = getLevel newPlayer.xp
                , monster = newMonster (getLevel newPlayer.xp)
            }
        else
            model


modelAfterHit : Model -> Int -> Model
modelAfterHit model damage =
    let
        damagedMonster =
            damageMonster model damage
    in
        checkDeadMonster damagedMonster


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Msg: " msg of
        NoOp ->
            model ! []

        HitMonster ->
            modelAfterHit model model.player.clickPower ! []

        Reborn ->
            let
                player =
                    initialPlayerModel.player

                newRebornStones =
                    model.player.rebornStones + model.player.rebornStonesToAdd
            in
                { initialPlayerModel
                    | player = { player | rebornStones = newRebornStones }
                }
                    ! []

        Tick time ->
            modelAfterHit model model.player.idlePower ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ br [] []
        , p [] [ text <| "Monster Hp: " ]
        , p [] [ text <| List.foldr (++) "" (List.repeat ((((model.monster.hp * 100) // model.monster.maxHp) // 10) + 1) "|") ]
        , p [] [ text <| toString model.monster.hp ++ " / " ++ toString model.monster.maxHp ]
        , br [] []
        , p [] [ text <| "Your Stats:" ]
        , p [] [ text <| "Level: " ++ (toString model.level) ]
        , p [] [ text <| "- Xp: " ++ (toString model.player.xp) ]
        , p [] [ text <| "- Gold: " ++ (toString model.player.gold) ]
          --, p [] [ text <| "- Click Power: " ++ (toString model.player.clickPower) ]
        , p [] [ text <| "- Idle Power: " ++ (toString model.player.idlePower) ]
        , br [] []
        , p [] [ text <| "- Reborn Stones: " ++ (toString model.player.rebornStones) ]
        , p [] [ text <| "Reborn Stones next reborn: +" ++ (toString model.player.rebornStonesToAdd) ]
          --, button [ onClick HitMonster ] [ text "Hit!" ]
        , button [ onClick Reborn ] [ text "Reborn" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.second / 10) Tick
        ]



-- INIT


initialPlayerModel : Model
initialPlayerModel =
    let
        player =
            { xp = 0
            , gold = 0
            , clickPower = 1
            , idlePower = 1
            , rebornStones = 0
            , rebornStonesToAdd = 0
            }

        monster =
            newMonster 1
    in
        Model player monster 1


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
