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
    { level : Int
    , xp : Int
    , gold : Int
    , clickPower : Int
    , idlePower : Int
    , rebornStones : Int
    , rebornStonesToAdd : Int
    }


type alias Model =
    { player : Player
    , monster : Monster
    }



-- UPDATE


type Msg
    = NoOp
    | HitMonster
    | Reborn
    | Tick Float


getLevel : Int -> Int
getLevel xp =
    let
        xpTable =
            [ 0, 5, 15, 30, 50, 80, 140, 220, 320, 450, 600 ]
    in
        (List.filter (\x -> x <= xp) xpTable) |> List.length


addReward : Player -> Monster -> Player
addReward player monster =
    let
        newXp =
            player.xp + monster.xpReward

        newGold =
            player.gold + monster.goldReward

        newRebornStonesToAdd =
            monster.rebornStonesReward

        newLevel =
            getLevel player.xp
    in
        { player
            | xp = newXp
            , gold = newGold
            , rebornStonesToAdd = newRebornStonesToAdd
            , level = newLevel
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

        newPlayer =
            addReward model.player currentMonster
    in
        if monsterIsDead then
            { model
                | player = newPlayer
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


makeHpBar : Int -> Int -> String -> String
makeHpBar hp maxHp char =
    List.foldr (++) char (List.repeat ((((hp * 100) // maxHp) // 10)) char)


view : Model -> Html Msg
view model =
    let
        makeMonsterHpBar =
            makeHpBar model.monster.hp model.monster.maxHp "|"

        mainDivStyle =
            style
                [ ( "display", "block" )
                , ( "position", "absolute" )
                , ( "top", "10px" )
                , ( "left", "20px" )
                ]
    in
        div [ mainDivStyle ]
            [ br [] []
            , p [] [ text <| "Monster Hp: " ]
            , p [] [ text <| makeMonsterHpBar ]
            , p [] [ text <| toString model.monster.hp ++ " / " ++ toString model.monster.maxHp ]
            , br [] []
            , p [] [ text <| "Your Stats:" ]
            , p [] [ text <| "- Level: " ++ (toString model.player.level) ]
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


newMonster : Int -> Monster
newMonster levelMultiplier =
    { hp = 100 * levelMultiplier
    , maxHp = 100 * levelMultiplier
    , xpReward = 1 * levelMultiplier
    , goldReward = 3 * levelMultiplier
    , rebornStonesReward = 1 * levelMultiplier // 10
    }


initialPlayerModel : Model
initialPlayerModel =
    let
        player =
            { level = 1
            , xp = 0
            , gold = 0
            , clickPower = 1
            , idlePower = 1
            , rebornStones = 0
            , rebornStonesToAdd = 0
            }

        monster =
            newMonster 1
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
