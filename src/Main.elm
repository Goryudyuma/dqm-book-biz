module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)



---- MODEL ----


infinity : Float
infinity =
    1 / 0


slime =
    Monster "スライム" 8 0 9 4


ogarasu =
    Monster "おおがらす" 9 0 10 6


samayouYoroi =
    Monster "さまようよろい" 55 0 47 10


druido =
    Monster "ドルイド" 35 10 55 29


hagreMetal =
    Monster "はぐれメタル" 6 infinity 55 150


zoma =
    Monster "ゾーマ" 4700 infinity 360 80


type alias Monster =
    { name : String, hp : Int, mp : Float, attack : Int, agility : Int }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ tr []
                [ th []
                    [ text "なまえ" ]
                , th [ class "" ]
                    [ text "HP"
                    , span [ class "arrow asc" ] []
                    ]
                , th [ class "" ]
                    [ text "MP"
                    , span [ class "arrow asc" ] []
                    ]
                , th [ class "" ]
                    [ text "こうげきりょく"
                    , span [ class "arrow asc" ] []
                    ]
                , th [ class "" ]
                    [ text "すばやさ"
                    , span [ class "arrow asc" ] []
                    ]
                ]
            ]
        , tbody []
            --  <|  List.map monsterViewModel2View
            --        [ MonsterViewModel "スライム" "8" "0" "9" "4"
            --        , MonsterViewModel "おおがらす" "9" "0" "10" "6"
            --        , MonsterViewModel "さまようよろい" "55" "0" "47" "10"
            --        , MonsterViewModel "ドルイド" "35" "10" "55" "29"
            --        , MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
            --        , MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
            --      ]
            [ tr []
                [ td [] [ text "スライム" ]
                , td [] [ text "8" ]
                , td [] [ text "0" ]
                , td [] [ text "9" ]
                , td [] [ text "4" ]
                ]
            , tr []
                [ td [] [ text "おおがらす" ]
                , td [] [ text "9" ]
                , td [] [ text "0" ]
                , td [] [ text "10" ]
                , td [] [ text "6" ]
                ]
            , tr []
                [ td [] [ text "さまようよろい" ]
                , td [] [ text "55" ]
                , td [] [ text "0" ]
                , td [] [ text "47" ]
                , td [] [ text "10" ]
                ]
            , tr []
                [ td [] [ text "ドルイド" ]
                , td [] [ text "35" ]
                , td [] [ text "10" ]
                , td [] [ text "55" ]
                , td [] [ text "29" ]
                ]
            , tr []
                [ td [] [ text "はぐれメタル" ]
                , td [] [ text "6" ]
                , td [] [ text "∞" ]
                , td [] [ text "55" ]
                , td [] [ text "150" ]
                ]
            , tr []
                [ td [] [ text "ゾーマ" ]
                , td [] [ text "4700" ]
                , td [] [ text "∞" ]
                , td [] [ text "360" ]
                , td [] [ text "80" ]
                ]
            ]
        ]


type alias MonsterViewModel =
    { name : String, hp : String, mp : String, attack : String, agility : String }


monster2ViewModel : Monster -> MonsterViewModel
monster2ViewModel _ =
    MonsterViewModel
        "スライム"
        "8"
        "0"
        "9"
        "4"

monsterFieldView : MonsterViewModel -> Html Msg
monsterFieldView { name, hp, mp, attack, agility } =
    tr []
        [ td [] [ text name ]
        , td [] [ text hp ]
        , td [] [ text mp ]
        , td [] [ text attack ]
        , td [] [ text agility ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
