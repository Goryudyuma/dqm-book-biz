module Main exposing (By(..), Dir(..), HeaderFieldViewModel, HeaderViewModel, Model, Monster, MonsterViewModel, Msg(..), Order(..), druido, hagreMetal, infinity, init, main, monster2ViewModel, monsterFieldView, ogarasu, order2HeaderViewModel, samayouYoroi, slime, subscriptions, update, view, zoma)

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


type Dir
    = Asc
    | Dsc


type By
    = Hp
    | Mp
    | Attack
    | Agility


type Order
    = DefaultOrder
    | Order By Dir


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
        , tbody [] <|
            List.map monsterFieldView
                [ MonsterViewModel "スライム" "8" "0" "9" "4"
                , MonsterViewModel "おおがらす" "9" "0" "10" "6"
                , MonsterViewModel "さまようよろい" "55" "0" "47" "10"
                , MonsterViewModel "ドルイド" "35" "10" "55" "29"
                , MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
                , MonsterViewModel "ゾーマ" "4700" "∞" "360" "80"
                ]
        ]


type alias MonsterViewModel =
    { name : String, hp : String, mp : String, attack : String, agility : String }


monster2ViewModel : Monster -> MonsterViewModel
monster2ViewModel { name, hp, mp, attack, agility } =
    MonsterViewModel
        name
        (String.fromInt hp)
        (if mp == infinity then
            "∞"

         else
            String.fromFloat mp
        )
        (String.fromInt attack)
        (String.fromInt agility)


monsterFieldView : MonsterViewModel -> Html Msg
monsterFieldView { name, hp, mp, attack, agility } =
    tr []
        [ td [] [ text name ]
        , td [] [ text hp ]
        , td [] [ text mp ]
        , td [] [ text attack ]
        , td [] [ text agility ]
        ]


type alias HeaderFieldViewModel =
    { active : String, dir : String }


type alias HeaderViewModel =
    { hp : HeaderFieldViewModel
    , mp : HeaderFieldViewModel
    , attack : HeaderFieldViewModel
    , agility : HeaderFieldViewModel
    }


order2HeaderViewModel : Order -> HeaderViewModel
order2HeaderViewModel _ =
    HeaderViewModel
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")


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
