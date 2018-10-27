module Main exposing
    ( By(..)
    , Dir(..)
    , HeaderFieldViewModel
    , HeaderViewModel
    , Model
    , Monster
    , MonsterViewModel
    , Msg(..)
    , Order(..)
    , ascComparison
    , changeOrder
    , defaultHeaderViewModel
    , druido
    , dscComparison
    , hagreMetal
    , headerViewModel2View
    , infinity
    , init
    , main
    , monster2ViewModel
    , monsterFieldView
    , ogarasu
    , order2HeaderViewModel
    , orderDir2String
    , samayouYoroi
    , slime
    , sortMonsters
    , subscriptions
    , update
    , view
    , zoma
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)



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


ascComparison : comparable -> comparable -> Basics.Order
ascComparison a b =
    compare a b


dscComparison : comparable -> comparable -> Basics.Order
dscComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


sortMonsters : Order -> List Monster -> List Monster
sortMonsters order monsters =
    case order of
        DefaultOrder ->
            monsters

        Order by dir ->
            let
                comparison f m1 m2 =
                    case dir of
                        Asc ->
                            ascComparison (f m1) (f m2)

                        Dsc ->
                            dscComparison (f m1) (f m2)
            in
            case by of
                Hp ->
                    List.sortWith (comparison .hp) monsters

                Mp ->
                    List.sortWith (comparison .mp) monsters

                Attack ->
                    List.sortWith (comparison .attack) monsters

                Agility ->
                    List.sortWith (comparison .agility) monsters


changeOrder : By -> Order -> Order
changeOrder nextBy order =
    case order of
        DefaultOrder ->
            Order nextBy Asc

        Order nowBy dir ->
            if nowBy == nextBy && dir == Asc then
                Order nextBy Dsc

            else
                Order nextBy Asc


type alias Model =
    { order : Order
    , monsters : List Monster
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { order = DefaultOrder
      , monsters =
            [ slime
            , ogarasu
            , samayouYoroi
            , druido
            , hagreMetal
            , zoma
            ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Sort By


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ order } as model) =
    case msg of
        Sort targetBy ->
            ( { model | order = changeOrder targetBy order }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    table []
        [ thead []
            [ headerViewModel2View <| order2HeaderViewModel model.order
            ]
        , tbody [] <|
            (model.monsters
                |> sortMonsters model.order
                |> List.map monster2ViewModel
                |> List.map monsterFieldView
            )
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
    { active : String, arrow : String }


type alias HeaderViewModel =
    { hp : HeaderFieldViewModel
    , mp : HeaderFieldViewModel
    , attack : HeaderFieldViewModel
    , agility : HeaderFieldViewModel
    }


defaultHeaderViewModel : HeaderViewModel
defaultHeaderViewModel =
    HeaderViewModel
        (HeaderFieldViewModel "" "arrow asc")
        (HeaderFieldViewModel "" "arrow asc")
        (HeaderFieldViewModel "" "arrow asc")
        (HeaderFieldViewModel "" "arrow asc")


orderDir2String : Dir -> String
orderDir2String dir =
    case dir of
        Asc ->
            "arrow asc"

        Dsc ->
            "arrow dsc"


order2HeaderViewModel : Order -> HeaderViewModel
order2HeaderViewModel order =
    case order of
        DefaultOrder ->
            defaultHeaderViewModel

        Order by dir ->
            case by of
                Hp ->
                    { defaultHeaderViewModel
                        | hp = HeaderFieldViewModel "active" (orderDir2String dir)
                    }

                Mp ->
                    { defaultHeaderViewModel
                        | mp = HeaderFieldViewModel "active" (orderDir2String dir)
                    }

                Attack ->
                    { defaultHeaderViewModel
                        | attack = HeaderFieldViewModel "active" (orderDir2String dir)
                    }

                Agility ->
                    { defaultHeaderViewModel
                        | agility = HeaderFieldViewModel "active" (orderDir2String dir)
                    }


headerViewModel2View : HeaderViewModel -> Html Msg
headerViewModel2View { hp, mp, attack, agility } =
    tr []
        [ th []
            [ text "なまえ" ]
        , th [ class hp.active, onClick <| Sort Hp ]
            [ text "HP"
            , span [ class hp.arrow ] []
            ]
        , th [ class mp.active, onClick <| Sort Mp ]
            [ text "MP"
            , span [ class mp.arrow ] []
            ]
        , th [ class attack.active, onClick <| Sort Attack ]
            [ text "こうげきりょく"
            , span [ class attack.arrow ] []
            ]
        , th [ class agility.active, onClick <| Sort Agility ]
            [ text "すばやさ"
            , span [ class agility.arrow ] []
            ]
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
