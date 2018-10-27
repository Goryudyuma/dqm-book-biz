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
    , defaultHeaderViewModel
    , druido
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
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



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
    { active : String, dir : String }


type alias HeaderViewModel =
    { hp : HeaderFieldViewModel
    , mp : HeaderFieldViewModel
    , attack : HeaderFieldViewModel
    , agility : HeaderFieldViewModel
    }


defaultHeaderViewModel : HeaderViewModel
defaultHeaderViewModel =
    HeaderViewModel
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")
        (HeaderFieldViewModel "" "asc")


orderDir2String : Dir -> String
orderDir2String dir =
    case dir of
        Asc ->
            "asc"

        Dsc ->
            "dsc"


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
    let
        orderBy2Arrow : String -> String
        orderBy2Arrow dir =
            "arrow " ++ dir
    in
    tr []
        [ th []
            [ text "なまえ" ]
        , th [ class hp.active ]
            [ text "HP"
            , span [ class (orderBy2Arrow hp.dir) ] []
            ]
        , th [ class mp.active ]
            [ text "MP"
            , span [ class (orderBy2Arrow mp.dir) ] []
            ]
        , th [ class attack.active ]
            [ text "こうげきりょく"
            , span [ class (orderBy2Arrow mp.dir) ] []
            ]
        , th [ class agility.active ]
            [ text "すばやさ"
            , span [ class (orderBy2Arrow mp.dir) ] []
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
