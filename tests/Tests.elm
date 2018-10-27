module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


headerOrderTest : String -> HeaderViewModel -> HeaderViewModel -> Test
headerOrderTest testCaseDescription actual expected =
    test testCaseDescription <|
        \_ ->
            Expect.equal actual expected


monsterSortTest : String -> Main.Order -> List Monster -> List Monster -> Test
monsterSortTest testCaseDescription order monsters expected =
    test testCaseDescription <|
        \_ ->
            let
                actual =
                    sortMonsters order monsters
            in
            Expect.equal actual expected


suite : Test
suite =
    describe "モンスター図鑑のテスト"
        [ describe "まずはViewモデルのテスト"
            [ test "スライムをViewモデルにしてみる" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "スライム" 8 0 9 4

                        expected =
                            MonsterViewModel "スライム" "8" "0" "9" "4"
                    in
                    Expect.equal actual expected
            , test "おおがらすをViewモデルにしてみる" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "おおがらす" 9 0 10 6

                        expected =
                            MonsterViewModel "おおがらす" "9" "0" "10" "6"
                    in
                    Expect.equal actual expected
            , test "はぐれメタルをViewモデルにしてみる" <|
                \_ ->
                    let
                        actual =
                            monster2ViewModel <| Monster "はぐれメタル" 6 infinity 55 150

                        expected =
                            MonsterViewModel "はぐれメタル" "6" "∞" "55" "150"
                    in
                    Expect.equal actual expected
            ]
        , describe "次はHeader部分のテスト"
            [ headerOrderTest
                "デフォルトの時、全ての項目が暗い状態で、全ての矢印が上を向いている"
                (order2HeaderViewModel DefaultOrder)
                (HeaderViewModel
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                )
            , headerOrderTest "HPが昇順の時、HPの項目が明るい状態で、全ての矢印が上を向いている"
                (order2HeaderViewModel (Order Hp Asc))
                (HeaderViewModel
                    (HeaderFieldViewModel "active" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                )
            , headerOrderTest "MPが昇順の時、MPの項目が明るい状態で、全ての矢印が上を向いている"
                (order2HeaderViewModel (Order Mp Asc))
                (HeaderViewModel
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "active" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                )
            , headerOrderTest "HPが降順の時、HPの項目が明るい状態で、HPの矢印が下を向いていて、その他の矢印が上を向いている"
                (order2HeaderViewModel (Order Hp Dsc))
                (HeaderViewModel
                    (HeaderFieldViewModel "active" "arrow dsc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                )
            , headerOrderTest "こうげきりょくが降順の時、こうげきりょくの項目が明るい状態で、こうげきりょくの矢印が下を向いていて、その他の矢印が上を向いている"
                (order2HeaderViewModel (Order Attack Dsc))
                (HeaderViewModel
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "" "arrow asc")
                    (HeaderFieldViewModel "active" "arrow dsc")
                    (HeaderFieldViewModel "" "arrow asc")
                )
            ]
        , describe "モンスターのソートのテスト"
            [ monsterSortTest
                "HPが昇順のテスト"
                (Order Hp Asc)
                [ Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
                [ Monster "はぐれメタル" 6 infinity 55 150
                , Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "ドルイド" 35 10 55 29
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
            , monsterSortTest
                "HPが降順のテスト"
                (Order Hp Dsc)
                [ Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
                [ Monster "ゾーマ" 4700 infinity 360 80
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "おおがらす" 9 0 10 6
                , Monster "スライム" 8 0 9 4
                , Monster "はぐれメタル" 6 infinity 55 150
                ]
            , monsterSortTest
                "すばやさが降順のテスト"
                (Order Agility Dsc)
                [ Monster "スライム" 8 0 9 4
                , Monster "おおがらす" 9 0 10 6
                , Monster "さまようよろい" 55 0 47 10
                , Monster "ドルイド" 35 10 55 29
                , Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                ]
                [ Monster "はぐれメタル" 6 infinity 55 150
                , Monster "ゾーマ" 4700 infinity 360 80
                , Monster "ドルイド" 35 10 55 29
                , Monster "さまようよろい" 55 0 47 10
                , Monster "おおがらす" 9 0 10 6
                , Monster "スライム" 8 0 9 4
                ]
            ]
        ]
