module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)


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
        ]
