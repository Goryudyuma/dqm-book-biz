module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (..)


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
            ]
        ]
