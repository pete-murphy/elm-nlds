module NldTest exposing (suite)

import Expect
import Nld exposing (choice, map, nat, repeat, runList, runTake, token, tokenMatching, topK, tuple2, word, words)
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Nld"
        [ describe "word"
            [ test "matches a word at position 0" <|
                \() ->
                    runList (word "hello") [ "hello", "world" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "hello" ]
            , test "matches a word at later position" <|
                \() ->
                    runList (word "cat") [ "the", "cat", "sat" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "cat" ]
            , test "ignores non-matching tokens" <|
                \() ->
                    runList (word "delete") [ "please", "delete", "this" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "delete" ]
            ]
        , describe "words (synonyms)"
            [ test "canonicalizes to first word" <|
                \() ->
                    runList (words [ "delete", "remove", "erase" ]) [ "erase" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "delete" ]
            , test "matches any synonym" <|
                \() ->
                    runList (words [ "yes", "yeah", "yep" ]) [ "yeah" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "yes" ]
            ]
        , describe "tuple2"
            [ test "combines two parsers" <|
                \() ->
                    runList (tuple2 (word "delete") (word "file")) [ "delete", "file" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ ( "delete", "file" ) ]
            , test "handles out-of-order tokens" <|
                \() ->
                    runList (tuple2 (word "delete") (word "file")) [ "file", "delete" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ ( "delete", "file" ) ]
            ]
        , describe "token"
            [ test "matches any token" <|
                \() ->
                    runTake 1 token [ "anything" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "anything" ]
            ]
        , describe "nat"
            [ test "matches natural numbers" <|
                \() ->
                    runList (tuple2 (word "buy") nat) [ "buy", "3", "apples" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ ( "buy", 3 ) ]
            ]
        , describe "tokenMatching"
            [ test "matches tokens satisfying predicate" <|
                \() ->
                    runList (tokenMatching (String.endsWith ".txt")) [ "open", "report.txt" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "report.txt" ]
            ]
        , describe "choice"
            [ test "tries multiple parsers" <|
                \() ->
                    runList (choice [ word "delete", word "add" ]) [ "delete" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ "delete" ]
            ]
        , describe "repeat"
            [ test "matches zero occurrences" <|
                \() ->
                    runTake 1 (repeat (word "x")) [ "a", "b" ]
                        |> List.map Tuple.second
                        |> Expect.equal [ [] ]
            ]
        , describe "map"
            [ test "transforms results" <|
                \() ->
                    runList (map String.toUpper (word "hello")) [ "hello" ]
                        |> Expect.equal [ ( 1, "HELLO" ) ]
            ]
        , describe "topK (autocomplete)"
            [ test "returns wanted tokens" <|
                \() ->
                    topK 5 (word "buy") []
                        |> List.head
                        |> Maybe.map (\s -> Set.member "buy" s)
                        |> Expect.equal (Just True)
            ]
        ]
