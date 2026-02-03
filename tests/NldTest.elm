module NldTest exposing (suite)

import Expect
import Nld exposing (choice, map, map2, nat, repeat, runList, runTake, token, tokenMatching, topK, tuple2, tuple3, word, words)
import Peach
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
            [ test "returns wanted tokens for simple word" <|
                \() ->
                    topK 5 (word "buy") []
                        |> List.head
                        |> Maybe.map (\s -> Set.member "buy" s)
                        |> Expect.equal (Just True)
            , test "returns next needed token after partial match" <|
                \() ->
                    -- Parser needs "buy" then "apples"
                    -- Input has "buy", so should suggest "apples"
                    topK 5 (tuple2 (word "buy") (word "apples")) [ "buy" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "apples" s)
                        |> Expect.equal (Just True)
            , test "returns multiple suggestions for choice" <|
                \() ->
                    topK 5 (choice [ word "delete", word "create" ]) []
                        |> List.head
                        |> Maybe.map (\s -> Set.member "delete" s && Set.member "create" s)
                        |> Expect.equal (Just True)
            , test "explores past first parser in sequence" <|
                \() ->
                    -- After matching "a", should suggest "b"
                    topK 5 (tuple2 (word "a") (word "b")) [ "a" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "b" s)
                        |> Expect.equal (Just True)
            , test "explores deeply nested parsers" <|
                \() ->
                    -- Parser: a -> b -> c, input has "a" and "b"
                    topK 5 (tuple3 (word "a") (word "b") (word "c")) [ "a", "b" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "c" s)
                        |> Expect.equal (Just True)
            ]
        , describe "Peach laziness"
            [ test "flatMap is lazy - doesn't evaluate all branches" <|
                \() ->
                    -- Create a peach with multiple items
                    -- flatMap should work lazily
                    Peach.peach [ ( 1.0, "a" ), ( 2.0, "b" ) ]
                        |> Peach.flatMap (\x -> Peach.peach [ ( 0.5, x ++ "1" ) ])
                        |> Peach.take 1
                        |> List.map Tuple.second
                        |> Expect.equal [ "a1" ]
            , test "lazy thunks are only evaluated when needed" <|
                \() ->
                    -- Use lazy to defer computation
                    Peach.lazy 0.0 (\() -> Peach.peach [ ( 1.0, "computed" ) ])
                        |> Peach.take 1
                        |> List.map Tuple.second
                        |> Expect.equal [ "computed" ]
            , test "succeed creates single-element peach" <|
                \() ->
                    Peach.succeed "hello"
                        |> Peach.head
                        |> Maybe.map Tuple.second
                        |> Expect.equal (Just "hello")
            ]
        ]
