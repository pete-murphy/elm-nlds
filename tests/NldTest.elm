module NldTest exposing (suite)

import Expect
import Nld exposing (andMap, choice, indexedNat, indexedTokenMatching, indexedWord, indexedWords, map, map2, minimalToken, nat, repeat, runList, runTake, succeed, token, tokenMatching, topK, tuple2, tuple3, word, words)
import Peach
import Set
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Nld"
        [ -- ============================================
          -- Tests from Unison library: Nld.tests.pure
          -- ============================================
          describe "pure/succeed"
            [ test "pure returns value unchanged with empty tokens" <|
                \() ->
                    runList (succeed "hello") []
                        |> Expect.equal [ "hello" ]
            , test "pure returns value unchanged with ignored tokens" <|
                \() ->
                    runList (succeed 42) [ "ignored", "tokens" ]
                        |> Expect.equal [ 42 ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.word
        -- ============================================
        , describe "word"
            [ test "matches exact token" <|
                \() ->
                    runList (word "cat") [ "cat" ]
                        |> Expect.equal [ "cat" ]
            , test "returns empty for non-matching token" <|
                \() ->
                    runList (word "cat") [ "dog" ]
                        |> Expect.equal []
            , test "matches word in middle of tokens" <|
                \() ->
                    runList (word "cat") [ "the", "cat" ]
                        |> Expect.equal [ "cat" ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.token
        -- ============================================
        , describe "token"
            [ test "captures single token" <|
                \() ->
                    runList token [ "hello" ]
                        |> Expect.equal [ "hello" ]
            , test "captures multiple tokens - one result per position" <|
                \() ->
                    List.length (runList token [ "a", "b", "c" ])
                        |> Expect.equal 3
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.nat
        -- ============================================
        , describe "nat"
            [ test "parses natural number" <|
                \() ->
                    runList nat [ "42" ]
                        |> Expect.equal [ 42 ]
            , test "returns empty for non-number" <|
                \() ->
                    runList nat [ "hello" ]
                        |> Expect.equal []
            , test "finds number among other tokens" <|
                \() ->
                    runList nat [ "the", "123", "cats" ]
                        |> Expect.equal [ 123 ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.tokenMatching
        -- ============================================
        , describe "tokenMatching"
            [ test "filters by predicate - matches" <|
                \() ->
                    runList (tokenMatching (String.startsWith "a")) [ "apple" ]
                        |> Expect.equal [ "apple" ]
            , test "filters by predicate - no match" <|
                \() ->
                    runList (tokenMatching (String.startsWith "a")) [ "banana" ]
                        |> Expect.equal []
            , test "filters by predicate - multiple matches" <|
                \() ->
                    let
                        results =
                            runList (tokenMatching (String.startsWith "a")) [ "apple", "banana", "avocado" ]
                    in
                    Expect.all
                        [ \r -> List.length r |> Expect.equal 2
                        , \r -> List.member "apple" r |> Expect.equal True
                        , \r -> List.member "avocado" r |> Expect.equal True
                        ]
                        results
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.map
        -- ============================================
        , describe "map"
            [ test "transforms word result" <|
                \() ->
                    runList (map String.toUpper (word "cat")) [ "cat" ]
                        |> Expect.equal [ "CAT" ]
            , test "transforms nat result" <|
                \() ->
                    runList (map (\n -> n * 2) nat) [ "5" ]
                        |> Expect.equal [ 10 ]
            , test "transforms multiple token results" <|
                \() ->
                    List.length (runList (map String.length token) [ "a", "bb", "ccc" ])
                        |> Expect.equal 3
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.map2
        -- ============================================
        , describe "map2"
            [ test "combines parsers in order" <|
                \() ->
                    runList (map2 Tuple.pair (word "red") (word "ball")) [ "red", "ball" ]
                        |> Expect.equal [ ( "red", "ball" ) ]
            , test "combines parsers regardless of token order" <|
                \() ->
                    runList (map2 Tuple.pair (word "red") (word "ball")) [ "ball", "red" ]
                        |> Expect.equal [ ( "red", "ball" ) ]
            , test "ignores irrelevant tokens" <|
                \() ->
                    runList (map2 Tuple.pair (word "red") (word "ball")) [ "the", "red", "ball" ]
                        |> Expect.equal [ ( "red", "ball" ) ]
            , test "ignores tokens between matches" <|
                \() ->
                    runList (map2 Tuple.pair (word "red") (word "ball")) [ "red", "big", "ball" ]
                        |> Expect.equal [ ( "red", "ball" ) ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.map2Nondeterminism
        -- ============================================
        , describe "map2 nondeterminism"
            [ test "produces multiple results for repeated tokens" <|
                \() ->
                    let
                        ab =
                            map2 Tuple.pair (word "a") (word "b")

                        results =
                            runList ab [ "a", "b", "a", "b" ]
                    in
                    List.length results |> Expect.equal 4
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.choice
        -- ============================================
        , describe "choice"
            [ test "matches first alternative" <|
                \() ->
                    runList (choice [ word "cat", word "dog" ]) [ "cat" ]
                        |> Expect.equal [ "cat" ]
            , test "matches second alternative" <|
                \() ->
                    runList (choice [ word "cat", word "dog" ]) [ "dog" ]
                        |> Expect.equal [ "dog" ]
            , test "returns empty when nothing matches" <|
                \() ->
                    runList (choice [ word "cat", word "dog" ]) [ "bird" ]
                        |> Expect.equal []
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.choiceNondeterminism
        -- ============================================
        , describe "choice nondeterminism"
            [ test "returns results from all matching alternatives" <|
                \() ->
                    let
                        catOrDog =
                            choice [ word "cat", word "dog" ]

                        results =
                            runList catOrDog [ "cat", "dog" ]
                    in
                    Expect.all
                        [ \r -> List.length r |> Expect.equal 2
                        , \r -> List.member "cat" r |> Expect.equal True
                        , \r -> List.member "dog" r |> Expect.equal True
                        ]
                        results
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.multipleOccurrences
        -- ============================================
        , describe "multiple occurrences"
            [ test "finds word at multiple positions" <|
                \() ->
                    let
                        results =
                            runList (word "cat") [ "cat", "dog", "cat" ]
                    in
                    List.length results |> Expect.equal 2
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.repeat
        -- ============================================
        , describe "repeat"
            [ test "includes empty list when no matches" <|
                \() ->
                    let
                        cats =
                            repeat (word "cat")

                        results =
                            runList cats [ "dog" ]
                    in
                    List.member [] results |> Expect.equal True
            , test "collects multiple matches" <|
                \() ->
                    let
                        cats =
                            repeat (word "cat")

                        results =
                            runList cats [ "cat", "cat" ]
                    in
                    List.member [ "cat", "cat" ] results |> Expect.equal True
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.repeatNat
        -- ============================================
        , describe "repeat nat"
            [ test "collects multiple numbers" <|
                \() ->
                    let
                        nums =
                            repeat nat

                        results =
                            runList nums [ "1", "2", "3" ]
                    in
                    List.any (\r -> List.length r == 3) results |> Expect.equal True
            ]

        -- ============================================
        -- Tests from Unison library: Nld.repeat.tests.prefixes
        -- ============================================
        , describe "repeat prefixes"
            [ test "returns all prefixes of greedy collection" <|
                \() ->
                    runList (repeat nat) [ "a", "1", "b", "2", "c", "3" ]
                        |> Expect.equal [ [ 1, 2, 3 ], [ 1, 2 ], [ 1 ], [] ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.wordAndNat
        -- ============================================
        , describe "word and nat"
            [ test "parses number with buy/apples context" <|
                \() ->
                    let
                        buyApples =
                            map2 (\n _ -> n) nat (map2 Tuple.pair (word "buy") (word "apples"))

                        results =
                            runList buyApples [ "buy", "5", "apples" ]
                    in
                    results |> Expect.equal [ 5 ]
            , test "parses number with reordered buy/apples" <|
                \() ->
                    let
                        buyApples =
                            map2 (\n _ -> n) nat (map2 Tuple.pair (word "buy") (word "apples"))

                        results =
                            runList buyApples [ "apples", "buy", "10" ]
                    in
                    results |> Expect.equal [ 10 ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.priorityOrdering
        -- ============================================
        , describe "priority ordering"
            [ test "produces all combinations for repeated patterns" <|
                \() ->
                    let
                        theFox =
                            map2 Tuple.pair (word "the") (word "fox")

                        results =
                            runList theFox [ "the", "fox", "the", "fox" ]
                    in
                    List.length results |> Expect.equal 4
            ]

        -- ============================================
        -- Tests from Unison library: Nld.indexed.tests
        -- ============================================
        , describe "indexed"
            [ test "indexedWord returns position" <|
                \() ->
                    runList (indexedWord "cat") [ "the", "cat", "sat" ]
                        |> Expect.equal [ ( "cat", 1 ) ]
            , test "indexedWords returns first word and position" <|
                \() ->
                    runList (indexedWords [ "hello", "hi" ]) [ "hi", "there" ]
                        |> Expect.equal [ ( "hello", 0 ) ]
            , test "indexedNat returns number and position" <|
                \() ->
                    runList indexedNat [ "buy", "42", "apples" ]
                        |> Expect.equal [ ( 42, 1 ) ]
            , test "indexedTokenMatching returns token and position" <|
                \() ->
                    runList (indexedTokenMatching (String.startsWith "a")) [ "the", "apple" ]
                        |> Expect.equal [ ( "apple", 1 ) ]
            , test "word still works (uses indexedWord)" <|
                \() ->
                    runList (word "cat") [ "cat" ]
                        |> Expect.equal [ "cat" ]
            , test "nat still works (uses indexedNat)" <|
                \() ->
                    runList nat [ "42" ]
                        |> Expect.equal [ 42 ]
            ]

        -- ============================================
        -- Tests from Unison library: Nld.autocomplete.tests.basic
        -- ============================================
        , describe "autocomplete"
            [ test "word parser wants its token when missing" <|
                \() ->
                    topK 5 (word "cat") []
                        |> Expect.equal [ Set.singleton "cat" ]
            , test "no suggestions when parse succeeds" <|
                \() ->
                    topK 5 (word "cat") [ "cat" ]
                        |> Expect.equal []
            , test "map2 suggests second parser after first matches" <|
                \() ->
                    topK 5 (map2 Tuple.pair (word "buy") (word "apples")) [ "buy" ]
                        |> Expect.equal [ Set.singleton "apples" ]
            , test "map2 suggests first parser when nothing matches" <|
                \() ->
                    topK 5 (map2 Tuple.pair (word "buy") (word "apples")) []
                        |> Expect.equal [ Set.singleton "buy" ]
            , test "map2 no suggestions when complete" <|
                \() ->
                    topK 5 (map2 Tuple.pair (word "buy") (word "apples")) [ "buy", "apples" ]
                        |> Expect.equal []
            , test "choice shows merged wanted" <|
                \() ->
                    let
                        result =
                            topK 5 (choice [ word "cat", word "dog" ]) []
                    in
                    List.any (\r -> Set.member "cat" r && Set.member "dog" r) result
                        |> Expect.equal True
            ]

        -- ============================================
        -- Tests from Unison library: Nld.tests.examples (comprehensive)
        -- ============================================
        , describe "examples (comprehensive)"
            [ test "pure returns value unchanged" <|
                \() ->
                    Expect.all
                        [ \() -> runList (succeed "hello") [] |> Expect.equal [ "hello" ]
                        , \() -> runList (succeed 42) [ "ignored", "tokens" ] |> Expect.equal [ 42 ]
                        ]
                        ()
            , test "word matches exact token" <|
                \() ->
                    Expect.all
                        [ \() -> runList (word "cat") [ "cat" ] |> Expect.equal [ "cat" ]
                        , \() -> runList (word "cat") [ "dog" ] |> Expect.equal []
                        , \() -> runList (word "cat") [ "the", "cat" ] |> Expect.equal [ "cat" ]
                        ]
                        ()
            , test "token captures any single token" <|
                \() ->
                    Expect.all
                        [ \() -> runList token [ "hello" ] |> Expect.equal [ "hello" ]
                        , \() -> List.length (runList token [ "a", "b", "c" ]) |> Expect.equal 3
                        ]
                        ()
            , test "nat parses natural numbers" <|
                \() ->
                    Expect.all
                        [ \() -> runList nat [ "42" ] |> Expect.equal [ 42 ]
                        , \() -> runList nat [ "hello" ] |> Expect.equal []
                        , \() -> runList nat [ "the", "123", "cats" ] |> Expect.equal [ 123 ]
                        ]
                        ()
            , test "tokenMatching filters by predicate" <|
                \() ->
                    let
                        startsWithA =
                            tokenMatching (String.startsWith "a")
                    in
                    Expect.all
                        [ \() -> runList startsWithA [ "apple" ] |> Expect.equal [ "apple" ]
                        , \() -> runList startsWithA [ "banana" ] |> Expect.equal []
                        , \() -> List.length (runList startsWithA [ "apple", "banana", "avocado" ]) |> Expect.equal 2
                        ]
                        ()
            , test "map transforms results" <|
                \() ->
                    Expect.all
                        [ \() -> runList (map String.toUpper (word "cat")) [ "cat" ] |> Expect.equal [ "CAT" ]
                        , \() -> runList (map (\n -> n * 2) nat) [ "5" ] |> Expect.equal [ 10 ]
                        ]
                        ()
            , test "map2 combines parsers regardless of order" <|
                \() ->
                    let
                        redBall =
                            map2 Tuple.pair (word "red") (word "ball")
                    in
                    Expect.all
                        [ \() -> runList redBall [ "red", "ball" ] |> Expect.equal [ ( "red", "ball" ) ]
                        , \() -> runList redBall [ "ball", "red" ] |> Expect.equal [ ( "red", "ball" ) ]
                        , \() -> runList redBall [ "the", "red", "ball" ] |> Expect.equal [ ( "red", "ball" ) ]
                        ]
                        ()
            , test "choice picks from alternatives" <|
                \() ->
                    let
                        catOrDog =
                            choice [ word "cat", word "dog" ]
                    in
                    Expect.all
                        [ \() -> runList catOrDog [ "cat" ] |> Expect.equal [ "cat" ]
                        , \() -> runList catOrDog [ "dog" ] |> Expect.equal [ "dog" ]
                        , \() -> runList catOrDog [ "bird" ] |> Expect.equal []
                        ]
                        ()
            , test "minimalToken matches weighted tokens" <|
                \() ->
                    let
                        synonyms =
                            minimalToken [ ( 0.0, "buy" ), ( 0.0, "purchase" ), ( 1.0, "get" ) ]
                    in
                    Expect.all
                        [ \() -> runList synonyms [ "buy" ] |> Expect.equal [ "buy" ]
                        , \() -> runList synonyms [ "purchase" ] |> Expect.equal [ "buy" ]
                        , \() -> runList synonyms [ "get" ] |> Expect.equal [ "buy" ]
                        , \() -> runList synonyms [ "sell" ] |> Expect.equal []
                        , \() -> runTake 1 synonyms [ "buy", "get" ] |> Expect.equal [ "buy" ]
                        ]
                        ()
            , test "repeat collects zero or more matches" <|
                \() ->
                    let
                        cats =
                            repeat (word "cat")
                    in
                    Expect.all
                        [ \() -> List.member [] (runList cats [ "dog" ]) |> Expect.equal True
                        , \() -> List.member [ "cat", "cat" ] (runList cats [ "cat", "cat" ]) |> Expect.equal True
                        ]
                        ()
            ]

        -- ============================================
        -- Property-based test equivalents from Nld.tests.props
        -- ============================================
        , describe "props"
            [ test "pure always returns exactly one result" <|
                \() ->
                    -- Test with various token counts
                    Expect.all
                        [ \() ->
                            let
                                results =
                                    runList (succeed 42) []
                            in
                            Expect.all
                                [ \r -> List.length r |> Expect.equal 1
                                , \r -> r |> Expect.equal [ 42 ]
                                ]
                                results
                        , \() ->
                            let
                                results =
                                    runList (succeed 42) [ "tok", "tok", "tok" ]
                            in
                            Expect.all
                                [ \r -> List.length r |> Expect.equal 1
                                , \r -> r |> Expect.equal [ 42 ]
                                ]
                                results
                        ]
                        ()
            , test "word returns count equal to occurrences" <|
                \() ->
                    -- 0 occurrences
                    Expect.all
                        [ \() ->
                            List.length (runList (word "target") [ "other", "other" ])
                                |> Expect.equal 0
                        , \() ->
                            -- 2 occurrences
                            List.length (runList (word "target") [ "other", "target", "target" ])
                                |> Expect.equal 2
                        , \() ->
                            -- 3 occurrences
                            List.length (runList (word "target") [ "target", "target", "target" ])
                                |> Expect.equal 3
                        ]
                        ()
            , test "map id is identity" <|
                \() ->
                    let
                        w =
                            "test"

                        tokens =
                            [ w, "extra" ]

                        original =
                            runList (word w) tokens

                        mapped =
                            runList (map identity (word w)) tokens
                    in
                    original |> Expect.equal mapped
            , test "map2 with distinct words produces product of counts" <|
                \() ->
                    -- 2 x 2 = 4 combinations
                    let
                        tokens =
                            [ "alpha", "alpha", "beta", "beta" ]

                        results =
                            runList (map2 Tuple.pair (word "alpha") (word "beta")) tokens
                    in
                    List.length results |> Expect.equal 4
            , test "minimalToken returns count equal to sum of occurrences" <|
                \() ->
                    -- 2 alphas + 1 beta = 3 results
                    let
                        tokens =
                            [ "alpha", "alpha", "beta" ]

                        parser =
                            minimalToken [ ( 0.0, "alpha" ), ( 0.0, "beta" ) ]

                        results =
                            runList parser tokens
                    in
                    List.length results |> Expect.equal 3
            , test "choice returns results from each matching alternative" <|
                \() ->
                    let
                        tokens =
                            [ "cat", "dog" ]

                        catOrDog =
                            choice [ word "cat", word "dog" ]

                        results =
                            runList catOrDog tokens
                    in
                    Expect.all
                        [ \r -> List.member "cat" r |> Expect.equal True
                        , \r -> List.member "dog" r |> Expect.equal True
                        ]
                        results
            ]

        -- ============================================
        -- Nondeterminism property tests equivalents
        -- ============================================
        , describe "nondeterminism props"
            [ test "multiple occurrences produce multiple parses" <|
                \() ->
                    -- 3 occurrences should produce 3 results
                    let
                        tokens =
                            [ "word", "word", "word" ]

                        results =
                            runList (word "word") tokens
                    in
                    List.length results |> Expect.equal 3
            , test "results are deterministic" <|
                \() ->
                    let
                        tokens =
                            [ "a", "b", "a", "c" ]

                        parser =
                            map2 Tuple.pair (word "a") (word "b")

                        results1 =
                            runList parser tokens

                        results2 =
                            runList parser tokens
                    in
                    results1 |> Expect.equal results2
            , test "token captures each position exactly once" <|
                \() ->
                    -- 4 tokens should produce 4 results
                    let
                        tokens =
                            [ "x", "x", "x", "x" ]

                        results =
                            runList token tokens
                    in
                    List.length results |> Expect.equal 4
            ]

        -- ============================================
        -- Additional tests from original test file
        -- ============================================
        , describe "words (synonyms)"
            [ test "canonicalizes to first word" <|
                \() ->
                    runList (words [ "delete", "remove", "erase" ]) [ "erase" ]
                        |> Expect.equal [ "delete" ]
            , test "matches any synonym" <|
                \() ->
                    runList (words [ "yes", "yeah", "yep" ]) [ "yeah" ]
                        |> Expect.equal [ "yes" ]
            ]
        , describe "tuple2"
            [ test "combines two parsers" <|
                \() ->
                    runList (tuple2 (word "delete") (word "file")) [ "delete", "file" ]
                        |> Expect.equal [ ( "delete", "file" ) ]
            , test "handles out-of-order tokens" <|
                \() ->
                    runList (tuple2 (word "delete") (word "file")) [ "file", "delete" ]
                        |> Expect.equal [ ( "delete", "file" ) ]
            ]
        , describe "andMap"
            [ test "applies function parser to value parser" <|
                \() ->
                    runList
                        (succeed String.toUpper
                            |> andMap (word "hello")
                        )
                        [ "hello" ]
                        |> Expect.equal [ "HELLO" ]
            , test "chains multiple parsers with pipe syntax" <|
                \() ->
                    runList
                        (succeed (\a b -> a ++ "-" ++ b)
                            |> andMap (word "hello")
                            |> andMap (word "world")
                        )
                        [ "hello", "world" ]
                        |> Expect.equal [ "hello-world" ]
            , test "works with 4+ fields using records" <|
                \() ->
                    let
                        parser =
                            succeed (\a b c d -> { w = a, x = b, y = c, z = d })
                                |> andMap (word "a")
                                |> andMap (word "b")
                                |> andMap (word "c")
                                |> andMap (word "d")
                    in
                    runList parser [ "a", "b", "c", "d" ]
                        |> Expect.equal [ { w = "a", x = "b", y = "c", z = "d" } ]
            , test "handles out-of-order tokens" <|
                \() ->
                    runList
                        (succeed Tuple.pair
                            |> andMap (word "first")
                            |> andMap (word "second")
                        )
                        [ "second", "first" ]
                        |> Expect.equal [ ( "first", "second" ) ]
            , test "works with default  via succeed" <|
                \() ->
                    runList
                        (succeed Tuple.pair
                            |> andMap (word "required")
                            |> andMap (choice [ nat, succeed 0 ])
                        )
                        [ "required" ]
                        |> Expect.equal [ ( "required", 0 ) ]
            ]
        , describe "topK (additional autocomplete tests)"
            [ test "returns next needed token after partial match" <|
                \() ->
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
                    topK 5 (tuple2 (word "a") (word "b")) [ "a" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "b" s)
                        |> Expect.equal (Just True)
            , test "explores deeply nested parsers" <|
                \() ->
                    topK 5 (tuple3 (word "a") (word "b") (word "c")) [ "a", "b" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "c" s)
                        |> Expect.equal (Just True)
            , test "multi-step autocomplete - suggests first token" <|
                \() ->
                    let
                        parser =
                            tuple3
                                (words [ "buy", "sell" ])
                                (words [ "apple", "orange" ])
                                (word "now")
                    in
                    topK 3 parser []
                        |> List.head
                        |> Maybe.map
                            (\s ->
                                Set.member "buy" s && Set.member "sell" s
                            )
                        |> Expect.equal (Just True)
            , test "multi-step autocomplete - suggests second token after first" <|
                \() ->
                    let
                        parser =
                            tuple3
                                (words [ "buy", "sell" ])
                                (words [ "apple", "orange" ])
                                (word "now")
                    in
                    topK 3 parser [ "buy" ]
                        |> List.head
                        |> Maybe.map
                            (\s ->
                                Set.member "apple" s && Set.member "orange" s
                            )
                        |> Expect.equal (Just True)
            , test "multi-step autocomplete - suggests third token after first two" <|
                \() ->
                    let
                        parser =
                            tuple3
                                (words [ "buy", "sell" ])
                                (words [ "apple", "orange" ])
                                (word "now")
                    in
                    topK 3 parser [ "buy", "apple" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "now" s)
                        |> Expect.equal (Just True)
            , test "multi-step autocomplete - complete parse returns empty" <|
                \() ->
                    let
                        parser =
                            tuple3
                                (words [ "buy", "sell" ])
                                (words [ "apple", "orange" ])
                                (word "now")
                    in
                    topK 3 parser [ "buy", "apple", "now" ]
                        |> Expect.equal []
            , test "multi-step autocomplete - works with out-of-order input" <|
                \() ->
                    let
                        parser =
                            tuple3
                                (words [ "buy", "sell" ])
                                (words [ "apple", "orange" ])
                                (word "now")
                    in
                    topK 3 parser [ "apple", "buy" ]
                        |> List.head
                        |> Maybe.map (\s -> Set.member "now" s)
                        |> Expect.equal (Just True)
            ]
        , describe "Peach laziness"
            [ test "flatMap is lazy - doesn't evaluate all branches" <|
                \() ->
                    Peach.peach [ ( 1.0, "a" ), ( 2.0, "b" ) ]
                        |> Peach.flatMap (\x -> Peach.peach [ ( 0.5, x ++ "1" ) ])
                        |> Peach.take 1
                        |> List.map Tuple.second
                        |> Expect.equal [ "a1" ]
            , test "lazy thunks are only evaluated when needed" <|
                \() ->
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

        -- ============================================
        -- Token consumption (regression tests)
        -- ============================================
        , describe "token consumption (regression tests)"
            [ test "map2 does not reuse tokens - numeric edition" <|
                \() ->
                    let
                        daysAgo =
                            map2 (\n _ -> n) nat (word "days")

                        combined =
                            map2 Tuple.pair daysAgo nat

                        results =
                            runList combined [ "2", "days", "5" ]
                    in
                    Expect.all
                        [ \r -> List.member ( 2, 5 ) r |> Expect.equal True
                        , \r -> List.member ( 2, 2 ) r |> Expect.equal False
                        ]
                        results
            , test "choice does not restore consumed tokens" <|
                \() ->
                    let
                        parser =
                            map2 Tuple.pair (choice [ word "a", word "b" ]) nat
                    in
                    runList parser [ "a", "3" ]
                        |> Expect.equal [ ( "a", 3 ) ]
            , test "complex: activity parser token consumption" <|
                \() ->
                    let
                        whenParser =
                            map2 (\n _ -> n) nat (words [ "days", "day" ])

                        minutesParser =
                            nat

                        combined =
                            map2 Tuple.pair whenParser minutesParser
                    in
                    runList combined [ "2", "days", "ago", "45", "minutes" ]
                        |> List.member ( 2, 2 )
                        |> Expect.equal False
            ]
        ]
