module Nld exposing
    ( Nld
    , run, runList, runTake
    , word, words, token, nat, int, float, boolean, tokenMatching, minimalToken
    , indexedWord, indexedWords, indexedToken, indexedNat, indexedInt, indexedFloat, indexedBoolean, indexedTokenMatching, indexedTokenOfType, indexedMinimalToken
    , succeed, map, map2, map3, map4, map5, map6, map7, map8, andThen, andMap
    , tuple2, tuple3
    , choice, repeat, combine, phrase
    , autocomplete, topK
    , grammar, wanted
    , tokenize
    )

{-| Natural Language Disambiguator - a flexible parser for loosely ordered token sequences.

`Nld` parses sequences of tokens while:

  - Ignoring irrelevant tokens
  - Allowing tokens to appear in any order (preferring specified order)
  - Lazily producing results in priority order

Unlike traditional parser combinators, `Nld` is robust to reordering and
alternate phrasings, making it useful for parsing natural language input.


# Running Parsers

@docs Nld
@docs run, runList, runTake


# Token Matchers

@docs word, words, token, nat, int, float, boolean, tokenMatching, minimalToken


# Indexed Token Matchers

These return both the matched value and its position in the input.

@docs indexedWord, indexedWords, indexedToken, indexedNat, indexedInt, indexedFloat, indexedBoolean, indexedTokenMatching, indexedTokenOfType, indexedMinimalToken


# Transforming and Combining

@docs succeed, map, map2, map3, map4, map5, map6, map7, map8, andThen, andMap
@docs tuple2, tuple3


# Alternatives, Repetition, and Sequencing

@docs choice, repeat, combine, phrase


# Autocompletion

@docs autocomplete, topK


# Grammar Introspection

@docs grammar, wanted


# Tokenizing

@docs tokenize

-}

import Dict exposing (Dict)
import Nld.Grammar as Grammar exposing (Grammar(..))
import Nld.TokenType exposing (TokenType(..))
import Peach exposing (Peach)
import Set exposing (Set)


{-| An `Nld a` is a parser that produces values of type `a` from a sequence of tokens.
It explores multiple parse branches lazily, preferring matches where tokens appear
in the specified order and closer together.
-}
type Nld a
    = Done a TokenPositions Int
    | More Grammar (Set String) (TokenPositions -> Int -> Peach (Nld a))


{-| Internal representation of token positions in the input.
Maps tokens to their positions, and positions to their tokens.
-}
type alias TokenPositions =
    { toMap : Dict String (Set Int)
    , byPosition : Dict Int String
    }


{-| Create TokenPositions from a list of tokens.
-}
tokenPositionsFromList : List String -> TokenPositions
tokenPositionsFromList tokens =
    let
        indexed =
            List.indexedMap Tuple.pair tokens

        toMap =
            List.foldl
                (\( i, t ) acc ->
                    Dict.update t
                        (\maybeSet ->
                            case maybeSet of
                                Nothing ->
                                    Just (Set.singleton i)

                                Just s ->
                                    Just (Set.insert i s)
                        )
                        acc
                )
                Dict.empty
                indexed

        byPosition =
            List.foldl
                (\( i, t ) acc -> Dict.insert i t acc)
                Dict.empty
                indexed
    in
    { toMap = toMap, byPosition = byPosition }



-- RUNNING PARSERS


{-| Run a parser on a list of tokens and return all results.
Results are ordered by priority (best matches first).

    runList (word "hello") [ "hello", "world" ]
    --> [ "hello" ]

    runList (tuple2 (word "delete") (word "file")) [ "file", "delete" ]
    --> [ ( "delete", "file" ) ]

-}
runList : Nld a -> List String -> List a
runList nld tokens =
    run nld (tokenPositionsFromList tokens)
        |> List.map Tuple.second


{-| Run a parser on TokenPositions and return all results.
-}
run : Nld a -> TokenPositions -> List ( Float, a )
run nld tp =
    runHelper nld tp -1
        |> Peach.toList


{-| Run a parser and take only the first n results.

    runTake 1 (word "cat") [ "the", "cat", "sat" ]
    --> [ "cat" ]

-}
runTake : Int -> Nld a -> List String -> List a
runTake n nld tokens =
    runHelper nld (tokenPositionsFromList tokens) -1
        |> Peach.take n
        |> List.map Tuple.second


{-| Internal helper to run an Nld and produce a Peach of results.
-}
runHelper : Nld a -> TokenPositions -> Int -> Peach a
runHelper nld tp lastPos =
    case nld of
        Done a _ _ ->
            Peach.peach [ ( 0, a ) ]

        More _ _ k ->
            k tp lastPos
                |> Peach.flatMap (\nld2 -> runHelper nld2 tp lastPos)



-- TOKEN MATCHERS


{-| Match a specific word/token.

    runList (word "delete") [ "please", "delete", "this" ]
    --> [ "delete" ]

-}
word : String -> Nld String
word w =
    map Tuple.first (indexedWord w)


{-| Match any of the given words, preferring earlier ones in the list.
Useful for synonyms.

    runList (words [ "delete", "remove", "erase" ]) [ "please", "erase", "this" ]
    --> [ "delete" ]

The result is canonicalized to the first word in the list.

-}
words : List String -> Nld String
words wordList =
    map Tuple.first (indexedWords wordList)


{-| Match any token and return it.

    runTake 1 token [ "anything" ]
    --> [ "anything" ]

-}
token : Nld String
token =
    map Tuple.first indexedToken


{-| Match a natural number (non-negative integer).

    runList (tuple2 (word "buy") nat) [ "buy", "3", "apples" ]
    --> [ ( "buy", 3 ) ]

-}
nat : Nld Int
nat =
    map Tuple.first indexedNat


{-| Match an integer (positive or negative).

    runList (tuple2 (word "offset") int) [ "offset", "-5", "pixels" ]
    --> [ ( "offset", -5 ) ]

-}
int : Nld Int
int =
    map Tuple.first indexedInt


{-| Match a floating-point number.

    runList float [ "3.14" ]
    --> [ 3.14 ]

    runList float [ "set", "rate", "to", "2.5" ]
    --> [ 2.5 ]

-}
float : Nld Float
float =
    map Tuple.first indexedFloat


{-| Match a boolean token. Accepts case-insensitive variations:
"true", "True", "TRUE", "false", "False", "FALSE", etc.

    runList boolean [ "true" ]
    --> [ True ]

    runList boolean [ "set", "verbose", "True" ]
    --> [ True ]

-}
boolean : Nld Bool
boolean =
    map Tuple.first indexedBoolean


{-| Match tokens satisfying a predicate.

    runList (tokenMatching (String.endsWith ".txt")) [ "open", "report.txt" ]
    --> [ "report.txt" ]

-}
tokenMatching : (String -> Bool) -> Nld String
tokenMatching pred =
    map Tuple.first (indexedTokenMatching pred)


{-| Match any of several weighted tokens. Lower weights are preferred.

    runTake 1 (minimalToken [ ( 0.0, "delete" ), ( 1.0, "remove" ) ]) [ "remove", "delete" ]
    --> [ "delete" ]

-}
minimalToken : List ( Float, String ) -> Nld String
minimalToken weightedWords =
    map Tuple.first (indexedMinimalToken weightedWords)



-- INDEXED TOKEN MATCHERS


{-| Match a specific word and return both the word and its position.

    runList (indexedWord "cat") [ "the", "cat", "sat" ]
    --> [ ( "cat", 1 ) ]

-}
indexedWord : String -> Nld ( String, Int )
indexedWord w =
    let
        k remaining prevPos =
            let
                posSet =
                    positions w remaining
            in
            if Set.isEmpty posSet then
                Peach.fail

            else
                let
                    posList =
                        Set.toList posSet

                    weightedPositions =
                        List.map (\p -> ( gapCost prevPos p, p )) posList
                in
                Peach.peach weightedPositions
                    |> Peach.map (\pos_ -> Done ( w, pos_ ) (remove w pos_ remaining) pos_)
    in
    More (Literal w) (Set.singleton w) k


{-| Match any of the given words and return the canonical (first) word with its position.

Note: Like `words`, this canonicalizes the result to the first word in the list.
For example, `indexedWords ["hello", "hi"]` with input `["hi"]` returns `[("hello", 0)]`.

-}
indexedWords : List String -> Nld ( String, Int )
indexedWords wordList =
    case wordList of
        [] ->
            indexedMinimalToken []

        first :: rest ->
            let
                weightedTokens =
                    ( 0.0, first ) :: List.map (\t -> ( 1.0, t )) rest
            in
            map (\( _, pos ) -> ( first, pos )) (indexedMinimalToken weightedTokens)


{-| Match any token and return it with its position.
-}
indexedToken : Nld ( String, Int )
indexedToken =
    indexedTokenMatching (always True)


{-| Match a natural number and return it with its position.
-}
indexedNat : Nld ( Int, Int )
indexedNat =
    indexedTokenOfType NatToken
        (\t ->
            String.toInt t
                |> Maybe.andThen
                    (\n ->
                        if n >= 0 then
                            Just n

                        else
                            Nothing
                    )
        )


{-| Match an integer (positive or negative) and return it with its position.
-}
indexedInt : Nld ( Int, Int )
indexedInt =
    indexedTokenOfType IntToken String.toInt


{-| Match a floating-point number and return it with its position.
-}
indexedFloat : Nld ( Float, Int )
indexedFloat =
    indexedTokenOfType FloatToken String.toFloat


{-| Match a boolean token and return it with its position.
Accepts case-insensitive variations: "true", "True", "TRUE", "false", "False", "FALSE", etc.
-}
indexedBoolean : Nld ( Bool, Int )
indexedBoolean =
    indexedTokenOfType BooleanToken
        (\t ->
            let
                lower =
                    String.toLower t
            in
            if lower == "true" then
                Just True

            else if lower == "false" then
                Just False

            else
                Nothing
        )


{-| Match tokens satisfying a predicate and return with position.
-}
indexedTokenMatching : (String -> Bool) -> Nld ( String, Int )
indexedTokenMatching pred =
    let
        k tp lastPos =
            let
                allPositions =
                    Dict.toList tp.byPosition
                        |> List.filter (\( _, t ) -> pred t)

                weightedPositions =
                    allPositions
                        |> List.map (\( p, t ) -> ( gapCost lastPos p, ( t, p ) ))
            in
            if List.isEmpty weightedPositions then
                Peach.fail

            else
                Peach.peach weightedPositions
                    |> Peach.map
                        (\( t, p ) ->
                            Done ( t, p ) (remove t p tp) p
                        )
    in
    More AnyToken Set.empty k


{-| Match a token of a given `TokenType` using a custom parse function.
The `TokenType` is used in the `Grammar` representation.

This is used to define `indexedNat`, `indexedFloat`, etc.

-}
indexedTokenOfType : TokenType -> (String -> Maybe a) -> Nld ( a, Int )
indexedTokenOfType tt parse =
    let
        k tp lastPos =
            let
                allPositions =
                    Dict.toList tp.byPosition
                        |> List.filterMap
                            (\( p, t ) ->
                                parse t |> Maybe.map (\a -> ( p, t, a ))
                            )

                weightedPositions =
                    allPositions
                        |> List.map (\( p, t, a ) -> ( gapCost lastPos p, ( t, p, a ) ))
            in
            if List.isEmpty weightedPositions then
                Peach.fail

            else
                Peach.peach weightedPositions
                    |> Peach.map
                        (\( t, p, a ) ->
                            Done ( a, p ) (remove t p tp) p
                        )
    in
    More (Token tt) Set.empty k


{-| Match any of several weighted tokens, returning both the token and its position.
Lower weights are preferred.
-}
indexedMinimalToken : List ( Float, String ) -> Nld ( String, Int )
indexedMinimalToken weightedTokens =
    let
        allWords =
            List.map Tuple.second weightedTokens |> Set.fromList

        k tp lastPos =
            let
                allPositions =
                    weightedTokens
                        |> List.concatMap
                            (\( baseWeight, w ) ->
                                positions w tp
                                    |> Set.toList
                                    |> List.map (\p -> ( baseWeight, w, p ))
                            )

                weighted =
                    allPositions
                        |> List.map
                            (\( baseWeight, w, p ) ->
                                ( gapCost lastPos p + baseWeight, ( w, p ) )
                            )
            in
            if List.isEmpty weighted then
                Peach.fail

            else
                Peach.peach weighted
                    |> Peach.map
                        (\( w, p ) ->
                            Done ( w, p ) (remove w p tp) p
                        )
    in
    More (MinimalTokenGrammar weightedTokens) allWords k



-- COMBINATORS


{-| A parser that always succeeds with the given value without consuming any tokens.

This is the fundamental building block for applicative-style parsing with `andMap`.

    runList (succeed 42) [ "any", "tokens" ]
    --> [ 42 ]

-}
succeed : a -> Nld a
succeed a =
    More (Seq [])
        Set.empty
        (\tp lastPos ->
            Peach.peach [ ( 0.0, Done a tp lastPos ) ]
        )


{-| Transform the result of a parser.

    runList (map String.toUpper (word "hello")) [ "hello" ]
    --> [ "HELLO" ]

-}
map : (a -> b) -> Nld a -> Nld b
map f nld =
    case nld of
        Done a rem lastPos ->
            Done (f a) rem lastPos

        More g w k ->
            More g w (\rem lastPos -> Peach.map (map f) (k rem lastPos))


{-| Combine two parsers.
-}
map2 : (a -> b -> c) -> Nld a -> Nld b -> Nld c
map2 f nldA nldB =
    case nldA of
        Done a remA lastPosA ->
            case nldB of
                Done b remB lastPosB ->
                    Done (f a b) remB (max lastPosA lastPosB)

                More gb wantedB k ->
                    More gb
                        wantedB
                        (\_ _ ->
                            Peach.map (\nldB2 -> map2 f (Done a remA lastPosA) nldB2) (k remA lastPosA)
                        )

        More ga wantedA k ->
            More (Grammar.seq [ ga, grammar nldB ])
                wantedA
                (\remaining lastPos ->
                    Peach.map (\nldA2 -> map2 f nldA2 nldB) (k remaining lastPos)
                )


{-| Combine three parsers.
-}
map3 : (a -> b -> c -> d) -> Nld a -> Nld b -> Nld c -> Nld d
map3 f nldA nldB nldC =
    map2 (\ab c -> ab c) (map2 f nldA nldB) nldC


{-| Combine four parsers.
-}
map4 : (a -> b -> c -> d -> e) -> Nld a -> Nld b -> Nld c -> Nld d -> Nld e
map4 f nldA nldB nldC nldD =
    map2 (\abc d -> abc d) (map3 f nldA nldB nldC) nldD


{-| Combine five parsers.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Nld a -> Nld b -> Nld c -> Nld d -> Nld e -> Nld f
map5 fn nldA nldB nldC nldD nldE =
    map2 (\abcd e -> abcd e) (map4 fn nldA nldB nldC nldD) nldE


{-| Combine six parsers.
-}
map6 : (a -> b -> c -> d -> e -> f -> g) -> Nld a -> Nld b -> Nld c -> Nld d -> Nld e -> Nld f -> Nld g
map6 fn nldA nldB nldC nldD nldE nldF =
    map2 (\abcde f_ -> abcde f_) (map5 fn nldA nldB nldC nldD nldE) nldF


{-| Combine seven parsers.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Nld a -> Nld b -> Nld c -> Nld d -> Nld e -> Nld f -> Nld g -> Nld h
map7 fn nldA nldB nldC nldD nldE nldF nldG =
    map2 (\abcdef g_ -> abcdef g_) (map6 fn nldA nldB nldC nldD nldE nldF) nldG


{-| Combine eight parsers.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Nld a -> Nld b -> Nld c -> Nld d -> Nld e -> Nld f -> Nld g -> Nld h -> Nld i
map8 fn nldA nldB nldC nldD nldE nldF nldG nldH =
    map2 (\abcdefg h_ -> abcdefg h_) (map7 fn nldA nldB nldC nldD nldE nldF nldG) nldH


{-| Combine two parsers into a tuple.

    runList (tuple2 (word "delete") (word "file")) [ "delete", "file" ]
    --> [ ( "delete", "file" ) ]

-}
tuple2 : Nld a -> Nld b -> Nld ( a, b )
tuple2 =
    map2 Tuple.pair


{-| Combine three parsers into a tuple.
-}
tuple3 : Nld a -> Nld b -> Nld c -> Nld ( a, b, c )
tuple3 =
    map3 (\a b c -> ( a, b, c ))


{-| Sequence two parsers. The second parser can depend on the result of the first.
-}
andThen : (a -> Nld b) -> Nld a -> Nld b
andThen f nld =
    case nld of
        Done a tp lastPos ->
            case f a of
                Done b _ _ ->
                    Done b tp lastPos

                More g2 w2 k ->
                    More g2 w2 k

        More g w k ->
            More g
                w
                (\tp lastPos ->
                    k tp lastPos
                        |> Peach.map (andThen f)
                )


{-| Apply a parser producing a function to a parser producing a value.

This enables pipe-style composition for building parsers with arbitrary arity:

    runList
        (succeed Tuple.pair
            |> andMap (word "buy")
            |> andMap nat
        )
        [ "buy", "3" ]
    --> [ ( "buy", 3 ) ]

-}
andMap : Nld a -> Nld (a -> b) -> Nld b
andMap nldA nldFn =
    map2 (\fn a -> fn a) nldFn nldA


{-| Try multiple parsers and return all successful parses.
-}
choice : List (Nld a) -> Nld a
choice parsers =
    case parsers of
        [] ->
            More (Seq []) Set.empty (\_ _ -> Peach.fail)

        [ single ] ->
            single

        _ ->
            let
                allWanted =
                    parsers
                        |> List.map wanted
                        |> List.foldl Set.union Set.empty

                g =
                    Grammar.choice (List.map grammar parsers)

                k tp lastPos =
                    Peach.peach (List.map (\n -> ( 0.0, n )) parsers)
                        |> Peach.flatMap
                            (\nld ->
                                case nld of
                                    Done a rem lp ->
                                        Peach.peach [ ( 0.0, Done a rem lp ) ]

                                    More _ _ k2 ->
                                        k2 tp lastPos
                            )
            in
            More g allWanted k


{-| Match zero or more occurrences of a parser.

Returns all prefixes of the greedy (in-order) match:

  - For `repeat nat` with `["1", "2", "3"]`, returns:
    `[[1, 2, 3], [1, 2], [1], []]`

The greedy collection always takes elements in position order.

-}
repeat : Nld a -> Nld (List a)
repeat nld =
    let
        -- Step a parser to completion, returning (value, remaining tokens, final position)
        step : Nld a -> TokenPositions -> Int -> Maybe ( a, TokenPositions, Int )
        step parser tp lastPos =
            case parser of
                Done a rem pos ->
                    Just ( a, rem, pos )

                More _ _ cont ->
                    -- Get the first result that's at or after lastPos
                    cont tp lastPos
                        |> Peach.toList
                        |> List.filterMap
                            (\( _, result ) ->
                                case result of
                                    Done a rem pos ->
                                        if pos >= lastPos then
                                            Just ( a, rem, pos )

                                        else
                                            Nothing

                                    More _ _ _ ->
                                        Nothing
                            )
                        |> List.head

        -- Greedily collect all matches in position order
        collectGreedy : List a -> TokenPositions -> Int -> ( List a, TokenPositions, Int )
        collectGreedy acc tp lastPos =
            case step nld tp lastPos of
                Nothing ->
                    ( List.reverse acc, tp, lastPos )

                Just ( a, newTp, newPos ) ->
                    collectGreedy (a :: acc) newTp newPos

        -- The continuation that runs when we parse
        k tp lastPos =
            let
                ( fullList, finalTp, finalPos ) =
                    collectGreedy [] tp lastPos

                n =
                    List.length fullList

                -- Generate all prefixes from n elements down to 0
                -- Weight by index so shorter prefixes have higher weight (less preferred)
                prefixWeights =
                    List.range 0 n
                        |> List.map (\i -> ( toFloat i, n - i ))
            in
            Peach.peach prefixWeights
                |> Peach.map
                    (\len ->
                        Done (List.take len fullList) finalTp finalPos
                    )
    in
    More (RepeatGrammar (grammar nld)) (wanted nld) k


{-| Run each parser in the list and collect the results into a list.

    runList (combine [ word "a", word "b" ]) [ "a", "b" ]
    --> [ [ "a", "b" ] ]

Individual elements can use `words` for synonyms:

    runList (combine [ words [ "remove", "delete" ], words [ "file", "document" ] ]) [ "please", "erase", "the", "document" ]
    --> []

-}
combine : List (Nld a) -> Nld (List a)
combine nlds =
    List.foldr (map2 (::)) (succeed []) nlds


{-| Match all of the words in the list (using `word` for each) and return them
joined by spaces.

    runList (phrase [ "delete", "file" ]) [ "please", "delete", "the", "file" ]
    --> [ "delete file" ]

    runList (tuple2 (phrase [ "set", "volume" ]) nat) [ "set", "the", "volume", "to", "11" ]
    --> [ ( "set volume", 11 ) ]

-}
phrase : List String -> Nld String
phrase ws =
    map (String.join " ") (combine (List.map word ws))



-- AUTOCOMPLETE


{-| Get autocomplete suggestions for incomplete input.
Returns a list of sets of tokens that could complete the parse.

    import Set

    topK 5 (word "buy") []
    -- Returns [ Set.fromList [ "buy" ] ]

    topK 5 (tuple2 (word "buy") (word "apples")) [ "buy" ]
    -- Returns [ Set.fromList [ "apples" ] ]

This explores the parse tree lazily, finding all points where the parser
needs tokens that aren't present in the input.

-}
topK : Int -> Nld a -> List String -> List (Set String)
topK n nld tokens =
    autocomplete nld (tokenPositionsFromList tokens)
        |> Peach.take n
        |> List.map Tuple.second


{-| Get autocomplete suggestions as a Peach.

This walks the parse tree, collecting suggestions at every point where
the parser fails due to missing tokens. The result is a lazy stream
of suggestion sets, ordered by parse weight.

-}
autocomplete : Nld a -> TokenPositions -> Peach (Set String)
autocomplete nld tp =
    autocompleteHelper nld tp 0 Set.empty


{-| Internal helper for autocomplete that tracks current state.
-}
autocompleteHelper : Nld a -> TokenPositions -> Int -> Set String -> Peach (Set String)
autocompleteHelper nld tp lastPos accumulated =
    case nld of
        Done _ _ _ ->
            -- Parse succeeded, no suggestions needed at this branch
            Peach.fail

        More _ w k ->
            let
                -- Try to continue parsing
                continuation =
                    k tp lastPos

                -- Extract concrete results and explore them
                exploreResults =
                    Peach.lazy 0
                        (\() ->
                            continuation
                                |> Peach.flatMap
                                    (\nextNld ->
                                        case nextNld of
                                            Done _ _ _ ->
                                                Peach.fail

                                            More _ nextWanted _ ->
                                                autocompleteHelper
                                                    nextNld
                                                    tp
                                                    lastPos
                                                    (Set.union accumulated nextWanted)
                                    )
                        )

                emitSuggestions =
                    let
                        newAccumulated =
                            Set.union accumulated w
                    in
                    if Set.isEmpty newAccumulated then
                        Peach.fail

                    else
                        Peach.lazy 0
                            (\() ->
                                case Peach.head continuation of
                                    Nothing ->
                                        Peach.peach [ ( 0, newAccumulated ) ]

                                    Just _ ->
                                        Peach.fail
                            )
            in
            Peach.choose [ emitSuggestions, exploreResults ]



-- GRAMMAR INTROSPECTION


{-| Extract the `Grammar` from an `Nld` parser. The `Grammar` is a first-class
description of what the parser expects, useful for introspection and serialization.

    import Nld.Grammar exposing (Grammar(..))
    import Nld.TokenType exposing (TokenType(..))

    grammar (word "hello")
    --> Literal "hello"

    grammar (tuple2 (word "buy") nat)
    --> Seq [ Literal "buy", Token NatToken ]

-}
grammar : Nld a -> Grammar
grammar nld =
    case nld of
        Done _ _ _ ->
            Seq []

        More g _ _ ->
            g


{-| Returns the set of tokens that the parser is looking for.
This is used internally for optimization but can be useful for debugging.

    import Set

    wanted (word "hello")
    --> Set.fromList [ "hello" ]

-}
wanted : Nld a -> Set String
wanted nld =
    case nld of
        Done _ _ _ ->
            Set.empty

        More _ w _ ->
            w



-- TOKENIZE


{-| Split a text string into tokens suitable for use with `runList`, `runTake`,
and other `Nld` runners. It handles:

  - Whitespace: splits on whitespace, discarding it.
  - camelCase: splits at lowercase-to-uppercase boundaries.
  - Uppercase runs: keeps runs of uppercase letters together, splitting before the
    final uppercase letter when followed by lowercase (e.g. "HTML" and "Parser").
  - Alpha/numeric boundaries: splits between letters and digits.
  - Punctuation: each non-alphanumeric, non-whitespace character becomes its own token.

Examples:

    tokenize "hello world"
    --> [ "hello", "world" ]

    tokenize "camelCase"
    --> [ "camel", "Case" ]

    tokenize "HTMLParser"
    --> [ "HTML", "Parser" ]

    tokenize "abcra123"
    --> [ "abcra", "123" ]

    tokenize "(hi!)"
    --> [ "(", "hi", "!", ")" ]

    tokenize "setHTMLParser"
    --> [ "set", "HTML", "Parser" ]

-}
tokenize : String -> List String
tokenize input =
    let
        chars =
            String.toList input

        categorize c =
            if Char.isUpper c then
                Upper

            else if Char.isLower c then
                Lower

            else if Char.isDigit c then
                Digit

            else if c == ' ' || c == '\t' || c == '\n' || c == '\u{000D}' then
                Whitespace

            else
                Punct

        go : List Char -> List Char -> List String -> List String
        go remaining current acc =
            case remaining of
                [] ->
                    let
                        final =
                            if List.isEmpty current then
                                acc

                            else
                                acc ++ [ String.fromList (List.reverse current) ]
                    in
                    final

                c :: rest ->
                    let
                        cat =
                            categorize c
                    in
                    case cat of
                        Whitespace ->
                            if List.isEmpty current then
                                go rest [] acc

                            else
                                go rest [] (acc ++ [ String.fromList (List.reverse current) ])

                        Punct ->
                            if List.isEmpty current then
                                go rest [] (acc ++ [ String.fromChar c ])

                            else
                                go rest [] (acc ++ [ String.fromList (List.reverse current), String.fromChar c ])

                        Upper ->
                            case current of
                                [] ->
                                    go rest [ c ] acc

                                prev :: _ ->
                                    if categorize prev == Lower then
                                        -- camelCase boundary
                                        go rest [ c ] (acc ++ [ String.fromList (List.reverse current) ])

                                    else if categorize prev == Upper then
                                        -- Check if this uppercase letter starts a new word
                                        -- (uppercase followed by lowercase = new camelCase word)
                                        case rest of
                                            next :: _ ->
                                                if categorize next == Lower && not (List.isEmpty current) then
                                                    -- Split: keep current as uppercase run, start new word with c
                                                    go rest [ c ] (acc ++ [ String.fromList (List.reverse current) ])

                                                else
                                                    go rest (c :: current) acc

                                            [] ->
                                                go rest (c :: current) acc

                                    else if categorize prev == Digit then
                                        -- digit-to-letter boundary
                                        go rest [ c ] (acc ++ [ String.fromList (List.reverse current) ])

                                    else
                                        go rest (c :: current) acc

                        Lower ->
                            case current of
                                [] ->
                                    go rest [ c ] acc

                                prev :: _ ->
                                    if categorize prev == Digit then
                                        go rest [ c ] (acc ++ [ String.fromList (List.reverse current) ])

                                    else
                                        go rest (c :: current) acc

                        Digit ->
                            case current of
                                [] ->
                                    go rest [ c ] acc

                                prev :: _ ->
                                    if categorize prev == Digit then
                                        go rest (c :: current) acc

                                    else
                                        go rest [ c ] (acc ++ [ String.fromList (List.reverse current) ])
    in
    go chars [] []


type CharCategory
    = Upper
    | Lower
    | Digit
    | Whitespace
    | Punct



-- HELPERS


{-| Look up positions of a token in TokenPositions.
-}
positions : String -> TokenPositions -> Set Int
positions t tp =
    Dict.get t tp.toMap |> Maybe.withDefault Set.empty


{-| Calculate the cost of a gap between positions.
Tokens appearing in order have lower cost than out-of-order tokens.
-}
gapCost : Int -> Int -> Float
gapCost prevPos currentPos =
    if currentPos >= prevPos then
        toFloat (currentPos - prevPos)

    else
        -- Penalty for out-of-order: 1.5x the distance plus 1
        1.5 * toFloat (prevPos - currentPos + 1)


{-| Remove a token at a specific position from TokenPositions.
-}
remove : String -> Int -> TokenPositions -> TokenPositions
remove t pos tp =
    let
        delPos =
            Maybe.map (Set.remove pos)

        newTextToPos =
            Dict.update t delPos tp.toMap

        newPosToText =
            Dict.remove pos tp.byPosition
    in
    { toMap = newTextToPos, byPosition = newPosToText }
