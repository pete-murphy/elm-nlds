module Nld exposing
    ( Nld
    , run, runList, runTake
    , word, words, token, nat, tokenMatching, minimalToken
    , indexedWord, indexedWords, indexedToken, indexedNat, indexedTokenMatching
    , succeed, map, map2, map3, andThen
    , tuple2, tuple3
    , choice, repeat
    , autocomplete, topK
    )

{-| Natural Language Disambiguator - a flexible parser for loosely ordered token sequences.

`Nld` parses sequences of tokens while:

  - Ignoring irrelevant tokens
  - Allowing tokens to appear in any order (preferring specified order)
  - Lazily producing results in priority order

Unlike traditional parser combinators, `Nld` is robust to reordering and
alternate phrasings, making it ideal for parsing natural language input.


# Running Parsers

@docs Nld
@docs run, runList, runTake


# Token Matchers

@docs word, words, token, nat, tokenMatching, minimalToken


# Indexed Token Matchers

These return both the matched value and its position in the input.

@docs indexedWord, indexedWords, indexedToken, indexedNat, indexedTokenMatching


# Transforming and Combining

@docs succeed, map, map2, map3, andThen
@docs tuple2, tuple3


# Alternatives and Repetition

@docs choice, repeat


# Autocompletion

@docs autocomplete, topK

-}

import Dict exposing (Dict)
import Peach exposing (Peach)
import Set exposing (Set)


{-| An `Nld a` is a parser that produces values of type `a` from a sequence of tokens.
It explores multiple parse branches lazily, preferring matches where tokens appear
in the specified order and closer together.
-}
type Nld a
    = Done a TokenPositions Int
    | More (Set String) (TokenPositions -> Int -> Peach (Nld a))


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


{-| Run a parser on a list of tokens and return all results as weighted pairs.
Results are ordered by weight (best matches first).

    runList (word "hello") [ "hello", "world" ]
        |> List.map Tuple.second
    --> [ "hello" ]

    runList (tuple2 (word "delete") (word "file")) [ "file", "delete" ]
        |> List.map Tuple.second
    --> [ ( "delete", "file" ) ]

-}
runList : Nld a -> List String -> List ( Float, a )
runList nld tokens =
    run nld (tokenPositionsFromList tokens)


{-| Run a parser on TokenPositions and return all results.
-}
run : Nld a -> TokenPositions -> List ( Float, a )
run nld tp =
    runHelper nld tp -1
        |> Peach.toList


{-| Run a parser and take only the first n results.

    runTake 1 (word "cat") [ "the", "cat", "sat" ]
    --> [ ( 2, "cat" ) ]

-}
runTake : Int -> Nld a -> List String -> List ( Float, a )
runTake n nld tokens =
    runHelper nld (tokenPositionsFromList tokens) -1
        |> Peach.take n


{-| Internal helper to run an Nld and produce a Peach of results.
-}
runHelper : Nld a -> TokenPositions -> Int -> Peach a
runHelper nld tp lastPos =
    case nld of
        Done a _ _ ->
            Peach.peach [ ( 0, a ) ]

        More _ k ->
            k tp lastPos
                |> Peach.flatMap (\nld2 -> runHelper nld2 tp lastPos)



-- TOKEN MATCHERS


{-| Match a specific word/token.

    runList (word "delete") [ "please", "delete", "this" ]
    --> [ ( 2, "delete" ) ]

-}
word : String -> Nld String
word w =
    map Tuple.first (indexedWord w)


{-| Match any of the given words, preferring earlier ones in the list.
Useful for synonyms.

    runList (words [ "delete", "remove", "erase" ]) [ "please", "erase", "this" ]
        |> List.map Tuple.second
    --> [ "delete" ]

The result is canonicalized to the first word in the list.

-}
words : List String -> Nld String
words wordList =
    case wordList of
        [] ->
            More Set.empty (\_ _ -> Peach.fail)

        first :: _ ->
            let
                allWords =
                    Set.fromList wordList

                k tp lastPos =
                    let
                        -- Find all positions where any of the words appear
                        allPositions =
                            wordList
                                |> List.concatMap
                                    (\w ->
                                        positions w tp
                                            |> Set.toList
                                            |> List.map (\p -> ( w, p ))
                                    )

                        -- Weight by position gap and preference for earlier words in list
                        weightedPositions =
                            allPositions
                                |> List.map
                                    (\( w, p ) ->
                                        let
                                            wordPenalty =
                                                wordList
                                                    |> List.indexedMap Tuple.pair
                                                    |> List.filter (\( _, word_ ) -> word_ == w)
                                                    |> List.head
                                                    |> Maybe.map (\( i, _ ) -> toFloat i * 0.1)
                                                    |> Maybe.withDefault 0
                                        in
                                        ( gapCost lastPos p + wordPenalty, ( w, p ) )
                                    )
                    in
                    if List.isEmpty weightedPositions then
                        Peach.fail

                    else
                        Peach.peach weightedPositions
                            |> Peach.map
                                (\( w, p ) ->
                                    Done first (remove w p tp) p
                                )
            in
            More allWords k


{-| Match any token and return it.

    runTake 1 token [ "anything" ]
        |> List.map Tuple.second
    --> [ "anything" ]

-}
token : Nld String
token =
    map Tuple.first indexedToken


{-| Match a natural number (non-negative integer).

    runList (tuple2 (word "buy") nat) [ "buy", "3", "apples" ]
        |> List.map Tuple.second
    --> [ ( "buy", 3 ) ]

-}
nat : Nld Int
nat =
    map Tuple.first indexedNat


{-| Match tokens satisfying a predicate.

    runList (tokenMatching (String.endsWith ".txt")) [ "open", "report.txt" ]
        |> List.map Tuple.second
    --> [ "report.txt" ]

-}
tokenMatching : (String -> Bool) -> Nld String
tokenMatching pred =
    map Tuple.first (indexedTokenMatching pred)


{-| Match any of several weighted tokens. Lower weights are preferred.

    runTake 1 (minimalToken [ ( 0.0, "delete" ), ( 1.0, "remove" ) ]) [ "remove", "delete" ]
        |> List.map Tuple.second
    --> [ "delete" ]

-}
minimalToken : List ( Float, String ) -> Nld String
minimalToken weightedWords =
    let
        allWords =
            List.map Tuple.second weightedWords |> Set.fromList

        canonical =
            weightedWords
                |> List.sortBy Tuple.first
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault ""

        k tp lastPos =
            let
                allPositions =
                    weightedWords
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
                            Done canonical (remove w p tp) p
                        )
    in
    More allWords k



-- INDEXED TOKEN MATCHERS


{-| Match a specific word and return both the word and its position.

    runList (indexedWord "cat") [ "the", "cat", "sat" ]
    --> [ ( 2, ( "cat", 1 ) ) ]

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
    More (Set.singleton w) k


{-| Match any of the given words and return the matched word with its position.
-}
indexedWords : List String -> Nld ( String, Int )
indexedWords wordList =
    let
        allWords =
            Set.fromList wordList

        k tp lastPos =
            let
                allPositions =
                    wordList
                        |> List.concatMap
                            (\w ->
                                positions w tp
                                    |> Set.toList
                                    |> List.map (\p -> ( w, p ))
                            )

                weightedPositions =
                    allPositions
                        |> List.map
                            (\( w, p ) ->
                                let
                                    wordPenalty =
                                        wordList
                                            |> List.indexedMap Tuple.pair
                                            |> List.filter (\( _, word_ ) -> word_ == w)
                                            |> List.head
                                            |> Maybe.map (\( i, _ ) -> toFloat i * 0.1)
                                            |> Maybe.withDefault 0
                                in
                                ( gapCost lastPos p + wordPenalty, ( w, p ) )
                            )
            in
            if List.isEmpty weightedPositions then
                Peach.fail

            else
                Peach.peach weightedPositions
                    |> Peach.map
                        (\( w, p ) ->
                            Done ( w, p ) (remove w p tp) p
                        )
    in
    More allWords k


{-| Match any token and return it with its position.
-}
indexedToken : Nld ( String, Int )
indexedToken =
    let
        k tp lastPos =
            let
                allPositions =
                    Dict.toList tp.byPosition

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
    More Set.empty k


{-| Match a natural number and return it with its position.
-}
indexedNat : Nld ( Int, Int )
indexedNat =
    let
        k tp lastPos =
            let
                allPositions =
                    Dict.toList tp.byPosition
                        |> List.filterMap
                            (\( p, t ) ->
                                String.toInt t
                                    |> Maybe.andThen
                                        (\n ->
                                            if n >= 0 then
                                                Just ( p, t, n )

                                            else
                                                Nothing
                                        )
                            )

                weightedPositions =
                    allPositions
                        |> List.map (\( p, t, n ) -> ( gapCost lastPos p, ( t, p, n ) ))
            in
            if List.isEmpty weightedPositions then
                Peach.fail

            else
                Peach.peach weightedPositions
                    |> Peach.map
                        (\( t, p, n ) ->
                            Done ( n, p ) (remove t p tp) p
                        )
    in
    More Set.empty k


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
    More Set.empty k



-- COMBINATORS


{-| A parser that always succeeds with the given value without consuming any tokens.

This is the fundamental building block for applicative-style parsing with `andMap`.

    runList (succeed 42) [ "any", "tokens" ]
        |> List.map Tuple.second
    --> [ 42 ]

    -- Use with choice for default values:
    runList (choice [ nat, succeed 1 ]) [ "not-a-number" ]
        |> List.map Tuple.second
    --> [ 1 ]

-}
succeed : a -> Nld a
succeed a =
    -- Use More with empty wanted set so it integrates properly with
    -- the continuation-based parsing. The thunk immediately returns
    -- Done with the value and preserves the current token positions.
    More Set.empty
        (\tp lastPos ->
            Peach.peach [ ( 0.0, Done a tp lastPos ) ]
        )


{-| Transform the result of a parser.

    runList (map String.toUpper (word "hello")) [ "hello" ]
    --> [ ( 1, "HELLO" ) ]

-}
map : (a -> b) -> Nld a -> Nld b
map f nld =
    case nld of
        Done a rem lastPos ->
            Done (f a) rem lastPos

        More wanted k ->
            More wanted (\rem lastPos -> Peach.map (map f) (k rem lastPos))


{-| Combine two parsers.
-}
map2 : (a -> b -> c) -> Nld a -> Nld b -> Nld c
map2 f nldA nldB =
    andThen (\a -> map (\b -> f a b) nldB) nldA


{-| Combine three parsers.
-}
map3 : (a -> b -> c -> d) -> Nld a -> Nld b -> Nld c -> Nld d
map3 f nldA nldB nldC =
    andThen (\a -> map2 (\b c -> f a b c) nldB nldC) nldA


{-| Combine two parsers into a tuple.

    runList (tuple2 (word "delete") (word "file")) [ "delete", "file" ]
        |> List.map Tuple.second
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

                More wanted k ->
                    More wanted k

        More wanted k ->
            More wanted
                (\tp lastPos ->
                    k tp lastPos
                        |> Peach.map (andThen f)
                )


{-| Try multiple parsers and return all successful parses.

    runList (choice [ word "delete", word "add" ]) [ "delete" ]
        |> List.map Tuple.second
    --> [ "delete" ]

-}
choice : List (Nld a) -> Nld a
choice parsers =
    let
        allWanted =
            parsers
                |> List.map getWanted
                |> List.foldl Set.union Set.empty

        k tp lastPos =
            parsers
                |> List.map (\nld -> runHelper nld tp lastPos)
                |> Peach.choose
                |> Peach.map (\a -> Done a tp lastPos)
    in
    More allWanted k


{-| Match zero or more occurrences of a parser.

Results include all possible match lengths, with longer matches preferred.

-}
repeat : Nld a -> Nld (List a)
repeat nld =
    let
        go : List a -> TokenPositions -> Int -> Peach (Nld (List a))
        go acc tp lastPos =
            let
                -- Option 1: Stop here and return what we have
                stopHere =
                    Peach.peach [ ( 0, Done (List.reverse acc) tp lastPos ) ]

                -- Option 2: Try to match one more
                tryMore =
                    case nld of
                        Done a newTp newPos ->
                            go (a :: acc) newTp newPos

                        More _ k ->
                            k tp lastPos
                                |> Peach.flatMap
                                    (\result ->
                                        case result of
                                            Done a newTp newPos ->
                                                go (a :: acc) newTp newPos

                                            More _ _ ->
                                                -- Shouldn't happen in well-formed parsers
                                                Peach.fail
                                    )
            in
            Peach.choose [ stopHere, tryMore ]
    in
    More (getWanted nld) (go [])



-- AUTOCOMPLETE


{-| Get autocomplete suggestions for incomplete input.
Returns a list of sets of tokens that could complete the parse.

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

  - `nld`: The current parser state
  - `tp`: Remaining token positions
  - `lastPos`: Last matched position
  - `accumulated`: Tokens we've been looking for along this path

-}
autocompleteHelper : Nld a -> TokenPositions -> Int -> Set String -> Peach (Set String)
autocompleteHelper nld tp lastPos accumulated =
    case nld of
        Done _ _ _ ->
            -- Parse succeeded, no suggestions needed at this branch
            Peach.fail

        More wanted k ->
            let
                -- Try to continue parsing
                continuation =
                    k tp lastPos

                -- Extract concrete results and explore them
                exploreResults =
                    Peach.lazy 0
                        (\() ->
                            -- Use flatMap to explore each result from the continuation
                            continuation
                                |> Peach.flatMap
                                    (\nextNld ->
                                        case nextNld of
                                            Done _ _ _ ->
                                                -- This branch succeeded, no suggestions
                                                Peach.fail

                                            More nextWanted nextK ->
                                                -- Continue exploring
                                                autocompleteHelper
                                                    (More nextWanted nextK)
                                                    tp
                                                    lastPos
                                                    (Set.union accumulated nextWanted)
                                    )
                        )

                -- Check if the continuation produces any results
                -- If not, we've hit a failure point - emit suggestions
                emitSuggestions =
                    let
                        newAccumulated =
                            Set.union accumulated wanted
                    in
                    if Set.isEmpty newAccumulated then
                        Peach.fail

                    else
                        -- Use lazy to defer checking if continuation is empty
                        Peach.lazy 0
                            (\() ->
                                case Peach.head continuation of
                                    Nothing ->
                                        -- Continuation is empty - this is a failure point
                                        Peach.peach [ ( 0, newAccumulated ) ]

                                    Just _ ->
                                        -- Continuation has results, don't emit here
                                        Peach.fail
                            )
            in
            Peach.choose [ emitSuggestions, exploreResults ]



-- HELPERS


{-| Get the set of wanted tokens from an Nld.
-}
getWanted : Nld a -> Set String
getWanted nld =
    case nld of
        Done _ _ _ ->
            Set.empty

        More wanted _ ->
            wanted


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
