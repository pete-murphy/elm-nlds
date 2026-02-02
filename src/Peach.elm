module Peach exposing
    ( Peach
    , peach, fail
    , map, flatMap, choose
    , head, toList, take
    , rankBy, each, optional
    )

{-| A priority search data structure, ported from Unison's `pchiusano/peachy` library.

`Peach` is similar to `Each` but expands branches with smaller weights first.
It allows you to explore multiple possibilities, prioritizing by weight.


# Core Types

@docs Peach


# Primitive Operations

@docs peach, fail


# Combinators

@docs map, flatMap, choose


# Extractors

@docs head, toList, take


# Convenience Functions

@docs rankBy, each, optional

-}

import Heap exposing (Heap)


{-| A `Peach a` represents a priority search computation that can yield
multiple weighted results. Results with smaller weights are explored first.

Internally, this is represented as a heap of weighted values.

-}
type Peach a
    = Peach (Heap ( Float, a ))


{-| Create a `Peach` from a list of weighted values. The values will be explored
in order of increasing weight (smallest first).

    peach [ ( 2.0, "b" ), ( 1.0, "a" ), ( 3.0, "c" ) ]
        |> head
    --> Just ( 1.0, "a" )

-}
peach : List ( Float, a ) -> Peach a
peach weightedValues =
    let
        heapOptions =
            Heap.smallest
                |> Heap.by Tuple.first
    in
    weightedValues
        |> Heap.fromList heapOptions
        |> Peach


{-| A `Peach` that yields no results (represents failure).

    fail
        |> head
    --> Nothing

-}
fail : Peach a
fail =
    let
        heapOptions =
            Heap.smallest
                |> Heap.by Tuple.first
    in
    Heap.empty heapOptions
        |> Peach


{-| Transform the values in a `Peach` computation.

    peach [ ( 1.0, 2 ), ( 2.0, 3 ) ]
        |> map (\x -> x * 2)
        |> toList
    --> [ ( 1.0, 4 ), ( 2.0, 6 ) ]

-}
map : (a -> b) -> Peach a -> Peach b
map f (Peach heap) =
    let
        heapOptions =
            Heap.smallest
                |> Heap.by Tuple.first

        mappedHeap =
            Heap.toList heap
                |> List.map (Tuple.mapSecond f)
                |> Heap.fromList heapOptions
    in
    Peach mappedHeap


{-| Chain computations where the second depends on the result of the first.
The weights are combined additively.

    peach [ ( 1.0, "a" ), ( 2.0, "b" ) ]
        |> flatMap (\x -> peach [ ( 0.5, x ++ "1" ), ( 1.0, x ++ "2" ) ])
        |> toList
    --> [ ( 1.5, "a1" ), ( 2.0, "a2" ), ( 2.5, "b1" ), ( 3.0, "b2" ) ]

-}
flatMap : (a -> Peach b) -> Peach a -> Peach b
flatMap f (Peach heap) =
    let
        heapOptions =
            Heap.smallest
                |> Heap.by Tuple.first

        -- For each (weight, value) in the input, apply f to get a Peach b,
        -- then add the original weight to all results
        flattenedItems =
            Heap.toList heap
                |> List.concatMap
                    (\( w1, a ) ->
                        let
                            (Peach innerHeap) =
                                f a
                        in
                        Heap.toList innerHeap
                            |> List.map (\( w2, b ) -> ( w1 + w2, b ))
                    )
    in
    flattenedItems
        |> Heap.fromList heapOptions
        |> Peach


{-| Choose between multiple `Peach` computations, exploring them in priority order.

    choose
        [ peach [ ( 1.0, "a" ) ]
        , peach [ ( 2.0, "b" ) ]
        , peach [ ( 0.5, "c" ) ]
        ]
        |> toList
    --> [ ( 0.5, "c" ), ( 1.0, "a" ), ( 2.0, "b" ) ]

-}
choose : List (Peach a) -> Peach a
choose peaches =
    let
        heapOptions =
            Heap.smallest
                |> Heap.by Tuple.first

        combineHeaps : Peach a -> Heap ( Float, a ) -> Heap ( Float, a )
        combineHeaps (Peach heap) acc =
            Heap.toList heap
                |> List.foldl (\item -> Heap.push item) acc
    in
    List.foldl combineHeaps (Heap.empty heapOptions) peaches
        |> Peach


{-| Get the first (lowest weight) result from a `Peach` computation, if any.

    peach [ ( 2.0, "b" ), ( 1.0, "a" ) ]
        |> head
    --> Just ( 1.0, "a" )

    fail
        |> head
    --> Nothing

-}
head : Peach a -> Maybe ( Float, a )
head (Peach heap) =
    Heap.peek heap


{-| Convert a `Peach` computation to a list of all results, ordered by weight (smallest first).

    peach [ ( 2.0, "b" ), ( 1.0, "a" ), ( 3.0, "c" ) ]
        |> toList
    --> [ ( 1.0, "a" ), ( 2.0, "b" ), ( 3.0, "c" ) ]

-}
toList : Peach a -> List ( Float, a )
toList (Peach heap) =
    Heap.toList heap


{-| Take the first `n` results from a `Peach` computation, ordered by weight.

    peach [ ( 2.0, "b" ), ( 1.0, "a" ), ( 3.0, "c" ) ]
        |> take 2
    --> [ ( 1.0, "a" ), ( 2.0, "b" ) ]

-}
take : Int -> Peach a -> List ( Float, a )
take n (Peach heap) =
    let
        go : Int -> Heap ( Float, a ) -> List ( Float, a ) -> List ( Float, a )
        go remaining h acc =
            if remaining <= 0 || Heap.isEmpty h then
                List.reverse acc

            else
                case Heap.pop h of
                    Nothing ->
                        List.reverse acc

                    Just ( ( w, a ), rest ) ->
                        go (remaining - 1) rest (( w, a ) :: acc)
    in
    go n heap []


{-| Rank values by a function and create a `Peach` from them.

    rankBy (\s -> toFloat (String.length s)) [ "apple", "kiwi", "banana" ]
        |> head
    --> Just ( 4.0, "kiwi" )

-}
rankBy : (a -> Float) -> List a -> Peach a
rankBy f values =
    values
        |> List.map (\v -> ( f v, v ))
        |> peach


{-| Create a `Peach` from a list, treating each element equally (weight 0).

    each [ "a", "b", "c" ]
        |> toList
        |> List.length
    --> 3

-}
each : List a -> Peach a
each values =
    values
        |> List.map (\v -> ( 0.0, v ))
        |> peach


{-| Convert an `Optional` value to a `Peach`.

    optional (Just "hello")
        |> head
    --> Just ( 0.0, "hello" )

    optional Nothing
        |> head
    --> Nothing

-}
optional : Maybe a -> Peach a
optional maybeValue =
    case maybeValue of
        Just a ->
            peach [ ( 0.0, a ) ]

        Nothing ->
            fail
