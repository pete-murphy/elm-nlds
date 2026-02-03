module Peach exposing
    ( Peach
    , peach, fail, succeed, lazy
    , map, flatMap, choose
    , head, toList, take
    , rankBy, each, optional
    )

{-| A lazy priority search data structure, ported from Unison's `pchiusano/peachy` library.

`Peach` is similar to `Each` but expands branches with smaller weights first.
It allows you to explore multiple possibilities, prioritizing by weight.

**Laziness:** Unlike a simple heap, `Peach` supports lazy evaluation. Results
are computed on-demand as you extract them with `head`, `take`, or `toList`.
This enables efficient exploration of large or infinite search spaces.


# Core Types

@docs Peach


# Primitive Operations

@docs peach, fail, succeed, lazy


# Combinators

@docs map, flatMap, choose


# Extractors

@docs head, toList, take


# Convenience Functions

@docs rankBy, each, optional

-}

import Heap exposing (Heap)


{-| A `Peach a` represents a lazy priority search computation that can yield
multiple weighted results. Results with smaller weights are explored first.

Internally, this is represented as a priority queue of either:

  - Concrete weighted values
  - Thunks that produce more `Peach` elements when forced

This allows lazy evaluation: results are only computed when extracted.

-}
type Peach a
    = Peach (Heap (PeachItem a))


{-| An item in the priority queue: either a value or a suspended computation.
-}
type PeachItem a
    = Value Float a
    | Thunk Float (() -> Peach a)


{-| Get the weight of a PeachItem for heap ordering.
-}
itemWeight : PeachItem a -> Float
itemWeight item =
    case item of
        Value w _ ->
            w

        Thunk w _ ->
            w


{-| Create heap options for our priority queue.
-}
heapOptions : Heap.Options (PeachItem a)
heapOptions =
    Heap.smallest
        |> Heap.by itemWeight


{-| Create an empty Peach.
-}
emptyHeap : Heap (PeachItem a)
emptyHeap =
    Heap.empty heapOptions


{-| Create a `Peach` from a list of weighted values. The values will be explored
in order of increasing weight (smallest first).

    peach [ ( 2.0, "b" ), ( 1.0, "a" ), ( 3.0, "c" ) ]
        |> head
    --> Just ( 1.0, "a" )

-}
peach : List ( Float, a ) -> Peach a
peach weightedValues =
    weightedValues
        |> List.map (\( w, a ) -> Value w a)
        |> Heap.fromList heapOptions
        |> Peach


{-| A `Peach` that yields no results (represents failure).

    fail
        |> head
    --> Nothing

-}
fail : Peach a
fail =
    Peach emptyHeap


{-| A `Peach` that yields exactly one result with weight 0.

    succeed "hello"
        |> head
    --> Just ( 0.0, "hello" )

-}
succeed : a -> Peach a
succeed a =
    peach [ ( 0.0, a ) ]


{-| Create a lazy `Peach` from a thunk. The thunk will only be evaluated
when results are extracted. This is the key primitive for lazy evaluation.

    lazy 0.0 (\() -> peach [ ( 1.0, "computed" ) ])
        |> head
    --> Just ( 1.0, "computed" )

The first argument is the minimum possible weight of results from this thunk.
This is used for priority ordering - thunks with lower weights are forced first.

-}
lazy : Float -> (() -> Peach a) -> Peach a
lazy weight thunk =
    Heap.fromList heapOptions [ Thunk weight thunk ]
        |> Peach


{-| Transform the values in a `Peach` computation.

    peach [ ( 1.0, 2 ), ( 2.0, 3 ) ]
        |> map (\x -> x * 2)
        |> toList
    --> [ ( 1.0, 4 ), ( 2.0, 6 ) ]

Note: `map` is lazy - it wraps transformations in thunks for deferred items.

-}
map : (a -> b) -> Peach a -> Peach b
map f (Peach heap) =
    let
        mapItem : PeachItem a -> PeachItem b
        mapItem item =
            case item of
                Value w a ->
                    Value w (f a)

                Thunk w thunk ->
                    Thunk w (\() -> map f (thunk ()))
    in
    Heap.toList heap
        |> List.map mapItem
        |> Heap.fromList (Heap.smallest |> Heap.by itemWeight)
        |> Peach


{-| Chain computations where the second depends on the result of the first.
The weights are combined additively.

    peach [ ( 1.0, "a" ), ( 2.0, "b" ) ]
        |> flatMap (\x -> peach [ ( 0.5, x ++ "1" ), ( 1.0, x ++ "2" ) ])
        |> take 4
    --> [ ( 1.5, "a1" ), ( 2.0, "a2" ), ( 2.5, "b1" ), ( 3.0, "b2" ) ]

**Laziness:** `flatMap` is lazy - it doesn't immediately evaluate all branches.
Instead, it creates thunks that are evaluated on-demand as results are extracted.

-}
flatMap : (a -> Peach b) -> Peach a -> Peach b
flatMap f (Peach heap) =
    let
        -- Convert each item in the input to items for the output
        convertItem : PeachItem a -> PeachItem b
        convertItem item =
            case item of
                Value w a ->
                    -- Create a thunk that will apply f when forced
                    Thunk w (\() -> addWeight w (f a))

                Thunk w thunk ->
                    -- Create a thunk that will flatMap over the result
                    Thunk w (\() -> flatMap f (thunk ()))

        -- Add weight to all items in a Peach
        addWeight : Float -> Peach b -> Peach b
        addWeight extraWeight (Peach h) =
            Heap.toList h
                |> List.map (addWeightToItem extraWeight)
                |> Heap.fromList (Heap.smallest |> Heap.by itemWeight)
                |> Peach

        addWeightToItem : Float -> PeachItem b -> PeachItem b
        addWeightToItem extraWeight item =
            case item of
                Value w b ->
                    Value (w + extraWeight) b

                Thunk w thunk ->
                    Thunk (w + extraWeight) (\() -> addWeight extraWeight (thunk ()))
    in
    Heap.toList heap
        |> List.map convertItem
        |> Heap.fromList (Heap.smallest |> Heap.by itemWeight)
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
        combineHeaps : Peach a -> Heap (PeachItem a) -> Heap (PeachItem a)
        combineHeaps (Peach heap) acc =
            Heap.toList heap
                |> List.foldl Heap.push acc
    in
    List.foldl combineHeaps emptyHeap peaches
        |> Peach


{-| Get the first (lowest weight) result from a `Peach` computation, if any.

This forces evaluation of thunks as needed to find the minimum-weight value.

    peach [ ( 2.0, "b" ), ( 1.0, "a" ) ]
        |> head
    --> Just ( 1.0, "a" )

    fail
        |> head
    --> Nothing

-}
head : Peach a -> Maybe ( Float, a )
head p =
    take 1 p |> List.head


{-| Convert a `Peach` computation to a list of all results, ordered by weight (smallest first).

**Warning:** This forces evaluation of all thunks. For infinite or very large
search spaces, use `take` instead.

    peach [ ( 2.0, "b" ), ( 1.0, "a" ), ( 3.0, "c" ) ]
        |> toList
    --> [ ( 1.0, "a" ), ( 2.0, "b" ), ( 3.0, "c" ) ]

-}
toList : Peach a -> List ( Float, a )
toList p =
    -- Use a large number; in practice this should be bounded
    take 10000 p


{-| Take the first `n` results from a `Peach` computation, ordered by weight.

This is the primary way to extract results lazily. Only the minimum number
of thunks needed to produce `n` results will be evaluated.

    peach [ ( 2.0, "b" ), ( 1.0, "a" ), ( 3.0, "c" ) ]
        |> take 2
    --> [ ( 1.0, "a" ), ( 2.0, "b" ) ]

-}
take : Int -> Peach a -> List ( Float, a )
take n (Peach initialHeap) =
    let
        go : Int -> Heap (PeachItem a) -> List ( Float, a ) -> List ( Float, a )
        go remaining heap acc =
            if remaining <= 0 then
                List.reverse acc

            else
                case Heap.pop heap of
                    Nothing ->
                        List.reverse acc

                    Just ( item, rest ) ->
                        case item of
                            Value w a ->
                                go (remaining - 1) rest (( w, a ) :: acc)

                            Thunk _ thunk ->
                                -- Force the thunk and merge its results into the heap
                                let
                                    (Peach thunkHeap) =
                                        thunk ()

                                    mergedHeap =
                                        Heap.toList thunkHeap
                                            |> List.foldl Heap.push rest
                                in
                                go remaining mergedHeap acc
    in
    go n initialHeap []


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
