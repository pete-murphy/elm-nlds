# Lack of laziness in Peach affects autocomplete depth and performance

- **ID:** elm-nlds-20
- **Type:** bug
- **Status:** open
- **Priority:** 1 (high)
- **Created:** 2026-02-03

## Description

The current `Peach` implementation in Elm is strict—it fully materializes all results. This causes two major problems:

1. **Autocomplete only returns immediate suggestions** — it cannot explore deeper branches or handle Peach choices
2. **Potential performance issues** — for large search spaces, we materialize the entire heap unnecessarily

## Comparison with Unison

### Unison's Approach (Lazy)

Unison implements `Peach` as an **ability** (effect) with lazy streaming:

```unison
ability Peach where
  lazily : '{Stream a} () ->{Peach} a
  peach : [(Float, a)] ->{Peach} a

-- toStream converts Peach computation to a lazy Stream
Peach.toStream : '{g, Peach} a -> '{g, Stream (Float, a)} ()
```

The key insight: Unison's `autocomplete` walks the parse tree lazily, handling `peach` operations via effect handlers, and can explore arbitrarily deep branches.

### Elm's Approach (Strict)

Elm's `Peach` is a simple wrapper around `Heap`:

```elm
type Peach a = Peach (Heap ( Float, a ))

-- flatMap fully materializes:
flatMap : (a -> Peach b) -> Peach a -> Peach b
flatMap f (Peach heap) =
    let
        flattenedItems =
            Heap.toList heap
                |> List.concatMap
                    (\( w1, a ) ->
                        let
                            (Peach innerHeap) = f a
                        in
                        Heap.toList innerHeap
                            |> List.map (\( w2, b ) -> ( w1 + w2, b ))
                    )
    in
    flattenedItems
        |> Heap.fromList heapOptions
        |> Peach
```

**Every `flatMap` fully evaluates the heap!**

## The Autocomplete Problem

### Current Elm Implementation (Limited)

```elm
autocomplete : Nld a -> TokenPositions -> Peach (Set String)
autocomplete nld _ =
    case nld of
        Done _ _ _ ->
            Peach.fail  -- Parse succeeded, no suggestions

        More wanted _ ->
            if Set.isEmpty wanted then
                Peach.fail
            else
                Peach.peach [ ( 0, wanted ) ]
```

**Problems:**

- Only returns suggestions for the **current** parser state
- Cannot explore what tokens are needed after a choice
- Cannot handle when `wanted` is empty (e.g., `token` matcher wants "any token")
- No weight information for ordering suggestions

### Unison Implementation (Complete)

Unison's `autocomplete` walks the entire search tree:

```unison
Nld.autocomplete : Nld a -> [Text] -> '{Stream (Float, [Text])} ()
autocomplete nld tokens =
  initialPositions = TokenPositions.fromList tokens
  go : Map Float [('{Peach} (Nld a, TokenPositions, Nat), [Text])] ->{Stream (Float, [Text])} ()
  go pq = match Map.breakOffMin pq with
    None -> ()
    Some ((_, []), pq) -> go pq
    Some ((curWeight, (thunk, wanted) +: rest), pq0) ->
      pq' = Map.insert curWeight rest pq0
      handle thunk()
      with cases
        { (nld, positions, lastPos) } ->
          match nld with
            Done _ _ _ -> go pq'
            More newWanted k ->
              go (Multimap.insert curWeight
                  (do (k positions lastPos, positions, lastPos), Set.toList newWanted) pq')
        { peach [] -> _ } ->
          emit (curWeight, wanted)  -- Failure point = suggestion!
          go pq'
        { peach weights -> resume } ->
          addChoice pq = cases (w, x) -> Multimap.insert (curTotal + w) (do resume x, wanted) pq
          go (foldLeft addChoice pq' weights)
```

This can:

- Explore past `More` nodes
- Handle `peach` choices
- Return weighted suggestions
- Continue after partial matches

## Concrete Examples

### Example 1: Nested suggestions

```elm
-- Parser: "buy" + "apples" + optional count
parser =
    tuple3 (\_ _ n -> n)
        (word "buy")
        (word "apples")
        (choice [ nat, succeed 1 ])

-- Input: ["buy"]
-- Current autocomplete: [Set.fromList []]  -- Empty! Doesn't know we need "apples"
-- Unison would return: [{"apples"}] with weight
```

### Example 2: Choices

```elm
-- Parser: choice between "delete" + file or "create" + file
parser =
    choice
        [ tuple2 (\_ f -> Action.Delete f) (word "delete") token
        , tuple2 (\_ f -> Action.Create f) (word "create") token
        ]

-- Input: []
-- Current autocomplete: [Set.fromList ["delete", "create"]]
-- After user types "del":
-- Current: []  -- Empty set, can't continue!
-- Unison: [{"file.txt", "report.txt", ...}] with weights
```

## Potential Solutions

### Option 1: Implement Lazy Streams in Elm

Create a lazy stream type and convert Peach to use it:

```elm
type Lazy a = Lazy (() -> a)

type Stream a = Nil | Cons a (Lazy (Stream a))

-- Peach becomes a function that produces a Stream
Peach.toStream : Peach a -> Stream (Float, a)
```

**Pros:** Full Unison parity, can handle infinite/large search spaces
**Cons:** Complex to implement, may have performance overhead in Elm's strict evaluation model

### Option 2: Use Continuation-Passing Style

Instead of materializing heaps, use CPS:

```elm
type Peach a = Peach ((Float -> a -> Peach b -> Peach b) -> Peach b -> Peach b)
```

**Pros:** Lazy evaluation of choices
**Cons:** Complex API, hard to debug

### Option 3: Limit Scope, Document Limitations

Accept that Elm's autocomplete is simpler and document:

- Only returns immediate `wanted` tokens
- Does not explore past choice points
- Does not handle "any token" cases

**Pros:** Simple, works for many use cases
**Cons:** Not feature-parity with Unison, surprising limitations

### Option 4: Restricted Lazy Evaluation

Use `() -> Peach a` thunks only at API boundaries:

```elm
type LazyPeach a = LazyPeach (() -> Peach a)

-- Autocomplete returns lazy computation
autocomplete : Nld a -> List String -> LazyPeach (Set String)
```

**Pros:** Minimal changes to core Peach
**Cons:** Two separate APIs, potential confusion

## Recommended Approach

**Start with Option 3 (document limitations) immediately**, then evaluate Option 1 for a future major version.

The strict heap approach works well for the core use case (parsing with weighted results), but autocomplete is indeed limited. We should:

1. Document the current autocomplete limitations
2. Provide workarounds (e.g., manual state tracking)
3. Consider a lazy stream implementation if the limitation becomes blocking

## Acceptance Criteria

- [ ] Document current autocomplete limitations in module docs
- [ ] Add notes about when autocomplete returns empty results
- [ ] Create example showing the limitation and workaround
- [ ] Decide whether to implement lazy Peach or accept the limitation
- [ ] If implementing lazy Peach: spike implementation with benchmarks

## Related Issues

- Blocks advanced autocomplete features
- Related to API design in elm-nlds-18 and elm-nlds-19 (if we add `succeed`, it works in the parser but autocomplete won't suggest it)

## References

- Unison's lazy Peach implementation
- `lazy` package in Elm: https://package.elm-lang.org/packages/elm/core/latest/Lazy
- Current Peach.flatMap strictness is the core issue
