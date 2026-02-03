# Add `andMap` for pipe-style composition of Nld parsers

- **ID:** elm-nlds-19
- **Type:** enhancement
- **Status:** open
- **Priority:** 2 (medium)
- **Created:** 2026-02-03

## Description

The current Nld API only supports fixed-arity composition via `map2`, `map3`, and `tuple2`, `tuple3`. This becomes unwieldy for records with more than 3 fields, and we don't want to add `map4` through `map7` like Unison does. Instead, we should add `andMap` to enable idiomatic Elm pipe-style composition.

## Current Limitation

```elm
-- Currently limited to 3 fields with map3:
type alias Command =
    { action : String
    , target : String
    , count : Int
    }

commandParser =
    map3 Command
        (word "delete")
        token
        nat

-- What if we have 4, 5, or more fields?
-- We'd need map4, map5, etc.
```

## Proposed API

```elm
andMap : Nld a -> Nld (a -> b) -> Nld b
```

This is the standard applicative pattern used across Elm parser libraries.

## Use Cases

### 1. Building records with arbitrary field counts

```elm
type alias Command =
    { action : String
    , target : String
    , count : Int
    , force : Bool
    }

-- Clean, extensible pipe syntax:
commandParser : Nld Command
commandParser =
    succeed Command
        |> andMap (word "delete")
        |> andMap token
        |> andMap nat
        |> andMap (choice [ word "force" |> map (\_ -> True), succeed False ])
```

### 2. Gradual parser construction

```elm
baseParser : Nld (String -> Command)
baseParser =
    succeed Command
        |> andMap (word "delete")

withTarget : Nld (String -> Int -> Bool -> Command)
withTarget =
    baseParser |> andMap token

-- Can extend incrementally without rewriting entire parser
```

### 3. Cleaner than nested map calls

```elm
-- Current approach with map3:
map3 (\a b c -> { action = a, target = b, count = c })
    (word "delete")
    token
    nat

-- With succeed + andMap:
succeed (\a b c -> { action = a, target = b, count = c })
    |> andMap (word "delete")
    |> andMap token
    |> andMap nat
```

## Implementation

The implementation uses the applicative pattern:

```elm
andMap : Nld a -> Nld (a -> b) -> Nld b
andMap nldA nldFnAB =
    map2 (\fn a -> fn a) nldFnAB nldA
```

Or defined in terms of `andThen`:

```elm
andMap : Nld a -> Nld (a -> b) -> Nld b
andMap nldA nldFn =
    nldFn |> andThen (\fn -> map fn nldA)
```

## Comparison with Unison

Unison doesn't have `andMap` because it has `map4` through `map7`. However, the Elm community strongly prefers the applicative pipe pattern. This is one case where we should diverge from Unison to follow Elm idioms.

From `elm/parser`:

```elm
andMap : Parser a -> Parser (a -> b) -> Parser b
```

## Acceptance Criteria

- [ ] `andMap` function added to Nld module
- [ ] Exported in module exposing list
- [ ] Documentation with examples showing pipe syntax
- [ ] Tests verifying correct behavior with multiple fields
- [ ] At least one example showing 4+ field record parsing
- [ ] Consider marking `map2`, `map3`, `tuple2`, `tuple3` as convenience functions in docs, with note that `andMap` is preferred for extensibility

## References

- `elm/parser` implementation: https://package.elm-lang.org/packages/elm/parser/latest/Parser#andMap
- Common pattern in Elm parser libraries
- Related: elm-nlds-18 (Add `succeed` for constant values)
