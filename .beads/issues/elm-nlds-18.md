# Add `succeed` parser for constant values (equivalent to Unison's `pure`)

- **ID:** elm-nlds-18
- **Type:** enhancement
- **Status:** open
- **Priority:** 2 (medium)
- **Created:** 2026-02-03

## Description

The Nld module is missing a fundamental building block: a parser that always succeeds with a constant value without consuming any tokens. In Unison, this is called `pure`, but in Elm parser convention, this is typically named `succeed`.

## Current Gap

Without `succeed`, we cannot:

- Build up parsers incrementally using pipe operators
- Create parsers that always produce a default value
- Implement the applicative pattern effectively

## Proposed API

```elm
succeed : a -> Nld a
```

This parser should:

- Always succeed with the given value
- Consume no tokens
- Have weight 0 (no cost)

## Use Cases

### 1. Building records with pipe syntax

```elm
type alias Command =
    { action : String
    , target : String
    , count : Int
    }

-- Currently must use nested map2/tuple2:
commandParser : Nld Command
commandParser =
    map3 Command
        (word "delete")
        token
        (word "3")

-- With succeed + andMap, can write:
commandParser : Nld Command
commandParser =
    succeed Command
        |> andMap (word "delete")
        |> andMap token
        |> andMap (word "3")
```

### 2. Providing default values

```elm
optionalCount : Nld Int
optionalCount =
    choice
        [ map String.toInt token
        , succeed 1  -- default to 1 if no number provided
        ]
```

## Comparison with Unison

Unison has `Nld.pure` which serves the same purpose:

```unison
Nld.pure : a -> Nld a
Nld.pure a = Done a (TokenPositions.empty) 0
```

## Implementation Notes

Implementation should be straightforward:

```elm
succeed : a -> Nld a
succeed a =
    Done a (tokenPositionsFromList []) -1
```

Or potentially:

```elm
succeed : a -> Nld a
succeed a =
    More Set.empty (\_ _ -> Peach.peach [ ( 0.0, Done a (tokenPositionsFromList []) -1 ) ])
```

The second form may be more appropriate to ensure consistency with other `More` parsers.

## Acceptance Criteria

- [ ] `succeed` function added to Nld module
- [ ] Exported in module exposing list
- [ ] Documentation with examples
- [ ] Tests verifying it produces the value with weight 0
- [ ] Works correctly in combination with `andMap` (see related issue elm-nlds-19)

## References

- Unison's `Nld.pure` implementation
- Related: elm-nlds-19 (Add `andMap` for pipe-style composition)
