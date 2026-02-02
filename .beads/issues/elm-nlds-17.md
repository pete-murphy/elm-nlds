# Follow Elm package naming conventions

- **ID:** elm-nlds-17
- **Type:** task
- **Status:** open
- **Priority:** 2 (medium)
- **Created:** 2026-02-02

## Description

Ensure the package follows Elm community naming conventions:

1. **Package name**: Should use `elm-` prefix unless `elm` already appears
   - Current: `elm-nlds` ✓ (good)

2. **Literal Naming Policy**: Names should be descriptive and literal
   - `Nld` = "Natural Language Disambiguator" - consider if this should be spelled out
   - `Peach` = from "peachy" - consider if a more descriptive name like `PrioritySearch` would be clearer

3. **Module names**: Should match what they expose
   - `Nld` module exposes `Nld` type ✓
   - `Peach` module exposes `Peach` type ✓

## Decision Needed

Consider whether to keep the names from the Unison originals (`Nld`, `Peach`) for familiarity, or use more descriptive Elm-style names.

## References

- https://discourse.elm-lang.org/t/literal-names-policy-i-e-how-to-name-packages/242
