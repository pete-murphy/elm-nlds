# Verify all public types are properly exposed

- **ID:** elm-nlds-11
- **Type:** task
- **Status:** closed
- **Priority:** 2 (medium)
- **Created:** 2026-02-02

## Description

Ensure all types that can be created through the public API are exposed so users can annotate their functions.

Current types to verify:
- `Nld a` - exposed (opaque)
- `Peach a` - exposed (opaque)

Check that users can write type annotations like:
```elm
myParser : Nld String
myParser = word "hello"

mySearch : Peach Int
mySearch = peach [ ( 1.0, 42 ) ]
```

## References

- Idiomatic Elm Package Guide: "Always expose types that can be created from your API"
- https://github.com/mdgriffith/style-elements/pull/95 (example issue)
