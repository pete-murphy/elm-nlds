# Add comprehensive doc comments to all public API

- **ID:** elm-nlds-10
- **Type:** task
- **Status:** closed
- **Priority:** 2 (medium)
- **Created:** 2026-02-02

## Description

Review and enhance doc comments for all exposed functions and types in:
- `Nld.elm`
- `Peach.elm`

Each doc comment should:
1. Explain what the function does
2. Include at least one code example that can be verified with `elm-verify-examples`
3. Use consistent domain language

Consider using `elm-verify-examples` to ensure doc examples are valid and stay up to date.

## References

- https://github.com/stoeffel/elm-verify-examples
- http://package.elm-lang.org/help/documentation-format
