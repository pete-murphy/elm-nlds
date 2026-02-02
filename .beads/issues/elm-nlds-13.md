# Add elm-verify-examples for doc comment testing

- **ID:** elm-nlds-13
- **Type:** task
- **Status:** open
- **Priority:** 3 (low)
- **Created:** 2026-02-02

## Description

Set up `elm-verify-examples` to run documentation examples as tests.

This ensures:
- Doc examples stay valid as the API evolves
- Examples are actually correct and compile
- CI catches broken examples

## Setup

1. Install: `npm install --save-dev elm-verify-examples`
2. Create `tests/VerifyExamples.elm`
3. Add to CI pipeline in GitHub Actions

## References

- https://github.com/stoeffel/elm-verify-examples
- https://www.npmjs.com/package/elm-verify-examples
