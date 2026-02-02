# Create examples/ folder with meaningful examples

- **ID:** elm-nlds-5
- **Type:** task
- **Status:** closed
- **Priority:** 2 (medium)
- **Created:** 2026-02-02

## Description

Create an `examples/` folder with concrete, meaningful use cases demonstrating the library.

Suggested examples:
1. **Simple command parser** - Parse commands like "delete file.txt" with synonyms
2. **Natural language query** - Parse queries like "show me users from last week"
3. **Autocomplete demo** - Show how `topK` provides suggestions

Each example should:
- Be compilable with `elm make`
- Demonstrate a real use case (not toy examples)
- Show the API's strengths (order independence, synonym handling, prioritized results)

This is "examples-driven development" - validates the API design through concrete use cases.

## References

- Idiomatic Elm Package Guide: "Practice examples-Driven Development"
