# elm-nlds

[![Build Status](https://github.com/pete-murphy/elm-nlds/workflows/CI/badge.svg)](https://github.com/pete-murphy/elm-nlds/actions?query=branch%3Amain)

**Natural language parser for loosely ordered token sequences.**

Parse user input where tokens can appear in any order, with synonyms, and get results ranked by how well they match. Perfect for command palettes, search boxes, and natural language interfaces.

```elm
import Nld exposing (Nld, word, words, tuple2, runTake)

-- Parse "delete file" even if user types "file delete"
deleteCommand : Nld ( String, String )
deleteCommand =
    tuple2
        (words [ "delete", "remove", "rm" ])  -- synonyms
        (word "file")

runTake 1 deleteCommand [ "file", "delete" ]
    |> List.map Tuple.second
--> [ ( "delete", "file" ) ]

runTake 1 deleteCommand [ "please", "remove", "the", "file" ]
    |> List.map Tuple.second
--> [ ( "delete", "file" ) ]  -- ignores irrelevant tokens, canonicalizes synonyms
```

## Design Goals

1. **Order-independent parsing** - Tokens can appear in any order. The parser prefers the specified order but handles reordering gracefully.

2. **Noise-tolerant** - Irrelevant tokens are ignored. Users can type "please delete the file" and it still works.

3. **Synonym support** - Define synonyms that canonicalize to a single value. "delete", "remove", and "rm" all become "delete".

4. **Ranked results** - Results are produced in priority order. Better matches (correct order, fewer gaps) come first.

5. **Autocomplete-ready** - Get suggestions for what tokens would complete a partial parse.

### When to use elm-nlds

- Command palettes with flexible input
- Search interfaces that tolerate word reordering
- Natural language command parsing
- Fuzzy matching where order doesn't matter

### When NOT to use elm-nlds

- Strict grammar parsing (use [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest/))
- Performance-critical parsing of large inputs
- Parsing structured data formats (JSON, etc.)

## Installation

```bash
elm install pete-murphy/elm-nlds
```

## Usage

### Basic Token Matching

```elm
import Nld exposing (word, words, token, nat, runList)

-- Match a specific word
runList (word "hello") [ "hello", "world" ]
    |> List.map Tuple.second
--> [ "hello" ]

-- Match any of several synonyms (canonicalizes to first)
runList (words [ "yes", "yeah", "yep" ]) [ "yeah" ]
    |> List.map Tuple.second
--> [ "yes" ]

-- Match any token
runList token [ "anything" ]
    |> List.map Tuple.second
--> [ "anything" ]

-- Match a natural number
runList nat [ "42" ]
    |> List.map Tuple.second
--> [ 42 ]
```

### Combining Parsers

```elm
import Nld exposing (tuple2, tuple3, map, map2, choice, repeat)

-- Combine two parsers into a tuple
tuple2 (word "buy") nat
-- Parses: "buy 3" or "3 buy"

-- Transform results
map String.toUpper (word "hello")

-- Try multiple alternatives
choice [ word "yes", word "no" ]

-- Match zero or more
tuple2 (word "add") (repeat nat)
-- Parses: "add 1 2 3"
```

### Autocomplete

```elm
import Nld exposing (topK, tuple2, word)
import Set

-- Get completion suggestions for what could come next
topK 5 (word "buy") []
-- Returns: [ Set containing "buy" ]

-- Autocomplete explores the parse tree deeply
-- After matching "buy", suggests "apples"
topK 5 (tuple2 (word "buy") (word "apples")) [ "buy" ]
-- Returns: [ Set containing "apples" ]
```

## Modules

- **`Nld`** - The main parser type and combinators
- **`Peach`** - Lazy priority search data structure (used internally, but exposed for advanced use)

## Architecture

### Lazy Evaluation

Both `Nld` and `Peach` use lazy evaluation to efficiently explore large search spaces:

- **`Peach`** maintains a priority queue of thunks (suspended computations) that are only evaluated when results are extracted
- **`Nld.autocomplete`** walks the parse tree lazily, finding suggestions at failure points without exploring unnecessary branches
- Use `take n` to extract only the first `n` results without computing the rest

## Attribution

This is an Elm port of Paul Chiusano's Unison libraries:

- [pchiusano/nlds](https://share.unison-lang.org/@pchiusano/nlds) - Natural Language Disambiguator
- [pchiusano/peachy](https://share.unison-lang.org/@pchiusano/peachy) - Priority search

## License

MIT
