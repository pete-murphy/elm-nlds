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
--> [ ( "delete", "file" ) ]

runTake 1 deleteCommand [ "please", "remove", "the", "file" ]
--> [ ( "delete", "file" ) ]  -- ignores irrelevant tokens, canonicalizes synonyms
```

## Design Goals

1. **Order-independent parsing** - Tokens can appear in any order. The parser prefers the specified order but handles reordering gracefully.

2. **Noise-tolerant** - Irrelevant tokens are ignored. Users can type "please delete the file" and it still works.

3. **Synonym support** - Define synonyms that canonicalize to a single value. "delete", "remove", and "rm" all become "delete".

4. **Ranked results** - Results are lazily produced in priority order. Better matches (correct order, fewer gaps) come first.

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
--> [ "hello" ]

-- Match any of several synonyms (canonicalizes to first)
runList (words [ "yes", "yeah", "yep" ]) [ "yeah" ]
--> [ "yes" ]

-- Match any token
runList token [ "anything" ]
--> [ "anything" ]

-- Match a natural number
runList nat [ "42" ]
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

Build context-aware completions that adapt as the user types:

```elm
import Nld exposing (topK, tuple3, words, word)
import Set

-- A three-part command: verb -> object -> modifier
-- Each position supports synonyms
parser =
    tuple3
        (words [ "buy", "sell" ])
        (words [ "apple", "orange" ])
        (word "now")

-- Empty input: suggests verbs (with synonyms)
topK 3 parser []
-- [ Set.fromList [ "buy", "sell" ] ]

-- After verb: suggests objects
topK 3 parser [ "buy" ]
-- [ Set.fromList [ "apple", "orange" ] ]

-- After verb + object: suggests modifier
topK 3 parser [ "buy", "apple" ]
-- [ Set.fromList [ "now" ] ]

-- Complete parse: no suggestions needed
topK 3 parser [ "buy", "apple", "now" ]
-- []
```

The autocomplete explores the parse tree lazily, finding the next tokens needed at each step. Combined with order-independence, users can type `apple buy` and still get `now` as the suggestion.

## Modules

- **`Nld`** - The main parser type and combinators
- **`Peach`** - Lazy priority search data structure (used internally, but exposed for advanced use)

## Attribution

This is an Elm port of Paul Chiusano's Unison libraries:

- [pchiusano/nlds](https://share.unison-lang.org/@pchiusano/nlds) - Natural Language Disambiguator
- [pchiusano/peachy](https://share.unison-lang.org/@pchiusano/peachy) - Priority search

## License

MIT
