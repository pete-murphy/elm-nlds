# Changelog [![Elm package](https://img.shields.io/elm-package/v/pete-murphy/elm-nlds.svg)](https://package.elm-lang.org/packages/pete-murphy/elm-nlds/latest/)

All notable changes to
[the `pete-murphy/elm-nlds` elm package](http://package.elm-lang.org/packages/pete-murphy/elm-nlds/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.1] - 2026-02-02

### Fixed

- Removed inaccurate "lazily" claims from documentation. The Elm port evaluates eagerly (unlike the original Unison implementation).

## [1.0.0] - 2026-02-02

Initial release. This is an Elm port of Paul Chiusano's Unison libraries
[pchiusano/nlds](https://share.unison-lang.org/@pchiusano/nlds) and
[pchiusano/peachy](https://share.unison-lang.org/@pchiusano/peachy).

### Added

#### Nld Module - Natural Language Disambiguator

A flexible parser for loosely ordered token sequences that:
- Ignores irrelevant tokens
- Allows tokens to appear in any order (preferring specified order)
- Produces results in priority order

**Token Matchers:**
- `word` - Match a specific word
- `words` - Match synonyms, canonicalizing to the first
- `token` - Match any token
- `nat` - Match natural numbers
- `tokenMatching` - Match tokens satisfying a predicate
- `minimalToken` - Match weighted tokens

**Indexed Variants:**
- `indexedWord`, `indexedWords`, `indexedToken`, `indexedNat`, `indexedTokenMatching`
- Return both the matched value and its position

**Combinators:**
- `map`, `map2`, `map3` - Transform parser results
- `andThen` - Sequence parsers
- `tuple2`, `tuple3` - Combine parsers into tuples
- `choice` - Try multiple alternatives
- `repeat` - Match zero or more occurrences

**Autocompletion:**
- `autocomplete` - Get suggestions for incomplete input
- `topK` - Get top K completion suggestions

**Running Parsers:**
- `run`, `runList`, `runTake` - Execute parsers on token lists

#### Peach Module - Priority Search

A priority search data structure that explores branches with smaller weights first.

- `peach` - Create from weighted values
- `fail` - Empty computation
- `map`, `flatMap` - Transform and chain computations
- `choose` - Choose between multiple computations
- `head`, `toList`, `take` - Extract results
- `rankBy`, `each`, `optional` - Convenience constructors
