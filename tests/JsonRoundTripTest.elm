module JsonRoundTripTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Nld.Grammar as Grammar exposing (Grammar(..))
import Nld.TokenType as TokenType exposing (TokenType(..))
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "JSON round-trip"
        [ describe "Nld.TokenType"
            [ fuzz tokenTypeFuzzer "encode >> decode == identity" <|
                \tt ->
                    TokenType.encode tt
                        |> Decode.decodeValue TokenType.decoder
                        |> Expect.equal (Ok tt)
            , test "decoding unknown tag fails" <|
                \() ->
                    Decode.decodeString TokenType.decoder "{\"tag\":\"bogus\"}"
                        |> Expect.err
            ]
        , describe "Nld.Grammar"
            [ fuzz grammarFuzzer "encode >> decode == identity" <|
                \g ->
                    Grammar.encode g
                        |> Decode.decodeValue Grammar.decoder
                        |> Expect.equal (Ok g)
            , test "decoding unknown tag fails" <|
                \() ->
                    Decode.decodeString Grammar.decoder "{\"tag\":\"bogus\"}"
                        |> Expect.err
            ]
        ]



-- FUZZERS


tokenTypeFuzzer : Fuzzer TokenType
tokenTypeFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant NatToken
        , Fuzz.constant IntToken
        , Fuzz.constant FloatToken
        , Fuzz.constant BooleanToken
        , Fuzz.constant DateToken
        , Fuzz.constant DateTimeToken
        , Fuzz.constant TimeToken
        , Fuzz.map CurrencyToken Fuzz.string
        ]


{-| Bounded-depth fuzzer to keep generated grammars finite.
-}
grammarFuzzer : Fuzzer Grammar
grammarFuzzer =
    grammarFuzzerAtDepth 3


grammarFuzzerAtDepth : Int -> Fuzzer Grammar
grammarFuzzerAtDepth depth =
    let
        leaves =
            [ Fuzz.constant AnyToken
            , Fuzz.map Literal Fuzz.string
            , Fuzz.map Token tokenTypeFuzzer
            , Fuzz.map MinimalTokenGrammar
                (Fuzz.listOfLengthBetween 0
                    4
                    (Fuzz.pair niceFloat Fuzz.string)
                )
            ]
    in
    if depth <= 0 then
        Fuzz.oneOf leaves

    else
        let
            child =
                grammarFuzzerAtDepth (depth - 1)
        in
        Fuzz.oneOf
            (leaves
                ++ [ Fuzz.map RepeatGrammar child
                   , Fuzz.map Seq (Fuzz.listOfLengthBetween 0 4 child)
                   , Fuzz.map Choice (Fuzz.listOfLengthBetween 0 4 child)
                   ]
            )


{-| Avoid NaN/Infinity which don't round-trip through JSON.
-}
niceFloat : Fuzzer Float
niceFloat =
    Fuzz.niceFloat
