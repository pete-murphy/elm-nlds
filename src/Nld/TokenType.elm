module Nld.TokenType exposing
    ( TokenType(..)
    , decoder, encode
    )

{-|

@docs TokenType
@docs decoder, encode

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| The type of a token appearing in a `Grammar`. An autocomplete provider can
use this information to provide a richer experience when editing text that is
meant to be parsed by that grammar.

Note: the `String` in `Currency` is an ISO 4217 currency code.

-}
type TokenType
    = CurrencyToken String
    | NatToken
    | IntToken
    | FloatToken
    | BooleanToken
    | DateToken
    | DateTimeToken
    | TimeToken


{-| Encode a `TokenType` to JSON.

    import Json.Encode as Encode

    Encode.encode 0 (encode NatToken)
    --> "{\"tag\":\"nat\"}"

-}
encode : TokenType -> Encode.Value
encode tokenType =
    case tokenType of
        NatToken ->
            Encode.object [ ( "tag", Encode.string "nat" ) ]

        IntToken ->
            Encode.object [ ( "tag", Encode.string "int" ) ]

        FloatToken ->
            Encode.object [ ( "tag", Encode.string "float" ) ]

        BooleanToken ->
            Encode.object [ ( "tag", Encode.string "boolean" ) ]

        DateToken ->
            Encode.object [ ( "tag", Encode.string "date" ) ]

        DateTimeToken ->
            Encode.object [ ( "tag", Encode.string "dateTime" ) ]

        TimeToken ->
            Encode.object [ ( "tag", Encode.string "time" ) ]

        CurrencyToken code ->
            Encode.object
                [ ( "tag", Encode.string "currency" )
                , ( "value", Encode.string code )
                ]


{-| Decode a `TokenType` from JSON.

    import Json.Decode as Decode

    Decode.decodeString decoder "{\"tag\":\"nat\"}"
    --> Ok NatToken

-}
decoder : Decoder TokenType
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "nat" ->
                        Decode.succeed NatToken

                    "int" ->
                        Decode.succeed IntToken

                    "float" ->
                        Decode.succeed FloatToken

                    "boolean" ->
                        Decode.succeed BooleanToken

                    "date" ->
                        Decode.succeed DateToken

                    "dateTime" ->
                        Decode.succeed DateTimeToken

                    "time" ->
                        Decode.succeed TimeToken

                    "currency" ->
                        Decode.field "value" Decode.string
                            |> Decode.map CurrencyToken

                    _ ->
                        Decode.fail ("invalid TokenType tag: " ++ tag)
            )
