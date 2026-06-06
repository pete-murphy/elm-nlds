module Nld.Grammar exposing
    ( Grammar(..)
    , seq, choice
    , encode, decoder
    )

{-|

@docs Grammar
@docs seq, choice
@docs encode, decoder

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Nld.TokenType as TokenType exposing (TokenType)


{-| A first-class representation of an NLD-based grammar.
-}
type Grammar
    = AnyToken
    | Literal String
    | MinimalTokenGrammar (List ( Float, String ))
    | RepeatGrammar Grammar
    | Seq (List Grammar)
    | Choice (List Grammar)
    | Token TokenType


{-| Combine a list of `Grammar` values into a single `Seq`, flattening
any nested `Seq` nodes.

    import Nld.TokenType exposing (TokenType(..))

    seq [ Literal "hello", Literal "world" ]
    --> Seq [ Literal "hello", Literal "world" ]

    seq [ Literal "a", Seq [ Literal "b", Literal "c" ] ]
    --> Seq [ Literal "a", Literal "b", Literal "c" ]

-}
seq : List Grammar -> Grammar
seq gs =
    let
        flatten g =
            case g of
                Seq inner ->
                    inner

                other ->
                    [ other ]
    in
    Seq (List.concatMap flatten gs)


{-| Combine a list of `Grammar` values into a single `Choice`, flattening
any nested `Choice` nodes.

    import Nld.TokenType exposing (TokenType(..))

    choice [ Literal "yes", Literal "no" ]
    --> Choice [ Literal "yes", Literal "no" ]

    choice [ Literal "a", Choice [ Literal "b", Literal "c" ] ]
    --> Choice [ Literal "a", Literal "b", Literal "c" ]

-}
choice : List Grammar -> Grammar
choice gs =
    let
        flatten g =
            case g of
                Choice inner ->
                    inner

                other ->
                    [ other ]
    in
    Choice (List.concatMap flatten gs)


{-| Encode a `Grammar` as a JSON value.

    import Json.Encode as Encode
    import Nld.TokenType exposing (TokenType(..))

    encode (Token NatToken)
    --> Encode.object [ ( "tag", Encode.string "token" ), ( "value", Encode.object [ ( "tag", Encode.string "nat" ) ] ) ]

-}
encode : Grammar -> Encode.Value
encode g =
    let
        enc grammar_ =
            case grammar_ of
                Literal t ->
                    Encode.object
                        [ ( "tag", Encode.string "literal" )
                        , ( "value", Encode.string t )
                        ]

                MinimalTokenGrammar pairs ->
                    Encode.object
                        [ ( "tag", Encode.string "minimalToken" )
                        , ( "value"
                          , Encode.list
                                (\( w, t ) ->
                                    Encode.object
                                        [ ( "weight", Encode.float w )
                                        , ( "text", Encode.string t )
                                        ]
                                )
                                pairs
                          )
                        ]

                AnyToken ->
                    Encode.object [ ( "tag", Encode.string "anyToken" ) ]

                Token tt ->
                    Encode.object
                        [ ( "tag", Encode.string "token" )
                        , ( "value", TokenType.encode tt )
                        ]

                Seq gs ->
                    Encode.object
                        [ ( "tag", Encode.string "seq" )
                        , ( "value", Encode.list enc gs )
                        ]

                Choice gs ->
                    Encode.object
                        [ ( "tag", Encode.string "choice" )
                        , ( "value", Encode.list enc gs )
                        ]

                RepeatGrammar inner ->
                    Encode.object
                        [ ( "tag", Encode.string "repeat" )
                        , ( "value", enc inner )
                        ]
    in
    enc g


{-| Decode a `Grammar` from JSON.

    import Json.Decode as Decode

    Decode.decodeString decoder """{"tag":"anyToken"}"""
    --> Ok AnyToken

-}
decoder : Decoder Grammar
decoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "literal" ->
                        Decode.field "value" Decode.string
                            |> Decode.map Literal

                    "anyToken" ->
                        Decode.succeed AnyToken

                    "token" ->
                        Decode.field "value" TokenType.decoder
                            |> Decode.map Token

                    "minimalToken" ->
                        Decode.field "value"
                            (Decode.list
                                (Decode.map2 Tuple.pair
                                    (Decode.field "weight" Decode.float)
                                    (Decode.field "text" Decode.string)
                                )
                            )
                            |> Decode.map MinimalTokenGrammar

                    "seq" ->
                        Decode.field "value" (Decode.list (Decode.lazy (\_ -> decoder)))
                            |> Decode.map Seq

                    "choice" ->
                        Decode.field "value" (Decode.list (Decode.lazy (\_ -> decoder)))
                            |> Decode.map Choice

                    "repeat" ->
                        Decode.field "value" (Decode.lazy (\_ -> decoder))
                            |> Decode.map RepeatGrammar

                    _ ->
                        Decode.fail ("invalid Grammar tag: " ++ tag)
            )
