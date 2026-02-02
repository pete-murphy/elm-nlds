module SearchQuery exposing
    ( Query(..)
    , UserFilter(..)
    , parseQuery
    , examples
    )

{-| A search query parser that demonstrates elm-nlds features.

This example parses search queries like:

  - "users active"
  - "show me active users from last week"
  - "3 recent orders"

The parser ignores filler words and allows flexible ordering.


## Try it in elm repl

    import SearchQuery exposing (..)
    parseQuery "users active"
    --> Just (ListUsers ActiveUsers)

    parseQuery "active users"
    --> Just (ListUsers ActiveUsers)

    parseQuery "show me the active users"
    --> Just (ListUsers ActiveUsers)

    parseQuery "5 orders"
    --> Just (ListOrders 5)

-}

import Nld exposing (Nld, choice, nat, runTake, tuple2, words)


{-| Search queries our parser understands.
-}
type Query
    = ListUsers UserFilter
    | ListOrders Int
    | ShowStats


type UserFilter
    = ActiveUsers
    | AllUsers


{-| Parser for user listing queries.
-}
usersQuery : Nld Query
usersQuery =
    tuple2
        (words [ "users", "user", "people" ])
        (choice
            [ words [ "active", "online" ] |> Nld.map (\_ -> ActiveUsers)
            , words [ "all", "every" ] |> Nld.map (\_ -> AllUsers)
            ]
        )
        |> Nld.map (\( _, filter ) -> ListUsers filter)


{-| Parser for listing users without a filter (defaults to AllUsers).
-}
usersQuerySimple : Nld Query
usersQuerySimple =
    words [ "users", "user", "people" ]
        |> Nld.map (\_ -> ListUsers AllUsers)


{-| Parser for order listing queries.
-}
ordersQuery : Nld Query
ordersQuery =
    tuple2
        nat
        (words [ "orders", "order", "purchases" ])
        |> Nld.map (\( n, _ ) -> ListOrders n)


{-| Parser for stats queries.
-}
statsQuery : Nld Query
statsQuery =
    words [ "stats", "statistics", "metrics", "dashboard" ]
        |> Nld.map (\_ -> ShowStats)


{-| Combined parser for all queries.
-}
query : Nld Query
query =
    choice
        [ usersQuery
        , ordersQuery
        , statsQuery
        , usersQuerySimple
        ]


{-| Parse a query from user input.
-}
parseQuery : String -> Maybe Query
parseQuery input =
    let
        tokens =
            String.words (String.toLower input)
    in
    runTake 1 query tokens
        |> List.head
        |> Maybe.map Tuple.second


{-| Example inputs demonstrating the parser.

    List.map (\(input, _) -> (input, parseQuery input)) examples

-}
examples : List ( String, Query )
examples =
    [ ( "users active", ListUsers ActiveUsers )
    , ( "active users", ListUsers ActiveUsers )
    , ( "show me the active users", ListUsers ActiveUsers )
    , ( "people online", ListUsers ActiveUsers )
    , ( "5 orders", ListOrders 5 )
    , ( "recent 10 purchases", ListOrders 10 )
    , ( "stats", ShowStats )
    , ( "show dashboard", ShowStats )
    , ( "users", ListUsers AllUsers )
    ]
