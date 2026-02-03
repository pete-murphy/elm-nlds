module CommandParser exposing
    ( Command(..)
    , parseCommand
    , examples
    )

{-| A simple command parser that demonstrates elm-nlds features.

This example parses file management commands with:

  - Synonym support (delete/remove/rm all become "delete")
  - Order-independent tokens ("file delete" works like "delete file")
  - Noise tolerance ("please delete the file" extracts "delete file")


## Try it in elm repl

    import CommandParser exposing (..)
    parseCommand "delete file.txt"
    --> Just (Delete "file.txt")

    parseCommand "rm old.log"
    --> Just (Delete "old.log")

    parseCommand "file.txt delete"
    --> Just (Delete "file.txt")

    parseCommand "please remove the temp.txt"
    --> Just (Delete "temp.txt")

-}

import Nld exposing (Nld, choice, runTake, token, tuple2, word, words)


{-| Commands our parser understands.
-}
type Command
    = Delete String
    | Create String
    | Copy String String
    | Help


{-| Parser for delete commands.
Accepts "delete", "remove", "rm" as synonyms.
-}
deleteCommand : Nld Command
deleteCommand =
    tuple2 (words [ "delete", "remove", "rm" ]) token
        |> Nld.map (\( _, filename ) -> Delete filename)


{-| Parser for create commands.
Accepts "create", "new", "touch" as synonyms.
-}
createCommand : Nld Command
createCommand =
    tuple2 (words [ "create", "new", "touch" ]) token
        |> Nld.map (\( _, filename ) -> Create filename)


{-| Parser for copy commands.
-}
copyCommand : Nld Command
copyCommand =
    Nld.map3
        (\_ src dest -> Copy src dest)
        (words [ "copy", "cp" ])
        token
        token


{-| Parser for help command.
-}
helpCommand : Nld Command
helpCommand =
    word "help"
        |> Nld.map (\_ -> Help)


{-| Combined parser for all commands.
-}
command : Nld Command
command =
    choice
        [ deleteCommand
        , createCommand
        , copyCommand
        , helpCommand
        ]


{-| Parse a command from user input.
-}
parseCommand : String -> Maybe Command
parseCommand input =
    let
        tokens =
            String.words (String.toLower input)
    in
    runTake 1 command tokens
        |> List.head


{-| Example inputs demonstrating the parser.

    List.map (\(input, _) -> (input, parseCommand input)) examples

-}
examples : List ( String, Command )
examples =
    [ ( "delete file.txt", Delete "file.txt" )
    , ( "rm old.log", Delete "old.log" )
    , ( "file.txt delete", Delete "file.txt" )
    , ( "please remove the temp.txt", Delete "temp.txt" )
    , ( "create notes.md", Create "notes.md" )
    , ( "new document.txt", Create "document.txt" )
    , ( "copy a.txt b.txt", Copy "a.txt" "b.txt" )
    , ( "help", Help )
    ]
