module ActivityParserTest exposing (suite)

{-| Tests for the activity parser from examples/Main.elm.

Copies the types and parsers directly so we can assert expected parse
results without Browser/port/Date dependencies.

-}

import Expect
import Nld exposing (Nld)
import Nld.TokenType exposing (TokenType(..))
import Test exposing (Test, describe, test)
import Time exposing (Weekday(..))



-- TYPES (from examples/Main.elm)


type alias ActivityEntry =
    { activity : Activity
    , when : Maybe When
    , minutes : Maybe Int
    }


type Activity
    = Cycling
    | Running
    | Swimming
    | Yoga


type When
    = MinutesAgo Int
    | Today (Maybe Time)
    | DaysAgo Int (Maybe Time)
    | OnDay Weekday (Maybe Time)


type alias Time =
    { hour : Int
    , minute : Int
    , meridiem : Meridiem
    }


type Meridiem
    = Am
    | Pm



-- SYNONYMS (from examples/Main.elm)


activitySynonyms : List ( Activity, List String )
activitySynonyms =
    [ ( Cycling, [ "bike", "biking", "biked", "cycle", "cycling", "cycled", "rode", "riding" ] )
    , ( Running, [ "run", "running", "ran", "jog", "jogging", "jogged" ] )
    , ( Swimming, [ "swim", "swimming", "swam", "laps" ] )
    , ( Yoga, [ "yoga" ] )
    ]



-- PARSERS (from examples/Main.elm)


activityParser : Nld Activity
activityParser =
    Nld.choice
        (List.map
            (\( activity, syns ) -> Nld.map (\_ -> activity) (Nld.words syns))
            activitySynonyms
        )


numberWordsParser : Nld Int
numberWordsParser =
    [ ( 1, "one" )
    , ( 2, "two" )
    , ( 3, "three" )
    , ( 4, "four" )
    , ( 5, "five" )
    , ( 10, "ten" )
    ]
        |> List.map (\( n, w ) -> Nld.map (\_ -> n) (Nld.word w))
        |> Nld.choice


minutesParser : Nld Int
minutesParser =
    let
        anHour =
            Nld.tuple2 (Nld.words [ "an", "a", "one", "1" ]) hour

        hours =
            Nld.words [ "hours", "hour", "hrs", "hr" ]

        minutes =
            Nld.words [ "minutes", "minute", "mins", "min" ]

        and =
            Nld.words [ "and", "&" ]

        a =
            Nld.words [ "a", "an" ]

        half =
            Nld.word "half"

        hour =
            Nld.words [ "hour", "hr" ]
    in
    Nld.choice
        [ Nld.map3 (\_ _ _ -> 30) a half hour
        , Nld.map4 (\_ _ _ _ -> 90) anHour and a half
        , Nld.map4 (\_ _ _ _ -> 90) hour and a half
        , Nld.map5 (\n _ _ _ _ -> n * 60 + 30) numberWordsParser and a half hours
        , Nld.map5 (\n _ _ _ _ -> n * 60 + 30) Nld.nat and a half hours
        , Nld.map4 (\_ _ mins _ -> 60 + mins) anHour and Nld.nat minutes
        , Nld.map2 (\mins _ -> mins) Nld.nat minutes
        , Nld.map2 (\hrs _ -> hrs * 60) Nld.nat hours
        , Nld.map (\_ -> 60) anHour
        , Nld.map2 (\n _ -> n * 60) numberWordsParser hours
        , Nld.map2 (\n _ -> n) numberWordsParser minutes
        ]


whenParser : Time -> Nld When
whenParser currentTime =
    let
        whenParserHelp : Nld (Maybe Time -> When)
        whenParserHelp =
            Nld.choice
                [ Nld.map (\_ -> Today) (Nld.word "today")
                , Nld.map3 (\days _ _ -> DaysAgo days) Nld.nat (Nld.words [ "days", "day" ]) (Nld.word "ago")
                , Nld.map3 (\days _ _ -> DaysAgo days) numberWordsParser (Nld.word "days") (Nld.word "ago")
                , Nld.map (\_ -> DaysAgo 1) (Nld.word "yesterday")
                , Nld.map2 (\_ _ -> DaysAgo 1) (Nld.word "last") (Nld.word "night")
                , Nld.map (\weekday -> OnDay weekday) weekdayParser
                , Nld.succeed Today
                ]
    in
    Nld.choice
        [ Nld.map2 (\mins _ -> MinutesAgo mins) minutesParser (Nld.word "ago")
        , whenParserHelp
            |> Nld.andMap (timeParser currentTime |> Nld.map Just)
        , whenParserHelp
            |> Nld.andMap (Nld.succeed Nothing)
        ]


timeParser : Time -> Nld Time
timeParser currentTime =
    let
        clockTimeParser =
            Nld.map Tuple.first (Nld.indexedTokenOfType TimeToken (clockTimeFromString currentTime))

        timeOfDayParser =
            Nld.choice
                [ Nld.map (\_ -> ( 9, Am )) (Nld.word "morning")
                , Nld.map (\_ -> ( 12, Pm )) (Nld.word "noon")
                , Nld.map (\_ -> ( 3, Pm )) (Nld.word "afternoon")
                , Nld.map (\_ -> ( 6, Pm )) (Nld.word "evening")
                , Nld.map (\_ -> ( 9, Pm )) (Nld.word "night")
                ]
                |> Nld.map (\( h, m ) -> { hour = h, minute = 0, meridiem = m })
    in
    Nld.choice
        [ Nld.map2 (\clk tod -> { clk | meridiem = tod.meridiem }) clockTimeParser timeOfDayParser
        , clockTimeParser
        , timeOfDayParser
        ]


weekdayParser : Nld Weekday
weekdayParser =
    Nld.choice
        [ Nld.map (\_ -> Mon) (Nld.words [ "monday", "mon" ])
        , Nld.map (\_ -> Tue) (Nld.words [ "tuesday", "tue", "tues" ])
        , Nld.map (\_ -> Wed) (Nld.words [ "wednesday", "wed", "weds" ])
        , Nld.map (\_ -> Thu) (Nld.words [ "thursday", "thu", "thurs" ])
        , Nld.map (\_ -> Fri) (Nld.words [ "friday", "fri" ])
        , Nld.map (\_ -> Sat) (Nld.words [ "saturday", "sat" ])
        , Nld.map (\_ -> Sun) (Nld.words [ "sunday", "sun" ])
        ]


activityEntryParser : Time -> Nld ActivityEntry
activityEntryParser currentTime =
    let
        whenP =
            Nld.map Just (whenParser currentTime)

        minutesP =
            Nld.map Just minutesParser
    in
    Nld.choice
        [ Nld.map3 ActivityEntry activityParser whenP minutesP
        , Nld.map3 (\act mins whn -> ActivityEntry act whn mins) activityParser minutesP whenP
        , Nld.map3 ActivityEntry activityParser whenP (Nld.succeed Nothing)
        , Nld.map3 ActivityEntry activityParser (Nld.succeed Nothing) minutesP
        , Nld.map3 ActivityEntry activityParser (Nld.succeed Nothing) (Nld.succeed Nothing)
        ]



-- TOKENIZER
-- Reimplemented from examples/Main.elm without the Regex dependency.
-- The original uses a regex to merge "3 pm" -> "3pm" before splitting;
-- this version does the same merge at the word level.


tokenize : String -> List String
tokenize input =
    input
        |> String.toLower
        |> String.words
        |> mergeTimeTokens
        |> List.map cleanToken
        |> List.filter (not << String.isEmpty)


{-| Strip non-alphanumeric characters from a token, but preserve colons
in tokens that contain them (e.g. time tokens like "3:30pm").
-}
cleanToken : String -> String
cleanToken tok =
    if String.contains ":" tok then
        String.filter (\c -> Char.isAlphaNum c || c == ':') tok

    else
        String.filter Char.isAlphaNum tok


{-| Merge adjacent tokens like ["3", "pm"] or ["3:30", "pm"] into ["3pm"] or ["3:30pm"].
Replicates the regex \\d{1,2}(:\\d{2})?\\s\*(am|pm) collapsing from Main.elm.
-}
mergeTimeTokens : List String -> List String
mergeTimeTokens tokens =
    case tokens of
        [] ->
            []

        tok :: next :: rest ->
            if isTimeLikePrefix tok && (next == "am" || next == "pm") then
                (tok ++ next) :: mergeTimeTokens rest

            else
                tok :: mergeTimeTokens (next :: rest)

        [ tok ] ->
            [ tok ]


isTimeLikePrefix : String -> Bool
isTimeLikePrefix s =
    case String.split ":" s of
        [ h ] ->
            not (String.isEmpty h)
                && String.length h
                <= 2
                && String.all Char.isDigit h

        [ h, m ] ->
            not (String.isEmpty h)
                && String.length h
                <= 2
                && String.all Char.isDigit h
                && String.length m
                == 2
                && String.all Char.isDigit m

        _ ->
            False



-- CLOCK TIME PARSER (from examples/Main.elm)


clockTimeFromString : Time -> String -> Maybe Time
clockTimeFromString currentTime tok =
    let
        parseMeridiem hour =
            if String.endsWith "pm" tok || String.endsWith "p" tok then
                Pm

            else if String.endsWith "am" tok || String.endsWith "a" tok then
                Am

            else
                case ( modBy 12 hour < modBy 12 currentTime.hour, currentTime.meridiem ) of
                    ( True, m ) ->
                        m

                    ( False, Am ) ->
                        Pm

                    ( False, Pm ) ->
                        Am

        toTime hour minute =
            let
                meridiem =
                    parseMeridiem hour
            in
            if hour >= 1 && hour <= 12 && minute >= 0 && minute < 60 then
                Just { hour = hour, minute = minute, meridiem = meridiem }

            else if hour > 12 && hour <= 24 && minute >= 0 && minute < 60 then
                Just { hour = hour - 12, minute = minute, meridiem = Pm }

            else
                Nothing
    in
    case String.split ":" tok of
        [ hourStr, minuteStr ] ->
            case ( String.toInt hourStr, String.toInt (String.filter Char.isDigit minuteStr) ) of
                ( Just hour, Just minute ) ->
                    toTime hour minute

                _ ->
                    Nothing

        [ bare ] ->
            if String.all Char.isDigit bare then
                Nothing

            else
                String.filter Char.isDigit bare
                    |> String.toInt
                    |> Maybe.andThen (\hour -> toTime hour 0)

        _ ->
            Nothing



-- SCORING (from examples/Main.elm)


scoreEntry : ActivityEntry -> ( Int, Int )
scoreEntry entry =
    let
        minutesScore =
            case entry.minutes of
                Just n ->
                    if n > 240 then
                        2

                    else if n == 60 then
                        1

                    else
                        0

                Nothing ->
                    1

        whenScore =
            case entry.when of
                Just (MinutesAgo _) ->
                    0

                Just (Today (Just _)) ->
                    1

                Just (DaysAgo _ (Just _)) ->
                    0

                Just (OnDay _ (Just _)) ->
                    0

                Just (Today Nothing) ->
                    2

                Just (DaysAgo _ Nothing) ->
                    1

                Just (OnDay _ Nothing) ->
                    1

                Nothing ->
                    3
    in
    ( minutesScore + whenScore
    , -(Maybe.withDefault 0 (Maybe.map (min 240) entry.minutes))
    )



-- PARSE ENTRY POINT


parseActivity : Time -> String -> List ActivityEntry
parseActivity currentTime input =
    Nld.runTake 20 (activityEntryParser currentTime) (tokenize input)
        |> List.sortBy scoreEntry


bestParse : Time -> String -> Maybe ActivityEntry
bestParse currentTime input =
    parseActivity currentTime input |> List.head



-- TEST HELPERS


{-| Fixed "current time" for all tests: 3:00 PM.
This affects meridiem disambiguation when no am/pm suffix is given.
-}
now : Time
now =
    { hour = 3, minute = 0, meridiem = Pm }


parse : String -> Maybe ActivityEntry
parse =
    bestParse now



-- TEST SUITE


suite : Test
suite =
    describe "Activity Parser (from examples/Main.elm)"
        [ examplesFromUI
        , activityRecognition
        , durationParsing
        , whenParsing
        , tokenizerTests
        ]


examplesFromUI : Test
examplesFromUI =
    describe "examples from UI"
        [ test "ran for 45 minutes yesterday morning" <|
            \() ->
                let
                    result =
                        parse "ran for 45 minutes yesterday morning"
                in
                Expect.all
                    [ \r -> Maybe.map .activity r |> Expect.equal (Just Running)
                    , \r -> Maybe.andThen .minutes r |> Expect.equal (Just 45)
                    , \r ->
                        Maybe.andThen .when r
                            |> Expect.equal
                                (Just
                                    (DaysAgo 1
                                        (Just { hour = 9, minute = 0, meridiem = Am })
                                    )
                                )
                    ]
                    result
        , test "just went for a jog -> Running" <|
            \() ->
                parse "just went for a jog"
                    |> Maybe.map .activity
                    |> Expect.equal (Just Running)
        , test "swam for an hour on Thursday" <|
            \() ->
                let
                    result =
                        parse "swam for an hour on Thursday"
                in
                Expect.all
                    [ \r -> Maybe.map .activity r |> Expect.equal (Just Swimming)
                    , \r -> Maybe.andThen .minutes r |> Expect.equal (Just 60)
                    , \r ->
                        Maybe.andThen .when r
                            |> expectWhenTag "OnDay Thu"
                    ]
                    result
        , test "bike ride, tues 3:30pm" <|
            \() ->
                let
                    result =
                        parse "bike ride, tues 3:30pm"
                in
                Expect.all
                    [ \r -> Maybe.map .activity r |> Expect.equal (Just Cycling)
                    , \r ->
                        Maybe.andThen .when r
                            |> Expect.equal
                                (Just
                                    (OnDay Tue
                                        (Just { hour = 3, minute = 30, meridiem = Pm })
                                    )
                                )
                    ]
                    result
        , test "yoga 3 pm" <|
            \() ->
                let
                    result =
                        parse "yoga 3 pm"
                in
                Expect.all
                    [ \r -> Maybe.map .activity r |> Expect.equal (Just Yoga)
                    , \r ->
                        Maybe.andThen .when r
                            |> Expect.equal
                                (Just
                                    (Today
                                        (Just { hour = 3, minute = 0, meridiem = Pm })
                                    )
                                )
                    ]
                    result
        , test "yesterday at 1 am I went swimming for 30 minutes" <|
            \() ->
                let
                    result =
                        parse "yesterday at 1 am I went swimming for 30 minutes"
                in
                Expect.all
                    [ \r -> Maybe.map .activity r |> Expect.equal (Just Swimming)
                    , \r -> Maybe.andThen .minutes r |> Expect.equal (Just 30)
                    , \r ->
                        Maybe.andThen .when r
                            |> Expect.equal
                                (Just
                                    (DaysAgo 1
                                        (Just { hour = 1, minute = 0, meridiem = Am })
                                    )
                                )
                    ]
                    result
        ]


activityRecognition : Test
activityRecognition =
    describe "activity recognition"
        [ test "cycling synonyms" <|
            \() ->
                List.map (\input -> parse input |> Maybe.map .activity)
                    [ "bike", "biking", "cycling", "rode" ]
                    |> Expect.equal
                        [ Just Cycling, Just Cycling, Just Cycling, Just Cycling ]
        , test "running synonyms" <|
            \() ->
                List.map (\input -> parse input |> Maybe.map .activity)
                    [ "run", "running", "ran", "jog", "jogging" ]
                    |> Expect.equal
                        [ Just Running, Just Running, Just Running, Just Running, Just Running ]
        , test "swimming synonyms" <|
            \() ->
                List.map (\input -> parse input |> Maybe.map .activity)
                    [ "swim", "swimming", "swam", "laps" ]
                    |> Expect.equal
                        [ Just Swimming, Just Swimming, Just Swimming, Just Swimming ]
        , test "yoga" <|
            \() ->
                parse "yoga"
                    |> Maybe.map .activity
                    |> Expect.equal (Just Yoga)
        ]


durationParsing : Test
durationParsing =
    describe "duration parsing"
        [ test "N minutes" <|
            \() ->
                parse "ran 30 minutes"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 30)
        , test "N hours" <|
            \() ->
                parse "ran 2 hours"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 120)
        , test "an hour" <|
            \() ->
                parse "swam for an hour"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 60)
        , test "a half hour" <|
            \() ->
                parse "ran a half hour"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 30)
        , test "an hour and a half" <|
            \() ->
                parse "cycled an hour and a half"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 90)
        , test "word number: one hour" <|
            \() ->
                parse "swam one hour"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 60)
        , test "word number: two hours" <|
            \() ->
                parse "ran two hours"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 120)
        , test "an hour and N minutes" <|
            \() ->
                parse "swam an hour and 15 minutes"
                    |> Maybe.andThen .minutes
                    |> Expect.equal (Just 75)
        ]


whenParsing : Test
whenParsing =
    describe "when parsing"
        [ test "yesterday" <|
            \() ->
                parse "ran yesterday"
                    |> Maybe.andThen .when
                    |> expectWhenTag "DaysAgo 1"
        , test "N days ago" <|
            \() ->
                parse "swam 3 days ago"
                    |> Maybe.andThen .when
                    |> expectWhenTag "DaysAgo 3"
        , test "weekday" <|
            \() ->
                parse "swam on thursday"
                    |> Maybe.andThen .when
                    |> expectWhenTag "OnDay Thu"
        , test "today" <|
            \() ->
                parse "ran today"
                    |> Maybe.andThen .when
                    |> expectWhenTag "Today"
        , test "minutes ago" <|
            \() ->
                parse "ran 30 minutes ago"
                    |> Maybe.andThen .when
                    |> Expect.equal (Just (MinutesAgo 30))
        , test "yesterday morning" <|
            \() ->
                parse "ran yesterday morning"
                    |> Maybe.andThen .when
                    |> Expect.equal
                        (Just
                            (DaysAgo 1
                                (Just { hour = 9, minute = 0, meridiem = Am })
                            )
                        )
        , test "thursday evening" <|
            \() ->
                parse "ran thursday evening"
                    |> Maybe.andThen .when
                    |> Expect.equal
                        (Just
                            (OnDay Thu
                                (Just { hour = 6, minute = 0, meridiem = Pm })
                            )
                        )
        ]


tokenizerTests : Test
tokenizerTests =
    describe "tokenizer"
        [ test "merges '3 pm' into '3pm'" <|
            \() ->
                tokenize "yoga 3 pm"
                    |> Expect.equal [ "yoga", "3pm" ]
        , test "merges '1 am' into '1am'" <|
            \() ->
                tokenize "yesterday 1 am"
                    |> Expect.equal [ "yesterday", "1am" ]
        , test "merges '3:30 pm' into '3:30pm' (colon preserved for time tokens)" <|
            \() ->
                tokenize "tues 3:30 pm"
                    |> Expect.equal [ "tues", "3:30pm" ]
        , test "strips punctuation" <|
            \() ->
                tokenize "bike ride, yesterday"
                    |> Expect.equal [ "bike", "ride", "yesterday" ]
        , test "lowercases input" <|
            \() ->
                tokenize "Ran Yesterday"
                    |> Expect.equal [ "ran", "yesterday" ]
        , test "does not merge non-time tokens with am/pm" <|
            \() ->
                tokenize "I am running"
                    |> Expect.equal [ "i", "am", "running" ]
        ]



-- HELPERS


{-| Compare just the "shape" of a When value (ignoring the Maybe Time detail).
Useful for tests where we only care about the day/category, not the exact time.
-}
expectWhenTag : String -> Maybe When -> Expect.Expectation
expectWhenTag expected maybeWhen =
    whenTag maybeWhen |> Expect.equal (Just expected)


whenTag : Maybe When -> Maybe String
whenTag maybeWhen =
    case maybeWhen of
        Nothing ->
            Nothing

        Just (MinutesAgo n) ->
            Just ("MinutesAgo " ++ String.fromInt n)

        Just (Today _) ->
            Just "Today"

        Just (DaysAgo n _) ->
            Just ("DaysAgo " ++ String.fromInt n)

        Just (OnDay weekday _) ->
            Just ("OnDay " ++ weekdayToString weekday)


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"
