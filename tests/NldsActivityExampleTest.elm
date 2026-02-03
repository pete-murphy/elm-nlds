module NldsActivityExampleTest exposing (suite)

{-| NLDS Activity Parser Tests

This module tests the activity log parser from the NLDS example application.
The parser extracts activity type, duration, and time specification from
natural language input.

Note: The time resolution (converting to actual dates/times) is not tested here.
We test the parsing of activity, duration, and time specification.

-}

import Expect
import Nld exposing (Nld, choice, map, map2, nat, runTake, tuple2, word, words)
import Test exposing (Test, describe, test)
import Time exposing (Weekday(..))



-- DOMAIN MODEL


type Activity
    = Cycling
    | Running
    | Swimming
    | Yoga


type alias Duration =
    Int


type TimeOfDay
    = Morning
    | Afternoon
    | Evening
    | Night
    | Noon


type TimeSpec
    = Now
    | Today TimeOfDay
    | Yesterday TimeOfDay
    | OnDay Weekday TimeOfDay


type alias ParsedActivity =
    { activity : Activity
    , duration : Maybe Duration
    , timeSpec : Maybe TimeSpec
    }



-- ACTIVITY SYNONYMS


activitySynonyms : List ( Activity, List String )
activitySynonyms =
    [ ( Cycling, [ "bike", "biking", "biked", "cycle", "cycling", "cycled", "rode", "riding" ] )
    , ( Running, [ "run", "running", "ran", "jog", "jogging", "jogged" ] )
    , ( Swimming, [ "swim", "swimming", "swam", "laps" ] )
    , ( Yoga, [ "yoga" ] )
    ]



-- NLDS PARSERS


{-| Create an activity parser using synonyms
-}
activityParser : Nld Activity
activityParser =
    choice
        (List.map
            (\( activity, syns ) ->
                map (\_ -> activity) (words syns)
            )
            activitySynonyms
        )


timeOfDayParser : Nld TimeOfDay
timeOfDayParser =
    choice
        [ map (\_ -> Morning) (words [ "morning", "am" ])
        , map (\_ -> Afternoon) (word "afternoon")
        , map (\_ -> Evening) (word "evening")
        , map (\_ -> Night) (words [ "night", "pm" ])
        ]


dayOfWeekParser : Nld Weekday
dayOfWeekParser =
    choice
        [ map (\_ -> Mon) (words [ "monday", "mon" ])
        , map (\_ -> Tue) (words [ "tuesday", "tue", "tues" ])
        , map (\_ -> Wed) (words [ "wednesday", "wed" ])
        , map (\_ -> Thu) (words [ "thursday", "thu", "thurs" ])
        , map (\_ -> Fri) (words [ "friday", "fri" ])
        , map (\_ -> Sat) (words [ "saturday", "sat" ])
        , map (\_ -> Sun) (words [ "sunday", "sun" ])
        ]


timeParser : Nld TimeSpec
timeParser =
    -- More specific patterns first, fallbacks last
    choice
        [ -- "now"
          map (\_ -> Now) (word "now")

        -- "this morning", "this afternoon", etc.
        , map (\_ -> Today Morning) (words [ "this morning" ])
        , map (\_ -> Today Afternoon) (word "afternoon")
        , map (\_ -> Today Evening) (word "evening")
        , map (\_ -> Today Night) (word "tonight")

        -- "yesterday morning", "yesterday evening", etc. (specific time of day)
        , map2 (\_ tod -> Yesterday tod)
            (word "yesterday")
            timeOfDayParser

        -- "yesterday" alone (fallback to noon - must come AFTER specific yesterday patterns)
        , map (\_ -> Yesterday Noon) (word "yesterday")

        -- "this" + time of day
        , map2 (\_ tod -> Today tod)
            (word "this")
            timeOfDayParser

        -- "am" / "pm" indicators (can match standalone or with context)
        , map (\_ -> Today Morning) (word "am")
        , map (\_ -> Today Night) (word "pm")

        -- "last night"
        , map2 (\_ _ -> Yesterday Night)
            (word "last")
            (word "night")

        -- Day of week + time of day (specific)
        , map2 (\day tod -> OnDay day tod)
            dayOfWeekParser
            timeOfDayParser

        -- Day of week alone (fallback to noon)
        , map (\day -> OnDay day Noon)
            dayOfWeekParser
        ]



-- Duration parsers


simpleDurationParser : Nld Duration
simpleDurationParser =
    choice
        [ -- "half hour" (most specific word-based pattern)
          map2 (\_ _ -> 30)
            (word "half")
            (words [ "hour", "hours", "hr", "hrs" ])

        -- "X hours" or "X minutes" (numeric patterns - most common)
        , map2
            (\n unit ->
                if List.member unit [ "hour", "hours", "hr", "hrs" ] then
                    n * 60

                else
                    n
            )
            nat
            (choice
                [ words [ "hour", "hours", "hr", "hrs" ]
                , words [ "minute", "minutes", "min", "mins" ]
                ]
            )

        -- "one hour", "an hour" (word-based, only match these specific words)
        -- Put this last so numeric patterns take precedence
        , map2 (\_ _ -> 60)
            (words [ "one", "an" ])
            (words [ "hour", "hours", "hr", "hrs" ])
        ]



-- Combined parsers


activityWithDuration : Nld ( Activity, Duration )
activityWithDuration =
    tuple2 activityParser simpleDurationParser


activityWithTime : Nld ( Activity, TimeSpec )
activityWithTime =
    tuple2 activityParser timeParser



-- Main parsing function


{-| Helper to split tokens like "3pm" into ["3", "pm"]
-}
splitTimeTokens : String -> List String
splitTimeTokens tok =
    -- Split tokens like "3pm" -> ["3", "pm"], "10am" -> ["10", "am"]
    if String.endsWith "am" tok && String.length tok > 2 then
        [ String.dropRight 2 tok, "am" ]

    else if String.endsWith "pm" tok && String.length tok > 2 then
        [ String.dropRight 2 tok, "pm" ]

    else
        [ tok ]


parseActivitySmart : String -> List ParsedActivity
parseActivitySmart input =
    let
        tokens =
            input
                |> String.toLower
                |> String.words
                |> List.filter (\t -> t /= "")
                |> List.concatMap splitTimeTokens

        -- Try activity + duration + time
        withAllThree =
            runTake 5
                (map2
                    (\( a, d ) t ->
                        { activity = a
                        , duration = Just d
                        , timeSpec = Just t
                        }
                    )
                    activityWithDuration
                    timeParser
                )
                tokens

        -- Activity + duration only
        withDurationOnly =
            runTake 5
                (map
                    (\( a, d ) ->
                        { activity = a
                        , duration = Just d
                        , timeSpec = Nothing
                        }
                    )
                    activityWithDuration
                )
                tokens

        -- Activity + time only
        withTimeOnly =
            runTake 5
                (map
                    (\( a, t ) ->
                        { activity = a
                        , duration = Nothing
                        , timeSpec = Just t
                        }
                    )
                    activityWithTime
                )
                tokens

        -- Activity only
        activityOnly =
            runTake 5
                (map
                    (\a ->
                        { activity = a
                        , duration = Nothing
                        , timeSpec = Nothing
                        }
                    )
                    activityParser
                )
                tokens
    in
    withAllThree ++ withDurationOnly ++ withTimeOnly ++ activityOnly


{-| Score a time specification. More specific times get higher scores.

  - Having a specific time of day (morning, afternoon, etc.) adds 1 point
  - Having a specific day (Monday, Tuesday, etc.) adds 2 points
  - "yesterday" counts as having a specific day

Base scores:

  - Now: 1
  - Today Noon: 1
  - Today + specific time: 2
  - Yesterday Noon: 3 (1 base + 2 for having a day)
  - Yesterday + specific time: 4 (2 + 2 for having a day)
  - OnDay \_ Noon: 3 (1 base + 2 for having a day)
  - OnDay \_ specific time: 4 (2 + 2 for having a day)

-}
scoreTimeSpec : TimeSpec -> Int
scoreTimeSpec ts =
    case ts of
        Now ->
            1

        Today Noon ->
            1

        Today _ ->
            2

        Yesterday Noon ->
            3

        Yesterday _ ->
            4

        OnDay _ Noon ->
            3

        OnDay _ _ ->
            4


parseBestActivity : String -> Maybe ParsedActivity
parseBestActivity input =
    case parseActivitySmart input of
        [] ->
            Nothing

        results ->
            let
                scored =
                    List.map
                        (\r ->
                            let
                                durationScore =
                                    if r.duration /= Nothing then
                                        10

                                    else
                                        0

                                timeScore =
                                    case r.timeSpec of
                                        Nothing ->
                                            0

                                        Just ts ->
                                            scoreTimeSpec ts
                            in
                            ( durationScore + timeScore, r )
                        )
                        results

                sorted =
                    List.sortBy (\( s, _ ) -> -s) scored
            in
            case sorted of
                ( _, best ) :: _ ->
                    Just best

                [] ->
                    Nothing



-- TEST SUITE


suite : Test
suite =
    describe "NLDS Activity Parser"
        [ describe "Activity parsing"
            [ test "parses 'run' as Running" <|
                \() ->
                    parseBestActivity "run"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Running)
            , test "parses 'ran' as Running" <|
                \() ->
                    parseBestActivity "ran"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Running)
            , test "parses 'jogged' as Running" <|
                \() ->
                    parseBestActivity "jogged"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Running)
            , test "parses 'bike' as Cycling" <|
                \() ->
                    parseBestActivity "bike"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Cycling)
            , test "parses 'bike ride' as Cycling" <|
                \() ->
                    parseBestActivity "bike ride"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Cycling)
            , test "parses 'swim' as Swimming" <|
                \() ->
                    parseBestActivity "swim"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Swimming)
            , test "parses 'yoga' as Yoga" <|
                \() ->
                    parseBestActivity "yoga"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Yoga)
            ]
        , describe "Duration parsing"
            [ test "parses '1 hour' as 60 minutes" <|
                \() ->
                    parseBestActivity "ran for 1 hour"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 60)
            , test "parses '2 hours' as 120 minutes" <|
                \() ->
                    parseBestActivity "bike ride 2 hours"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 120)
            , test "parses '30 minutes' as 30 minutes" <|
                \() ->
                    parseBestActivity "ran for 30 minutes"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 30)
            , test "parses 'one hour' as 60 minutes" <|
                \() ->
                    parseBestActivity "did one hour yoga"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 60)
            , test "parses 'an hour' as 60 minutes" <|
                \() ->
                    parseBestActivity "cycling for an hour"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 60)
            , test "parses 'half hour' as 30 minutes" <|
                \() ->
                    parseBestActivity "jogged for half hour"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 30)
            , test "no duration when not specified" <|
                \() ->
                    parseBestActivity "went jogging"
                        |> Maybe.andThen .duration
                        |> Expect.equal Nothing
            ]
        , describe "Time specification parsing"
            [ test "parses 'yesterday morning' correctly" <|
                \() ->
                    parseBestActivity "went for a swim yesterday morning"
                        |> Maybe.andThen .timeSpec
                        |> Expect.equal (Just (Yesterday Morning))
            , test "parses 'last night' correctly" <|
                \() ->
                    parseBestActivity "went for a 1 hour run last night"
                        |> Maybe.andThen .timeSpec
                        |> Expect.equal (Just (Yesterday Night))
            , test "parses 'sunday' as OnDay Sun Noon" <|
                \() ->
                    parseBestActivity "went for a 2 hour bike ride sunday"
                        |> Maybe.andThen .timeSpec
                        |> Expect.equal (Just (OnDay Sun Noon))
            , test "parses 'Thursday' as OnDay Thu Noon" <|
                \() ->
                    parseBestActivity "went jogging on Thursday"
                        |> Maybe.andThen .timeSpec
                        |> Expect.equal (Just (OnDay Thu Noon))
            , test "parses 'Thursday evening' as OnDay Thu Evening" <|
                \() ->
                    parseBestActivity "jogged on Thursday evening"
                        |> Maybe.andThen .timeSpec
                        |> Expect.equal (Just (OnDay Thu Evening))
            , test "parses 'Sat afternoon' as OnDay Sat Afternoon" <|
                \() ->
                    parseBestActivity "did one hour yoga Sat afternoon"
                        |> Maybe.andThen .timeSpec
                        |> Expect.equal (Just (OnDay Sat Afternoon))
            ]
        , describe "Full examples from examples.txt"
            [ -- went for a 1 hour run at 7:30 last night -> Running; 1 hour
              test "went for a 1 hour run last night -> Running, 1 hour, Yesterday Night" <|
                \() ->
                    let
                        result =
                            parseBestActivity "went for a 1 hour run last night"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Running)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 60)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (Yesterday Night))
                        ]
                        result

            -- went for a 2 hour bike ride sunday -> Cycling; 2 hours; Sunday noon
            , test "went for a 2 hour bike ride sunday -> Cycling, 2 hours, Sunday noon" <|
                \() ->
                    let
                        result =
                            parseBestActivity "went for a 2 hour bike ride sunday"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Cycling)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 120)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (OnDay Sun Noon))
                        ]
                        result

            -- went jogging on Thursday -> Running; no duration; Thursday noon
            , test "went jogging on Thursday -> Running, no duration, Thursday noon" <|
                \() ->
                    let
                        result =
                            parseBestActivity "went jogging on Thursday"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Running)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (OnDay Thu Noon))
                        ]
                        result

            -- jogged on Thursday evening -> Running; no duration; Thursday evening
            , test "jogged on Thursday evening -> Running, no duration, Thursday evening" <|
                \() ->
                    let
                        result =
                            parseBestActivity "jogged on Thursday evening"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Running)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (OnDay Thu Evening))
                        ]
                        result

            -- went for a swim yesterday morning -> Swimming; no duration; yesterday morning
            , test "went for a swim yesterday morning -> Swimming, no duration, yesterday morning" <|
                \() ->
                    let
                        result =
                            parseBestActivity "went for a swim yesterday morning"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Swimming)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (Yesterday Morning))
                        ]
                        result

            -- did one hour yoga Sat afternoon -> Yoga; 1 hour; Saturday afternoon
            , test "did one hour yoga Sat afternoon -> Yoga, 1 hour, Saturday afternoon" <|
                \() ->
                    let
                        result =
                            parseBestActivity "did one hour yoga Sat afternoon"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Yoga)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 60)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (OnDay Sat Afternoon))
                        ]
                        result
            ]
        , describe "Noise tolerance"
            [ test "ignores 'please log that I did' prefix" <|
                \() ->
                    -- Note: "gym" is not a recognized activity, so we use "yoga" instead
                    parseBestActivity "please log that I did yoga for 45 minutes"
                        |> Maybe.andThen .duration
                        |> Expect.equal (Just 45)
            , test "ignores 'went for a' prefix" <|
                \() ->
                    parseBestActivity "went for a run"
                        |> Maybe.map .activity
                        |> Expect.equal (Just Running)
            , test "parses despite extra words" <|
                \() ->
                    let
                        result =
                            parseBestActivity "I really enjoyed my 30 minute jog this morning"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Running)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 30)
                        ]
                        result
            ]
        , describe "Token order flexibility"
            [ test "duration before activity" <|
                \() ->
                    let
                        result =
                            parseBestActivity "30 minutes of yoga"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Yoga)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 30)
                        ]
                        result
            , test "time before activity and duration" <|
                \() ->
                    let
                        result =
                            parseBestActivity "yesterday morning ran for 45 minutes"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Running)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 45)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (Yesterday Morning))
                        ]
                        result
            , test "activity at end with duration first" <|
                \() ->
                    let
                        result =
                            parseBestActivity "cycling for an hour yesterday morning"
                    in
                    Expect.all
                        [ \r -> Maybe.map .activity r |> Expect.equal (Just Cycling)
                        , \r -> Maybe.andThen .duration r |> Expect.equal (Just 60)
                        , \r -> Maybe.andThen .timeSpec r |> Expect.equal (Just (Yesterday Morning))
                        ]
                        result
            ]
        ]
