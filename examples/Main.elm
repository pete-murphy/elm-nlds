module Main exposing (main)

{-| NLDS Activity Parser Example - Ellie-friendly
-}

import Browser
import Date
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Nld exposing (Nld)
import Time exposing (Month(..), Weekday(..))


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


type DaySpec
    = Today
    | Yesterday
    | OnDay Weekday


type TimeSpec
    = Now
    | TimeDetail TimeDetailData


type alias TimeDetailData =
    { day : DaySpec
    , timeOfDay : Maybe TimeOfDay
    , clock : Maybe Clock
    }


type Meridiem
    = AM
    | PM


type alias Clock =
    { hour : Int
    , minute : Int
    , meridiem : Maybe Meridiem
    }


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


activityParser : Nld Activity
activityParser =
    Nld.choice
        (List.map
            (\( activity, syns ) ->
                Nld.map (\_ -> activity) (Nld.words syns)
            )
            activitySynonyms
        )


timeOfDayParser : Nld TimeOfDay
timeOfDayParser =
    Nld.choice
        [ Nld.map (\_ -> Morning) (Nld.words [ "morning", "am" ])
        , Nld.map (\_ -> Afternoon) (Nld.word "afternoon")
        , Nld.map (\_ -> Evening) (Nld.word "evening")
        , Nld.map (\_ -> Night) (Nld.words [ "night", "pm" ])
        , Nld.map (\_ -> Noon) (Nld.word "noon")
        ]


dayOfWeekParser : Nld Weekday
dayOfWeekParser =
    Nld.choice
        [ Nld.map (\_ -> Mon) (Nld.words [ "monday", "mon" ])
        , Nld.map (\_ -> Tue) (Nld.words [ "tuesday", "tue", "tues" ])
        , Nld.map (\_ -> Wed) (Nld.words [ "wednesday", "wed" ])
        , Nld.map (\_ -> Thu) (Nld.words [ "thursday", "thu", "thurs" ])
        , Nld.map (\_ -> Fri) (Nld.words [ "friday", "fri" ])
        , Nld.map (\_ -> Sat) (Nld.words [ "saturday", "sat" ])
        , Nld.map (\_ -> Sun) (Nld.words [ "sunday", "sun" ])
        ]


timeParser : Nld TimeSpec
timeParser =
    Nld.choice
        [ Nld.map (\_ -> Now) (Nld.word "now")
        , Nld.map TimeDetail timeDetailParser
        ]


timeDetailParser : Nld TimeDetailData
timeDetailParser =
    Nld.choice
        [ Nld.map2
            (\( day, tod ) clock ->
                { day = day
                , timeOfDay = Just tod
                , clock = clock
                }
            )
            timeOfDayWithDayParser
            maybeClockParser
        , Nld.map2
            (\day clock ->
                { day = day
                , timeOfDay = Nothing
                , clock = clock
                }
            )
            daySpecParser
            maybeClockParser
        , Nld.map
            (\clock ->
                { day = Today
                , timeOfDay = Nothing
                , clock = Just clock
                }
            )
            clockParser
        ]


daySpecParser : Nld DaySpec
daySpecParser =
    Nld.choice
        [ Nld.map (\_ -> Yesterday) (Nld.word "yesterday")
        , Nld.map OnDay dayOfWeekParser
        ]


timeOfDayWithDayParser : Nld ( DaySpec, TimeOfDay )
timeOfDayWithDayParser =
    Nld.choice
        [ Nld.map2 (\_ _ -> ( Yesterday, Night ))
            (Nld.word "last")
            (Nld.word "night")
        , Nld.map (\_ -> ( Today, Night )) (Nld.word "tonight")
        , Nld.map2 (\_ tod -> ( Yesterday, tod ))
            (Nld.word "yesterday")
            timeOfDayParser
        , Nld.map2 (\_ tod -> ( Today, tod ))
            (Nld.word "this")
            timeOfDayParser
        , Nld.map (\tod -> ( Today, tod ))
            timeOfDayParser
        , Nld.map2
            (\day tod -> ( OnDay day, tod ))
            dayOfWeekParser
            timeOfDayParser
        ]


maybeClockParser : Nld (Maybe Clock)
maybeClockParser =
    Nld.choice
        [ Nld.map Just clockParser
        , Nld.succeed Nothing
        ]


clockParser : Nld Clock
clockParser =
    Nld.choice
        [ Nld.map2
            (\( h, m ) meridiem ->
                { hour = h, minute = m, meridiem = Just meridiem }
            )
            clockTokenParser
            meridiemParser
        , Nld.map2
            (\h meridiem ->
                { hour = h, minute = 0, meridiem = Just meridiem }
            )
            Nld.nat
            meridiemParser
        , Nld.map
            (\( h, m ) ->
                { hour = h, minute = m, meridiem = Nothing }
            )
            clockTokenParser
        ]


clockTokenParser : Nld ( Int, Int )
clockTokenParser =
    Nld.tokenMatching isClockToken
        |> Nld.map (\tok -> parseClockToken tok |> Maybe.withDefault ( 0, 0 ))


meridiemParser : Nld Meridiem
meridiemParser =
    Nld.choice
        [ Nld.map (\_ -> AM) (Nld.word "am")
        , Nld.map (\_ -> PM) (Nld.word "pm")
        ]


isClockToken : String -> Bool
isClockToken tok =
    case parseClockToken tok of
        Just _ ->
            True

        Nothing ->
            False


parseClockToken : String -> Maybe ( Int, Int )
parseClockToken tok =
    case String.split ":" tok of
        [ hourStr, minuteStr ] ->
            case ( String.toInt hourStr, String.toInt minuteStr ) of
                ( Just hour, Just minute ) ->
                    if hour >= 1 && hour <= 12 && minute >= 0 && minute < 60 then
                        Just ( hour, minute )

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


-- Duration parsers


simpleDurationParser : Nld Duration
simpleDurationParser =
    Nld.choice
        [ Nld.map2 (\_ _ -> 30)
            (Nld.word "half")
            (Nld.words [ "hour", "hours", "hr", "hrs" ])
        , Nld.map2
            (\n unit ->
                if List.member unit [ "hour", "hours", "hr", "hrs" ] then
                    n * 60

                else
                    n
            )
            Nld.nat
            (Nld.choice
                [ Nld.words [ "hour", "hours", "hr", "hrs" ]
                , Nld.words [ "minute", "minutes", "min", "mins" ]
                ]
            )
        , Nld.map2 (\_ _ -> 60)
            (Nld.words [ "one", "an" ])
            (Nld.words [ "hour", "hours", "hr", "hrs" ])
        ]


-- Combined parsers


activityWithDuration : Nld ( Activity, Duration )
activityWithDuration =
    Nld.tuple2 activityParser simpleDurationParser


activityWithTime : Nld ( Activity, TimeSpec )
activityWithTime =
    Nld.tuple2 activityParser timeParser


-- Main parsing function


splitTimeTokens : String -> List String
splitTimeTokens tok =
    let
        prefix =
            String.dropRight 2 tok

        isClockPrefix str =
            String.length str > 0
                && String.all (\c -> Char.isDigit c || c == ':') str
                && String.any Char.isDigit str
    in
    if String.endsWith "am" tok && isClockPrefix prefix then
        [ prefix, "am" ]

    else if String.endsWith "pm" tok && isClockPrefix prefix then
        [ prefix, "pm" ]

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

        withAllThree =
            Nld.runTake 5
                (Nld.map2
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

        withDurationOnly =
            Nld.runTake 5
                (Nld.map
                    (\( a, d ) ->
                        { activity = a
                        , duration = Just d
                        , timeSpec = Nothing
                        }
                    )
                    activityWithDuration
                )
                tokens

        withTimeOnly =
            Nld.runTake 5
                (Nld.map
                    (\( a, t ) ->
                        { activity = a
                        , duration = Nothing
                        , timeSpec = Just t
                        }
                    )
                    activityWithTime
                )
                tokens

        activityOnly =
            Nld.runTake 5
                (Nld.map
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


scoreTimeSpec : TimeSpec -> Int
scoreTimeSpec ts =
    case ts of
        Now ->
            2

        TimeDetail detail ->
            let
                dayScore =
                    case detail.day of
                        Today ->
                            0

                        Yesterday ->
                            2

                        OnDay _ ->
                            2

                timeScore =
                    case ( detail.clock, detail.timeOfDay ) of
                        ( Just _, _ ) ->
                            3

                        ( Nothing, Just _ ) ->
                            1

                        ( Nothing, Nothing ) ->
                            0
            in
            dayScore + timeScore


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


-- DISPLAY FUNCTIONS


activityToString : Activity -> String
activityToString activity =
    case activity of
        Cycling ->
            "Cycling"

        Swimming ->
            "Swimming"

        Running ->
            "Running"

        Yoga ->
            "Yoga"


durationToString : Duration -> String
durationToString minutes =
    if minutes < 60 then
        String.fromInt minutes ++ " minutes"

    else if minutes == 60 then
        "1 hour"

    else if modBy 60 minutes == 0 then
        String.fromInt (minutes // 60) ++ " hours"

    else
        String.fromInt (minutes // 60) ++ " hours " ++ String.fromInt (modBy 60 minutes) ++ " minutes"


type alias ClockTime =
    { hour : Int
    , minute : Int
    , meridiem : Meridiem
    }


type alias ResolvedDateTime =
    { date : Date.Date
    , time : ClockTime
    }


bestMatchText : Maybe ParsedActivity -> String
bestMatchText best =
    case best of
        Nothing ->
            "No match yet"

        Just parsed ->
            let
                duration =
                    parsed.duration |> Maybe.withDefault 30

                resolved =
                    resolveDateTime parsed.timeSpec
            in
            activityToString parsed.activity
                ++ "; "
                ++ durationToString duration
                ++ "; "
                ++ formatDateTime resolved


resolveDateTime : Maybe TimeSpec -> ResolvedDateTime
resolveDateTime maybeSpec =
    let
        baseDate =
            Date.fromCalendarDate 2026 Feb 3

        baseTime =
            { hour = 3, minute = 45, meridiem = PM }

        defaultDetail =
            { day = Today, timeOfDay = Just Noon, clock = Nothing }
    in
    case maybeSpec of
        Nothing ->
            { date = baseDate
            , time = clockFromDetail defaultDetail
            }

        Just Now ->
            { date = baseDate
            , time = baseTime
            }

        Just (TimeDetail detail) ->
            { date = resolveDay baseDate detail.day
            , time = clockFromDetail detail
            }


resolveDay : Date.Date -> DaySpec -> Date.Date
resolveDay baseDate daySpec =
    case daySpec of
        Today ->
            baseDate

        Yesterday ->
            Date.add Date.Days -1 baseDate

        OnDay target ->
            let
                baseIndex =
                    Date.weekdayNumber baseDate

                targetIndex =
                    Date.weekdayToNumber target

                delta =
                    modBy 7 (baseIndex - targetIndex)
            in
            Date.add Date.Days (-delta) baseDate


clockFromDetail : TimeDetailData -> ClockTime
clockFromDetail detail =
    case detail.clock of
        Just clock ->
            let
                meridiem =
                    case clock.meridiem of
                        Just m ->
                            m

                        Nothing ->
                            detail.timeOfDay
                                |> Maybe.map meridiemForTimeOfDay
                                |> Maybe.withDefault AM
            in
            { hour = clock.hour, minute = clock.minute, meridiem = meridiem }

        Nothing ->
            detail.timeOfDay
                |> Maybe.withDefault Noon
                |> defaultClockForTimeOfDay


defaultClockForTimeOfDay : TimeOfDay -> ClockTime
defaultClockForTimeOfDay tod =
    case tod of
        Morning ->
            { hour = 9, minute = 0, meridiem = AM }

        Afternoon ->
            { hour = 3, minute = 0, meridiem = PM }

        Evening ->
            { hour = 6, minute = 0, meridiem = PM }

        Night ->
            { hour = 9, minute = 0, meridiem = PM }

        Noon ->
            { hour = 12, minute = 0, meridiem = PM }


meridiemForTimeOfDay : TimeOfDay -> Meridiem
meridiemForTimeOfDay tod =
    case tod of
        Morning ->
            AM

        Afternoon ->
            PM

        Evening ->
            PM

        Night ->
            PM

        Noon ->
            PM


formatDateTime : ResolvedDateTime -> String
formatDateTime resolved =
    Date.format "MMM d, y" resolved.date
        ++ ", "
        ++ formatClock resolved.time
        ++ " ET"


formatClock : ClockTime -> String
formatClock clock =
    let
        minuteString =
            String.fromInt clock.minute |> String.padLeft 2 '0'

        meridiemString =
            case clock.meridiem of
                AM ->
                    "AM"

                PM ->
                    "PM"
    in
    String.fromInt clock.hour ++ ":" ++ minuteString ++ " " ++ meridiemString


-- MODEL/UPDATE/VIEW


type alias Model =
    { input : String
    , best : Maybe ParsedActivity
    }


type Msg
    = InputChanged String


initialModel : Model
initialModel =
    { input = ""
    , best = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newInput ->
            { model
                | input = newInput
                , best = parseBestActivity newInput
            }


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "page" ]
        [ Html.h1 [ Attr.class "title" ] [ Html.text "NLDS Activity Parser" ]
        , Html.p [ Attr.class "subtitle" ] [ Html.text "Type an activity and see the best match." ]
        , Html.div [ Attr.class "field" ]
            [ Html.label [ Attr.class "label", Attr.for "activity-input" ] [ Html.text "Activity" ]
            , Html.input
                [ Attr.id "activity-input"
                , Attr.class "input"
                , Attr.type_ "text"
                , Attr.placeholder "e.g., ran for 30 minutes yesterday morning"
                , Attr.value model.input
                , Events.onInput InputChanged
                ]
                []
            ]
        , Html.div [ Attr.class "result" ]
            [ Html.label [ Attr.class "label", Attr.for "best-output" ] [ Html.text "Best match" ]
            , Html.node "output"
                [ Attr.id "best-output"
                , Attr.class "output"
                , Attr.attribute "for" "activity-input"
                , Attr.attribute "aria-live" "polite"
                ]
                [ Html.text (bestMatchText model.best) ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
