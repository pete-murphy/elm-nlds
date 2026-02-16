port module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode
import Json.Encode
import Nld exposing (Nld)
import Regex
import Task
import Time exposing (Month(..), Weekday(..))



-- TYPES


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


type alias DateTime =
    { date : Date
    , time : Time
    }



-- SYNONYMS


activitySynonyms : List ( Activity, List String )
activitySynonyms =
    [ ( Cycling, [ "bike", "biking", "biked", "cycle", "cycling", "cycled", "rode", "riding" ] )
    , ( Running, [ "run", "running", "ran", "jog", "jogging", "jogged" ] )
    , ( Swimming, [ "swim", "swimming", "swam", "laps" ] )
    , ( Yoga, [ "yoga" ] )
    ]



-- PARSERS


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
        |> List.map (\( n, word ) -> Nld.map (\_ -> n) (Nld.word word))
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
            Nld.tokenFilterMap (clockTimeFromString currentTime)

        timeOfDayParser =
            Nld.choice
                [ Nld.map (\_ -> ( 9, Am )) (Nld.word "morning")
                , Nld.map (\_ -> ( 12, Pm )) (Nld.word "noon")
                , Nld.map (\_ -> ( 3, Pm )) (Nld.word "afternoon")
                , Nld.map (\_ -> ( 6, Pm )) (Nld.word "evening")
                , Nld.map (\_ -> ( 9, Pm )) (Nld.word "night")
                ]
                |> Nld.map (\( hour, meridiem ) -> { hour = hour, minute = 0, meridiem = meridiem })
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
        [ -- when before minutes (natural for "swam yesterday for 30 min")
          Nld.map3 ActivityEntry activityParser whenP minutesP

        -- minutes before when (natural for "swam for an hour on Thursday")
        , Nld.map3 (\act mins whn -> ActivityEntry act whn mins) activityParser minutesP whenP
        , Nld.map3 ActivityEntry activityParser whenP (Nld.succeed Nothing)
        , Nld.map3 ActivityEntry activityParser (Nld.succeed Nothing) minutesP
        , Nld.map3 ActivityEntry activityParser (Nld.succeed Nothing) (Nld.succeed Nothing)
        ]


tokenize : String -> List String
tokenize input =
    let
        timeRegex =
            Regex.fromString "\\d{1,2}(:\\d{2})?\\s*(am|pm)"
                |> Maybe.withDefault Regex.never
    in
    input
        |> String.toLower
        |> Regex.replace timeRegex (.match >> String.words >> String.join "")
        |> String.words
        |> List.map (String.filter Char.isAlphaNum)


parseActivity : DateTime -> String -> List ActivityEntry
parseActivity now input =
    let
        tokens =
            tokenize input
    in
    Nld.runTake 20 (activityEntryParser now.time) tokens
        |> Debug.log "parsed"
        |> List.sortBy scoreEntry
        |> List.map (Debug.log "sorted")


{-| Penalize less specific matches.
-}
scoreEntry : ActivityEntry -> Int
scoreEntry entry =
    let
        minutesScore =
            case entry.minutes of
                Just n ->
                    if n == 60 then
                        1

                    else
                        0

                Nothing ->
                    2

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
    minutesScore + whenScore



-- MODEL/UPDATE/VIEW


type alias Flags =
    JsDateTime


type alias JsDateTime =
    { year : Int
    , dayOfYear : Int
    , hour : Int
    , minute : Int
    }


jsDateTimeToDateTime : JsDateTime -> DateTime
jsDateTimeToDateTime jsDateTime =
    let
        date =
            Date.fromOrdinalDate jsDateTime.year jsDateTime.dayOfYear

        time =
            { hour = modBy 12 (jsDateTime.hour + 11) + 1
            , minute = jsDateTime.minute
            , meridiem =
                if jsDateTime.hour < 12 then
                    Am

                else
                    Pm
            }
    in
    { date = date
    , time = time
    }


type alias Model =
    { input : String
    , now : DateTime
    }


type Msg
    = UserChangedInput String
    | UserSubmittedForm String
    | ElmAttemptedToFocusOutput (Result Dom.Error ())
    | JsSentDateTime JsDateTime


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { input = ""
      , now = jsDateTimeToDateTime flags
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserChangedInput newInput ->
            ( { model
                | input = newInput
              }
            , elmRequestedDateTime ()
            )

        UserSubmittedForm input ->
            ( { model
                | input = input
              }
            , Cmd.batch
                [ Dom.focus "activity-output"
                    |> Task.attempt ElmAttemptedToFocusOutput
                , elmRequestedDateTime ()
                ]
            )

        ElmAttemptedToFocusOutput result ->
            let
                _ =
                    Debug.log "attempted to focus output" result
            in
            ( model
            , Cmd.none
            )

        JsSentDateTime currentTime ->
            ( { model
                | now = jsDateTimeToDateTime currentTime
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.header []
            [ Html.h1 [] [ Html.text "Activity Parser" ]
            , Html.p [] [ Html.text "Natural language activity entry" ]
            ]
        , Html.section []
            [ Html.h2 [] [ Html.text "Try it" ]
            , Html.form
                [ Events.preventDefaultOn "submit"
                    (Json.Decode.at [ "target", "elements", "0", "value" ] Json.Decode.string
                        |> Json.Decode.map (\res -> ( UserSubmittedForm res, True ))
                    )
                ]
                [ Html.label [ Attr.for "activity-input" ] [ Html.text "Activity" ]
                , Html.textarea
                    [ Attr.id "activity-input"
                    , Attr.placeholder "e.g., ran for 30 minutes yesterday morning"
                    , Attr.value model.input
                    , Attr.spellcheck False
                    ]
                    []
                , Html.button []
                    [ Html.text "Parse" ]
                ]
            , case parseActivity model.now model.input of
                [] ->
                    Html.text ""

                parsed :: _ ->
                    let
                        when =
                            parsed.when |> Maybe.withDefault (Today Nothing)

                        resolved =
                            resolveDateTime model.now when
                    in
                    Html.output
                        [ Attr.attribute "for" "activity-input"
                        , Attr.id "activity-output"
                        , Attr.tabindex -1
                        ]
                        [ viewActivity parsed.activity
                        , viewMinutes (parsed.minutes |> Maybe.withDefault 30)
                        , viewDateTime resolved
                        ]
            , Html.section []
                [ Html.h2 [] [ Html.text "Examples" ]
                , Html.ul []
                    ([ "ran for 45 minutes yesterday morning"
                     , "just went for a jog"
                     , "swam for an hour on Thursday"
                     , "bike ride, tues 3:30pm"
                     , "yoga 3 pm"
                     , "yesterday at 1 am I went swimming for 30 minutes"
                     ]
                        |> List.map
                            (\example ->
                                Html.li []
                                    [ Html.button
                                        [ Events.onClick (UserChangedInput example) ]
                                        [ Html.text example ]
                                    ]
                            )
                    )
                ]
            ]
        ]


port jsSentDateTime : (JsDateTime -> msg) -> Sub msg


port elmRequestedDateTime : () -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    jsSentDateTime JsSentDateTime


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


viewMinutes : Int -> Html msg
viewMinutes minutes =
    let
        hours =
            minutes // 60

        minutes_ =
            modBy 60 minutes
    in
    Html.node "duration-format"
        [ Attr.property "duration"
            (Json.Encode.object
                [ ( "hours", Json.Encode.int hours )
                , ( "minutes", Json.Encode.int minutes_ )
                ]
            )
        ]
        []


viewDateTime : DateTime -> Html msg
viewDateTime dateTime =
    let
        encoded =
            Json.Encode.object
                [ ( "date", Json.Encode.string (Date.format "MMM d, y" dateTime.date) )
                , ( "time", Json.Encode.string (formatTime dateTime.time) )
                ]
    in
    Html.div []
        [ Html.node "date-time-format" [ Attr.property "dateTime" encoded ] []
        , Html.node "relative-time-format" [ Attr.property "dateTime" encoded ] []
        ]


formatTime : Time -> String
formatTime time =
    String.fromInt time.hour
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt time.minute)
        ++ " "
        ++ (case time.meridiem of
                Am ->
                    "AM"

                Pm ->
                    "PM"
           )


viewActivity : Activity -> Html msg
viewActivity activity =
    let
        text =
            case activity of
                Cycling ->
                    "🚴 Cycling"

                Swimming ->
                    "🏊 Swimming"

                Running ->
                    "🏃 Running"

                Yoga ->
                    "🧘 Yoga"
    in
    Html.div [] [ Html.text text ]



-- UTILS


resolveDateTime :
    DateTime
    -> When
    -> DateTime
resolveDateTime current when =
    let
        defaultTime =
            { hour = 12, minute = 0, meridiem = Pm }
    in
    case when of
        Today (Just time) ->
            { date = current.date
            , time = time
            }

        Today Nothing ->
            current

        DaysAgo days (Just time) ->
            { date = Date.add Date.Days -days current.date
            , time = time
            }

        DaysAgo days Nothing ->
            { date = Date.add Date.Days -days current.date
            , time = defaultTime
            }

        MinutesAgo minutes ->
            { date = current.date
            , time = subtractMinutes minutes current.time
            }

        OnDay weekday (Just time) ->
            let
                delta =
                    modBy 7 (Date.weekdayNumber current.date - Date.weekdayToNumber weekday)
            in
            { date = Date.add Date.Days -delta current.date
            , time = time
            }

        OnDay weekday Nothing ->
            let
                delta =
                    modBy 7 (Date.weekdayNumber current.date - Date.weekdayToNumber weekday)
            in
            { date = Date.add Date.Days -delta current.date
            , time = defaultTime
            }


subtractMinutes : Int -> Time -> Time
subtractMinutes minutes time =
    let
        -- Convert 12-hour to 24-hour
        hour24 =
            case time.meridiem of
                Am ->
                    if time.hour == 12 then
                        0

                    else
                        time.hour

                Pm ->
                    if time.hour == 12 then
                        12

                    else
                        time.hour + 12

        -- Total minutes since midnight, subtract, and wrap to 0..1439
        newTotal =
            modBy (24 * 60) (hour24 * 60 + time.minute - minutes)

        -- Convert back to 12-hour
        newHour24 =
            newTotal // 60

        newMinute =
            modBy 60 newTotal

        newMeridiem =
            if newHour24 < 12 then
                Am

            else
                Pm

        newHour =
            case modBy 12 newHour24 of
                0 ->
                    12

                h ->
                    h
    in
    { hour = newHour, minute = newMinute, meridiem = newMeridiem }


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
                -- Bare numbers like "1" or "3" are too ambiguous
                -- (could be duration, quantity, etc.) — require am/pm suffix
                Nothing

            else
                String.filter Char.isDigit bare
                    |> String.toInt
                    |> Maybe.andThen (\hour -> toTime hour 0)

        _ ->
            Nothing
