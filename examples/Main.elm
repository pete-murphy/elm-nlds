module Main exposing (main)

{-| NLDs Activity Parser Example - Ellie-friendly
-}

import Browser
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Encode
import Nld exposing (Nld)
import Regex
import Time exposing (Month(..), Weekday(..))



-- TYPES


type alias ActivityEntry =
    { activity : Activity
    , when : When
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


minutesParser : Nld Int
minutesParser =
    let
        anHour =
            Nld.tuple2 (Nld.words [ "an", "a", "one", "1" ]) (Nld.word "hour")

        hours =
            Nld.words [ "hours", "hour", "hrs", "hr" ]

        minutes =
            Nld.words [ "minutes", "minute", "mins", "min" ]

        and =
            Nld.words [ "and", "&" ]

        numberWords =
            [ ( 1, "one" )
            , ( 2, "two" )
            , ( 3, "three" )
            , ( 4, "four" )
            , ( 5, "five" )
            , ( 10, "ten" )
            ]
                |> List.map (\( n, word ) -> Nld.map (\_ -> n) (Nld.word word))
                |> Nld.choice
    in
    Nld.choice
        [ Nld.map3 (\_ _ _ -> 30) (Nld.words [ "a", "an" ]) (Nld.word "half") (Nld.word "hour")
        , Nld.map4 (\_ _ mins _ -> 60 + mins) anHour and Nld.nat minutes
        , Nld.map2 (\mins _ -> mins) Nld.nat minutes
        , Nld.map2 (\hrs _ -> hrs * 60) Nld.nat hours
        , Nld.map4 (\_ _ _ _ -> 90) anHour and (Nld.word "a") (Nld.word "half")
        , Nld.map (\_ -> 60) anHour
        , Nld.map2 (\n _ -> n * 60) numberWords hours
        , Nld.map2 (\n _ -> n) numberWords minutes
        ]


whenParser : Time -> Nld When
whenParser currentTime =
    let
        whenParserHelp parsedTime =
            Nld.choice
                [ Nld.map (\_ -> Today parsedTime) (Nld.word "today")
                , Nld.map3 (\days _ _ -> DaysAgo days parsedTime) Nld.nat (Nld.words [ "days", "day" ]) (Nld.word "ago")
                , Nld.map (\_ -> DaysAgo 1 parsedTime) (Nld.word "yesterday")
                , Nld.map2 (\_ _ -> DaysAgo 1 parsedTime) (Nld.word "last") (Nld.word "night")
                , Nld.map2 (\mins _ -> MinutesAgo mins) minutesParser (Nld.word "ago")
                , Nld.map (\weekday -> OnDay weekday parsedTime) weekdayParser
                ]
    in
    Nld.choice
        [ timeParser currentTime
            |> Nld.andThen (\parsedTime -> whenParserHelp (Just parsedTime))
        , timeParser currentTime
            |> Nld.map (\parsedTime -> Today (Just parsedTime))
        , whenParserHelp Nothing
        , Nld.succeed (Today Nothing)
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
        , Nld.map (\_ -> Wed) (Nld.words [ "wednesday", "wed" ])
        , Nld.map (\_ -> Thu) (Nld.words [ "thursday", "thu", "thurs" ])
        , Nld.map (\_ -> Fri) (Nld.words [ "friday", "fri" ])
        , Nld.map (\_ -> Sat) (Nld.words [ "saturday", "sat" ])
        , Nld.map (\_ -> Sun) (Nld.words [ "sunday", "sun" ])
        ]


clockTimeFromString : Time -> String -> Maybe Time
clockTimeFromString currentTime tok =
    case String.split ":" tok of
        [ hourStr, minuteStr ] ->
            case ( String.toInt hourStr, String.toInt (String.filter Char.isDigit minuteStr) ) of
                ( Just hour, Just minute ) ->
                    let
                        meridiem =
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
                    in
                    if hour >= 1 && hour <= 12 && minute >= 0 && minute < 60 then
                        Just { hour = hour, minute = minute, meridiem = meridiem }

                    else if hour <= 24 && minute >= 0 && minute < 60 then
                        Just { hour = hour - 12, minute = minute, meridiem = Pm }

                    else
                        Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


activityEntryParser : Time -> Nld ActivityEntry
activityEntryParser currentTime =
    Nld.choice
        [ Nld.map3 ActivityEntry activityParser (whenParser currentTime) (Nld.map Just minutesParser)
        , Nld.map3 ActivityEntry activityParser (whenParser currentTime) (Nld.succeed Nothing)
        ]


tokenize : String -> List String
tokenize input =
    let
        timeRegex =
            Regex.fromString "\\d{1,2}(:\\d{2})?\\s*(am|pm)?"
                |> Debug.log "timeRegex"
                |> Maybe.withDefault Regex.never
    in
    input
        |> String.toLower
        |> Regex.replace timeRegex (.match >> String.words >> String.join "")
        |> String.words


parseActivity : Time -> String -> List ActivityEntry
parseActivity currentTime input =
    let
        tokens =
            tokenize input

        _ =
            Debug.log "tokens" tokens
    in
    Nld.runTake 5 (activityEntryParser currentTime) tokens



-- MODEL/UPDATE/VIEW


type alias Flags =
    { year : Int
    , dayOfYear : Int
    , hour : Int
    , minute : Int
    }


type alias Model =
    { input : String
    , now : DateTime
    }


type Msg
    = InputChanged String


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        currentDate =
            Date.fromOrdinalDate flags.year flags.dayOfYear

        currentTime =
            { hour = modBy 12 (flags.hour + 11) + 1
            , minute = flags.minute
            , meridiem =
                if flags.hour < 12 then
                    Am

                else
                    Pm
            }
    in
    ( { input = ""
      , now =
            { date = currentDate
            , time = currentTime
            }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged newInput ->
            ( { model
                | input = newInput
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.main_ []
        [ Html.h1 [] [ Html.text "Nlds Activity Parser" ]
        , Html.section []
            [ Html.header []
                [ Html.h2 [] [ Html.text "Parse from user input" ]
                , Html.p [] [ Html.text "Type an activity and see the best match." ]
                ]
            , Html.div []
                [ Html.label [ Attr.for "activity-input" ] [ Html.text "Activity" ]
                , Html.input
                    [ Attr.id "activity-input"
                    , Attr.type_ "text"
                    , Attr.placeholder "e.g., ran for 30 minutes yesterday morning"
                    , Attr.value model.input
                    , Events.onInput InputChanged
                    ]
                    []
                ]
            , Html.div []
                [ Html.label [ Attr.for "best-output" ] [ Html.text "Best match" ]
                , Html.node "output"
                    [ Attr.id "best-output"
                    , Attr.attribute "for" "activity-input"
                    , Attr.attribute "aria-live" "polite"
                    ]
                    [ viewBestMatch model.now model.input ]
                ]
            ]
        , Html.section []
            [ Html.h2 [] [ Html.text "Examples" ]
            , Html.dl []
                (examples
                    |> List.map
                        (\example ->
                            Html.div []
                                [ Html.dt [] [ Html.text example ]
                                , Html.dd [] [ viewBestMatch model.now example ]
                                ]
                        )
                )
            ]
        ]


examples : List String
examples =
    [ "ran for 30 minutes yesterday morning"
    , "just went for a jog"
    , "swam for an hour on Thursday"
    , "bike ride, tues 3:30pm"
    , "yoga 3 pm"
    ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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
    Html.node "date-time-format"
        [ Attr.property "dateTime"
            (Json.Encode.object
                [ ( "date"
                  , Json.Encode.string (Date.format "MMM d, y" dateTime.date)
                  )
                , ( "time", Json.Encode.string (formatTime dateTime.time) )
                ]
            )
        ]
        []


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
    case activity of
        Cycling ->
            Html.text "🚴 Cycling"

        Swimming ->
            Html.text "🏊 Swimming"

        Running ->
            Html.text "🏃 Running"

        Yoga ->
            Html.text "🧘 Yoga"


viewBestMatch :
    DateTime
    -> String
    -> Html msg
viewBestMatch currentDateTime input =
    case parseActivity currentDateTime.time input of
        [] ->
            Html.text "No match yet"

        parsed :: _ ->
            let
                resolved =
                    resolveDateTime currentDateTime parsed.when
            in
            Html.div []
                [ viewActivity parsed.activity
                , viewMinutes (parsed.minutes |> Maybe.withDefault 30)
                , viewDateTime resolved
                ]


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
