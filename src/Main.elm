module Main exposing (..)

import Css
import Date as StdDate
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format
import DateParser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (type_, checked, name, disabled, value, class, src, id, selected, for, href, attribute, property)
import Json.Decode exposing (Decoder, int, string, list)
import Json.Encode
import Task
import Time.Date as Date
import DateTimePicker
import DateTimePicker.Config exposing (defaultDatePickerConfig, defaultDateTimePickerConfig, defaultDateI18n)
import DateTimePicker.Css


workDaysBetweenFlexi : Int
workDaysBetweenFlexi =
    5


customDatePattern : String
customDatePattern =
    "%d-%m-%Y"


customInputFormat : DateTimePicker.Config.InputFormat
customInputFormat =
    { inputFormatter = Date.Extra.Format.format config customDatePattern
    , inputParser = DateParser.parse config customDatePattern >> Result.toMaybe
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type LeaveType
    = Flexi
    | Annual
    | Holiday
    | Weekend


type alias LeaveDay =
    { date : Date.Date
    , leaveType : LeaveType
    }


type alias Usage =
    { annualLeave : Int
    , flexiLeave : Int
    , weekendLeave : Int
    , holidayLeave : Int
    , total : Int
    }


type ViewMode
    = Calendar
    | Table


initUsage : Usage
initUsage =
    { annualLeave = 0
    , flexiLeave = 0
    , weekendLeave = 0
    , holidayLeave = 0
    , total = 0
    }


type AlertColor
    = Primary
    | Secondary
    | Success
    | Danger
    | Warning
    | Info
    | Light
    | Dark


type alias Alert =
    { message : List (Html Msg)
    , color : AlertColor
    , onClick : Maybe Msg
    , dismissible : Bool
    }


openingAlert : Alert
openingAlert =
    { message = [ text "This page is not associated with ESA and is 'best effort' only. If you find a fault in the calculator, please give me a notice." ]
    , color = Info
    , onClick = Just DeleteAlert
    , dismissible = True
    }


type alias Model =
    { date : Maybe StdDate.Date
    , startDateValue : Maybe StdDate.Date
    , endDateValue : Maybe StdDate.Date
    , startDatePickerState : DateTimePicker.State
    , endDatePickerState : DateTimePicker.State
    , vacationDays : List LeaveDay
    , usage : Usage
    , viewMode : ViewMode
    , alert : Maybe Alert
    }


init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , startDateValue = Nothing
      , endDateValue = Nothing
      , startDatePickerState = DateTimePicker.initialState
      , endDatePickerState = DateTimePicker.initialState
      , vacationDays = []
      , usage = initUsage
      , viewMode = Table
      , alert = Just openingAlert
      }
    , Cmd.batch
        [ DateTimePicker.initialCmd StartDateChanged DateTimePicker.initialState
        , DateTimePicker.initialCmd EndDateChanged DateTimePicker.initialState
        , Task.perform SetDate StdDate.now
        ]
    )


type Msg
    = NoOp
    | SetDate StdDate.Date
    | StartDateChanged DateTimePicker.State (Maybe StdDate.Date)
    | EndDateChanged DateTimePicker.State (Maybe StdDate.Date)
    | ComputeVacationDays
    | ChangeViewMode ViewMode
    | SetAlert Alert
    | DeleteAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDate date ->
            ( { model | date = Just date }, Cmd.none )

        StartDateChanged state value ->
            { model
                | startDateValue = value
                , startDatePickerState = state
            }
                |> update ComputeVacationDays

        EndDateChanged state value ->
            { model
                | endDateValue = value
                , endDatePickerState = state
            }
                |> update ComputeVacationDays

        ComputeVacationDays ->
            let
                vacationDays =
                    createVacationDaysList model.startDateValue model.endDateValue
            in
                ( { model
                    | vacationDays = vacationDays
                    , usage = (usageStatistics vacationDays)
                  }
                , Cmd.none
                )

        ChangeViewMode mode ->
            ( { model
                | viewMode = mode
              }
            , Cmd.none
            )

        SetAlert alert ->
            ( { model
                | alert = Just alert
              }
            , Cmd.none
            )

        DeleteAlert ->
            ( { model
                | alert = Nothing
              }
            , Cmd.none
            )


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        on "keydown" (Json.Decode.andThen isEnter keyCode)


alertToBootstrapCSS : AlertColor -> String
alertToBootstrapCSS alert =
    case alert of
        Primary ->
            "alert alert-primary"

        Secondary ->
            "alert alert-secondary"

        Success ->
            "alert alert-success"

        Danger ->
            "alert alert-danger"

        Warning ->
            "alert alert-warning"

        Info ->
            "alert alert-info"

        Light ->
            "alert alert-light"

        Dark ->
            "alert alert-dark"


elmMonthToInt : StdDate.Month -> Int
elmMonthToInt month =
    case month of
        StdDate.Jan ->
            1

        StdDate.Feb ->
            2

        StdDate.Mar ->
            3

        StdDate.Apr ->
            4

        StdDate.May ->
            5

        StdDate.Jun ->
            6

        StdDate.Jul ->
            7

        StdDate.Aug ->
            8

        StdDate.Sep ->
            9

        StdDate.Oct ->
            10

        StdDate.Nov ->
            11

        StdDate.Dec ->
            12


dayOfWeekAsIndex : Date.Date -> Int
dayOfWeekAsIndex day =
    case (Date.weekday day) of
        Date.Mon ->
            0

        Date.Tue ->
            1

        Date.Wed ->
            2

        Date.Thu ->
            3

        Date.Fri ->
            4

        Date.Sat ->
            5

        Date.Sun ->
            6


elmDateToTimeDate : StdDate.Date -> Date.Date
elmDateToTimeDate d =
    Date.date (StdDate.year d) (elmMonthToInt <| StdDate.month d) (StdDate.day d)


leaveDay : Date.Date -> LeaveType -> LeaveDay
leaveDay d lt =
    { date = d
    , leaveType = lt
    }


createVacationDaysList : Maybe StdDate.Date -> Maybe StdDate.Date -> List LeaveDay
createVacationDaysList startDate endDate =
    let
        vdays =
            case ( startDate, endDate ) of
                ( Nothing, Nothing ) ->
                    []

                ( Just start, Nothing ) ->
                    [ leaveDay (elmDateToTimeDate start) Flexi ]

                ( Nothing, Just end ) ->
                    [ leaveDay (elmDateToTimeDate end) Flexi ]

                ( Just start, Just end ) ->
                    calculateVacationDays (elmDateToTimeDate start) (elmDateToTimeDate end)
    in
        vdays


datesBetween : Date.Date -> Date.Date -> List Date.Date
datesBetween start end =
    if (Date.compare start end) == LT then
        let
            datesBetweenRec : Date.Date -> Date.Date -> List Date.Date -> List Date.Date
            datesBetweenRec current end result =
                let
                    newCurrent =
                        Date.addDays 1 current
                in
                    if current == (Date.addDays 1 end) then
                        result
                    else
                        datesBetweenRec newCurrent end (current :: result)
        in
            List.reverse <| datesBetweenRec start end []
    else
        []


calculateVacationDays : Date.Date -> Date.Date -> List LeaveDay
calculateVacationDays startDate endDate =
    let
        dates =
            datesBetween startDate endDate

        calculateVacationDaysRec workDaysSinceLastFlexi dates vacationDays =
            case dates of
                [] ->
                    vacationDays

                hd :: tl ->
                    let
                        flexiCounter =
                            if (isEndOfReferencePeriod hd) then
                                5
                            else
                                workDaysSinceLastFlexi
                    in
                        if (isWeekend hd) then
                            calculateVacationDaysRec
                                flexiCounter
                                tl
                            <|
                                (leaveDay hd Weekend)
                                    :: vacationDays
                        else if (isHoliday hd) then
                            calculateVacationDaysRec
                                flexiCounter
                                tl
                            <|
                                (leaveDay hd Holiday)
                                    :: vacationDays
                        else if workDaysSinceLastFlexi == workDaysBetweenFlexi then
                            calculateVacationDaysRec
                                0
                                tl
                            <|
                                (leaveDay hd Flexi)
                                    :: vacationDays
                        else
                            calculateVacationDaysRec
                                (flexiCounter + 1)
                                tl
                            <|
                                (leaveDay hd Annual)
                                    :: vacationDays
    in
        List.reverse (calculateVacationDaysRec 5 dates [])


usageStatistics : List LeaveDay -> Usage
usageStatistics vacationDays =
    let
        vacationTypes =
            List.foldl
                (\elem result ->
                    let
                        { date, leaveType } =
                            elem
                    in
                        leaveType :: result
                )
                []
                vacationDays

        countUsageRec : List LeaveType -> Usage -> Usage
        countUsageRec vacationTypes usage =
            case vacationTypes of
                [] ->
                    usage

                hd :: tl ->
                    case hd of
                        Annual ->
                            countUsageRec tl { usage | annualLeave = usage.annualLeave + 1 }

                        Flexi ->
                            countUsageRec tl { usage | flexiLeave = usage.flexiLeave + 1 }

                        Weekend ->
                            countUsageRec tl { usage | weekendLeave = usage.weekendLeave + 1 }

                        Holiday ->
                            countUsageRec tl { usage | holidayLeave = usage.holidayLeave + 1 }

        usageWithTotal =
            { initUsage | total = List.length vacationDays }
    in
        countUsageRec vacationTypes usageWithTotal


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ viewNav model.viewMode
        , viewMain model
        , viewFooter model.date
        ]


viewNav : ViewMode -> Html Msg
viewNav mode =
    header []
        [ nav [ class "navbar navbar-expand-md navbar-dark bg-dark " ]
            [ a [ class "navbar-brand flexiday-logo", href "#" ]
                [ text "" ]
            , div [ class "ml-auto d-none d-xl-block" ]
                [ viewModeSwitch mode
                ]
            ]
        ]


viewMain : Model -> Html Msg
viewMain model =
    main_ [ class "container-fluid", attribute "role" "main" ]
        [ div [ class "col-sm-12" ]
            [ (case model.alert of
                Nothing ->
                    text ""

                Just alert ->
                    viewAlert alert
              )
            ]
        , div [ class "row" ]
            [ viewDatePicker "start" model.startDatePickerState model.startDateValue
            , viewDatePicker "end" model.endDatePickerState model.endDateValue
            ]
        , div [ class "row mt-3" ]
            [ (case model.viewMode of
                Calendar ->
                    viewVacationDaysCalendar model.vacationDays

                Table ->
                    viewVacationDaysTable model.vacationDays
              )
            , viewUsage model.usage
            ]
        ]


viewFooter : Maybe StdDate.Date -> Html Msg
viewFooter d =
    footer [ class "footer" ]
        [ div [ class "container-fluid" ]
            [ div [ class "row" ]
                [ div [ class "col-sm text-muted" ]
                    [ text "Made with "
                    , a [ href "http://elm-lang.org" ] [ text "Elm" ]
                    ]
                , div [ class "col-sm text-muted " ]
                    [ text
                        ("Copyright "
                            ++ (toString
                                    (case d of
                                        Nothing ->
                                            1337

                                        Just date ->
                                            StdDate.year date
                                    )
                               )
                            ++ " "
                        )
                    , a [ href "https://kradalby.no" ] [ text " Kristoffer Dalby" ]
                    ]
                ]
            ]
        ]


viewModeSwitch : ViewMode -> Html Msg
viewModeSwitch currentMode =
    div [ class "btn-group", attribute "data-toggle" "buttons" ]
        [ label
            [ class
                (case currentMode of
                    Table ->
                        "btn btn-secondary btn-sm active"

                    Calendar ->
                        "btn btn-secondary btn-sm"
                )
            , onClick (ChangeViewMode Table)
            ]
            [ input [ type_ "radio", name "mode", attribute "autocomplete" "off", id "option2" ] []
            , text "Table"
            ]
        , label
            [ class
                (case currentMode of
                    Calendar ->
                        "btn btn-secondary btn-sm active"

                    Table ->
                        "btn btn-secondary btn-sm"
                )
            , onClick (ChangeViewMode Calendar)
            ]
            [ input [ type_ "radio", name "mode", attribute "autocomplete" "off", id "option1" ] []
            , text "Calendar"
            ]
        ]


viewVacationDaysCalendar : List LeaveDay -> Html Msg
viewVacationDaysCalendar vacationDays =
    let
        padWeekStart day =
            List.repeat (dayOfWeekAsIndex day.date) viewEmptyCalendar

        padWeekEnd day =
            List.repeat (abs ((dayOfWeekAsIndex day.date) - 6)) viewEmptyCalendar

        cards vacationDays results =
            case vacationDays of
                [] ->
                    results

                hd :: [ rest ] ->
                    cards [] <| results ++ [ (viewLeaveDayCalendar hd), (viewLeaveDayCalendar rest) ] ++ (padWeekEnd rest)

                hd :: tl ->
                    case results of
                        [] ->
                            cards tl <| (padWeekStart hd) ++ [ (viewLeaveDayCalendar hd) ]

                        _ ->
                            cards tl <| results ++ [ (viewLeaveDayCalendar hd) ]

        rows vacationDaysCalendar results =
            let
                hd =
                    List.take 7 vacationDaysCalendar

                tl =
                    List.drop 7 vacationDaysCalendar
            in
                case ((Debug.log "length" <| List.length vacationDaysCalendar) % 7) of
                    0 ->
                        case vacationDaysCalendar of
                            [] ->
                                results

                            _ ->
                                rows tl <| results ++ [ div [ class "row" ] hd ]

                    _ ->
                        results
    in
        div [ class "col-sm-8" ]
            [ h2 [] [ text "Dates" ]
            , viewAlert
                { message =
                    [ strong [] [ text "NB! " ]
                    , text "Calendar mode is currently not very good, works ish ok on a big screen"
                    ]
                , color = Info
                , onClick = Nothing
                , dismissible = False
                }
            , div [ class "" ]
                [ div [ class "row" ]
                    (rows
                        (cards
                            vacationDays
                            []
                        )
                        []
                    )
                ]
            ]


viewLeaveDayCalendar : LeaveDay -> Html Msg
viewLeaveDayCalendar day =
    div [ class "card", Html.Attributes.style [ ( "min-width", "125px" ) ] ]
        [ div [ class "card-header" ]
            [ text <| (padded (Date.day day.date) ++ "-" ++ padded (Date.month day.date) ++ "-" ++ toString (Date.year day.date))
            ]
        , div [ class "card-body" ]
            [ text <| toString <| Date.weekday day.date
            ]
        , div [ class "card-footer" ]
            [ text <| toString day.leaveType
            ]
        ]


viewEmptyCalendar : Html Msg
viewEmptyCalendar =
    div [ class "card bg-dark", Html.Attributes.style [ ( "min-width", "125px" ) ] ]
        [ div [ class "card-header" ]
            [ text "Work"
            ]
        , div [ class "card-body" ]
            [ text "Work"
            ]
        , div [ class "card-footer" ]
            [ text "Work"
            ]
        ]


viewVacationDaysTable : List LeaveDay -> Html Msg
viewVacationDaysTable vacationDays =
    div [ class "col-sm-8" ]
        [ h2 [] [ text "Dates" ]
        , table [ class "table table-striped table-bordered" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Date" ]
                    , th [] [ text "Type" ]
                    ]
                ]
            , (case vacationDays of
                [] ->
                    text ""

                _ ->
                    tbody [] <| List.map viewLeaveDayTable vacationDays
              )
            ]
        ]


viewLeaveDayTable : LeaveDay -> Html Msg
viewLeaveDayTable leaveDay =
    tr []
        [ td [] [ text <| (padded (Date.day leaveDay.date) ++ "-" ++ padded (Date.month leaveDay.date) ++ "-" ++ toString (Date.year leaveDay.date)) ]
        , td [] [ text <| toString leaveDay.leaveType ]
        ]


viewUsage : Usage -> Html Msg
viewUsage usage =
    div [ class "col-sm-4" ]
        [ h2 [] [ text "Usage" ]
        , table [ class "table table-striped table-bordered" ]
            [ thead []
                [ tr []
                    [ th []
                        [ text "Type" ]
                    , th []
                        [ text "Usage" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td []
                        [ text "Annual" ]
                    , td []
                        [ text <| toString usage.annualLeave ]
                    ]
                , tr []
                    [ td []
                        [ text "Flexi" ]
                    , td []
                        [ text <| toString usage.flexiLeave ]
                    ]
                , tr []
                    [ td []
                        [ text "Weekend" ]
                    , td []
                        [ text <| toString usage.weekendLeave ]
                    ]
                , tr []
                    [ td []
                        [ text "Holiday" ]
                    , td []
                        [ text <| toString usage.holidayLeave ]
                    ]
                , tr [ class "table-dark" ]
                    [ td []
                        [ strong [] [ text "Total" ] ]
                    , td []
                        [ strong [] [ text <| toString usage.total ] ]
                    ]
                ]
            ]
        ]


viewDatePicker : String -> DateTimePicker.State -> Maybe StdDate.Date -> Html Msg
viewDatePicker name state value =
    let
        { css } =
            Css.compile [ DateTimePicker.Css.css ]

        datePickerConfig =
            let
                defaultDateConfig =
                    defaultDatePickerConfig
                        (case name of
                            "start" ->
                                StartDateChanged

                            _ ->
                                EndDateChanged
                        )
            in
                { defaultDateConfig
                    | allowYearNavigation = True
                    , firstDayOfWeek = StdDate.Mon
                    , i18n = { defaultDateI18n | inputFormat = customInputFormat }
                }
    in
        div [ class "col-sm mt-3" ]
            [ h2 [] [ text ("Vacation " ++ name) ]
            , form []
                [ Html.node "style" [] [ Html.text css ]
                , div []
                    [ DateTimePicker.datePickerWithConfig
                        datePickerConfig
                        [ class "form-control", attribute "min" "2017-01-01" ]
                        state
                        value
                    ]
                ]
            ]


viewAlert : Alert -> Html Msg
viewAlert alert =
    let
        cssClasses =
            "ml-auto"
                ++ " "
                ++ (alertToBootstrapCSS alert.color)
                ++ " "
                ++ (case alert.dismissible of
                        True ->
                            "alert-dismissible fade show"

                        False ->
                            ""
                   )

        attr =
            case alert.onClick of
                Nothing ->
                    [ class cssClasses ]

                Just click ->
                    [ class cssClasses, onClick click ]
    in
        div
            attr
        <|
            alert.message
                ++ [ (case alert.dismissible of
                        True ->
                            button [ type_ "button", class "close" ] [ span [ property "innerHTML" (Json.Encode.string "&times;") ] [] ]

                        False ->
                            text ""
                     )
                   ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


isWeekend : Date.Date -> Bool
isWeekend date =
    case Date.weekday date of
        Date.Sat ->
            True

        Date.Sun ->
            True

        _ ->
            False


isHoliday : Date.Date -> Bool
isHoliday date =
    List.member date holidays


isEndOfReferencePeriod : Date.Date -> Bool
isEndOfReferencePeriod date =
    let
        dates =
            [ ( 31, 3 )
            , ( 30, 5 )
            , ( 30, 9 )
            , ( 31, 12 )
            ]

        ( year, month, day ) =
            Date.toTuple date
    in
        List.member ( day, month ) dates


holidays : List Date.Date
holidays =
    estecHolidays2017 ++ estecHolidays2018


estecHolidays2018 : List Date.Date
estecHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 3 30
    , Date.date 2018 4 2
    , Date.date 2018 4 27
    , Date.date 2018 5 10
    , Date.date 2018 5 21
    , Date.date 2018 12 24
    , Date.date 2018 12 25
    , Date.date 2018 12 26
    , Date.date 2018 12 27
    , Date.date 2018 12 28
    , Date.date 2018 12 31
    ]


estecHolidays2017 : List Date.Date
estecHolidays2017 =
    [ Date.date 2017 12 24
    , Date.date 2017 12 25
    , Date.date 2017 12 26
    , Date.date 2017 12 27
    , Date.date 2017 12 28
    , Date.date 2017 12 29
    , Date.date 2017 12 30
    , Date.date 2017 12 31
    , Date.date 2018 1 1
    ]


padded : Int -> String
padded n =
    if n < 10 then
        "0" ++ toString n
    else
        toString n
