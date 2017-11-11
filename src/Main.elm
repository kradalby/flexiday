module Main exposing (..)

import Css
import Date as StdDate
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (type_, checked, name, disabled, value, class, src, id, selected, for, href)
import Json.Decode exposing (Decoder, int, string, list)
import Task
import Time.Date as Date
import DateTimePicker
import DateTimePicker.Config exposing (defaultDatePickerConfig, defaultDateTimePickerConfig, defaultDateI18n)
import DateTimePicker.Css


-- i18n : DateTimePicker.Config.I18n
-- i18n =
--     { defaultDateI18n
--         | inputFormat = "%d/%m/%Y"
--     }
-- dateTimePickerConfig =
--     { defaultDatePickerConfig
--         | firstDayOfWeek = Mon
--         , allowYearNavigation = True
--         , i18n = i18n
--     }


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


type alias LeaveDay =
    { date : Date.Date
    , leaveType : LeaveType
    }


type alias Model =
    { date : Maybe StdDate.Date
    , startDateValue : Maybe StdDate.Date
    , endDateValue : Maybe StdDate.Date
    , startDatePickerState : DateTimePicker.State
    , endDatePickerState : DateTimePicker.State
    , vacationDays : List LeaveDay
    }


init : ( Model, Cmd Msg )
init =
    ( { date = Nothing
      , startDateValue = Nothing
      , endDateValue = Nothing
      , startDatePickerState = DateTimePicker.initialState
      , endDatePickerState = DateTimePicker.initialState
      , vacationDays = []
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDate date ->
            ( { model | date = Just date }, Cmd.none )

        StartDateChanged state value ->
            ( { model
                | startDateValue = value
                , startDatePickerState = state
                , vacationDays = createVacationDaysList value model.endDateValue
              }
            , Cmd.none
            )

        EndDateChanged state value ->
            ( { model
                | endDateValue = value
                , endDatePickerState = state
                , vacationDays = createVacationDaysList model.startDateValue value
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
                    if current == end then
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
    in
        List.map (\d -> leaveDay d Flexi) dates


view : Model -> Html Msg
view model =
    div [ class "" ]
        [ div [ class "container" ]
            [ h1 [] [ text "Flexidays" ]
            , viewDatePicker "Start" model.startDatePickerState model.startDateValue
            , viewDatePicker "End" model.endDatePickerState model.endDateValue
            , viewVacationDays model.vacationDays
            ]
        , viewFooter model.date
        ]


viewVacationDays : List LeaveDay -> Html Msg
viewVacationDays vacationDays =
    div [ class "" ]
        [ case vacationDays of
            [] ->
                text "Select start and end dates"

            _ ->
                table [] <|
                    [ tr []
                        [ th [] [ text "Date" ]
                        , th [] [ text "Type" ]
                        ]
                    ]
                        ++ List.map viewLeaveDay vacationDays
        ]


viewLeaveDay : LeaveDay -> Html Msg
viewLeaveDay leaveDay =
    tr []
        [ td [] [ text <| (Date.toISO8601 leaveDay.date) ]
        , td [] [ text <| (toString leaveDay.leaveType) ]
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
                            "Start" ->
                                StartDateChanged

                            "End" ->
                                EndDateChanged

                            _ ->
                                StartDateChanged
                        )
            in
                { defaultDateConfig | allowYearNavigation = True }
    in
        form []
            [ Html.node "style" [] [ Html.text css ]
            , div [ class "container" ]
                [ p
                    []
                    [ label []
                        [ text (name ++ "date Picker: ")
                        , DateTimePicker.datePickerWithConfig
                            datePickerConfig
                            []
                            state
                            value
                        ]
                    ]
                ]
            ]


viewFooter : Maybe StdDate.Date -> Html Msg
viewFooter d =
    footer [ class "footer" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-sm text-muted" ] [ text "Made with ", a [ href "http://elm-lang.org" ] [ text "Elm" ] ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


estecHolidays : List Date.Date
estecHolidays =
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


type Workday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri


type Weekend
    = Sat
    | Sun
