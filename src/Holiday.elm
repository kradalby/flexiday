module Holiday exposing (..)

import Time.Date as Date


type Campus
    = ESTEC
    | HQ
    | ESRIN


campuses : List Campus
campuses =
    [ ESTEC, HQ, ESRIN ]


holidays : Campus -> List Date.Date
holidays campus =
    case campus of
        ESTEC ->
            estecHolidays

        HQ ->
            hqHolidays

        ESRIN ->
            esrinHolidays


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


estecHolidays : List Date.Date
estecHolidays =
    estecHolidays2017 ++ estecHolidays2018


hqHolidays2018 : List Date.Date
hqHolidays2018 =
    []


hqHolidays2017 : List Date.Date
hqHolidays2017 =
    []


hqHolidays : List Date.Date
hqHolidays =
    hqHolidays2017 ++ hqHolidays2018


esrinHolidays2018 : List Date.Date
esrinHolidays2018 =
    []


esrinHolidays2017 : List Date.Date
esrinHolidays2017 =
    []


esrinHolidays : List Date.Date
esrinHolidays =
    esrinHolidays2017 ++ esrinHolidays2018
