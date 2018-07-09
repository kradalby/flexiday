module Holiday exposing (..)

import Time.Date as Date


type Campus
    = ESTEC
    | HQ
    | ESRIN
    | ESOC
    | ESAC
    | ECSAT
    | EAC


campuses : List Campus
campuses =
    [ ESTEC, HQ, ESRIN, ESOC, ESAC, ECSAT, EAC ]


holidays : Campus -> List Date.Date
holidays campus =
    case campus of
        ESTEC ->
            estecHolidays

        HQ ->
            hqHolidays

        ESRIN ->
            esrinHolidays

        ESOC ->
            esocHolidays

        ESAC ->
            esacHolidays

        ECSAT ->
            ecsatHolidays

        EAC ->
            eacHolidays


estecHolidays2019 : List Date.Date
estecHolidays2019 =
    [ Date.date 2019 1 1
    ]


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
    estecHolidays2017 ++ estecHolidays2018 ++ estecHolidays2019


hqHolidays2019 : List Date.Date
hqHolidays2019 =
    [ Date.date 2019 1 1
    ]


hqHolidays2018 : List Date.Date
hqHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 4 2
    , Date.date 2018 5 1
    , Date.date 2018 5 10
    , Date.date 2018 5 21
    , Date.date 2018 11 1
    , Date.date 2018 12 24
    , Date.date 2018 12 25
    , Date.date 2018 12 26
    , Date.date 2018 12 27
    , Date.date 2018 12 28
    , Date.date 2018 12 31
    ]


hqHolidays2017 : List Date.Date
hqHolidays2017 =
    [ Date.date 2017 12 25
    , Date.date 2017 12 26
    , Date.date 2017 12 27
    , Date.date 2017 12 28
    , Date.date 2017 12 29
    ]


hqHolidays : List Date.Date
hqHolidays =
    hqHolidays2017 ++ hqHolidays2018 ++ hqHolidays2019


esrinHolidays2019 : List Date.Date
esrinHolidays2019 =
    [ Date.date 2019 1 1
    ]


esrinHolidays2018 : List Date.Date
esrinHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 4 2
    , Date.date 2018 4 25
    , Date.date 2018 5 1
    , Date.date 2018 8 15
    , Date.date 2018 11 1
    , Date.date 2018 12 24
    , Date.date 2018 12 25
    , Date.date 2018 12 26
    , Date.date 2018 12 27
    , Date.date 2018 12 28
    , Date.date 2018 12 31
    ]


esrinHolidays2017 : List Date.Date
esrinHolidays2017 =
    [ Date.date 2017 12 25
    , Date.date 2017 12 26
    , Date.date 2017 12 27
    , Date.date 2017 12 28
    , Date.date 2017 12 29
    ]


esrinHolidays : List Date.Date
esrinHolidays =
    esrinHolidays2017 ++ esrinHolidays2018 ++ esrinHolidays2019


esocHolidays2019 : List Date.Date
esocHolidays2019 =
    [ Date.date 2019 1 1
    ]


esocHolidays2018 : List Date.Date
esocHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 3 30
    , Date.date 2018 4 2
    , Date.date 2018 5 1
    , Date.date 2018 5 10
    , Date.date 2018 5 11
    , Date.date 2018 5 21
    , Date.date 2018 5 31
    , Date.date 2018 10 3
    , Date.date 2018 12 25
    , Date.date 2018 12 26
    ]


esocHolidays2017 : List Date.Date
esocHolidays2017 =
    [ Date.date 2017 12 25
    , Date.date 2017 12 26
    ]


esocHolidays : List Date.Date
esocHolidays =
    esocHolidays2017 ++ esocHolidays2018 ++ esocHolidays2019


esacHolidays2019 : List Date.Date
esacHolidays2019 =
    [ Date.date 2019 1 1
    ]


esacHolidays2018 : List Date.Date
esacHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 3 19
    , Date.date 2018 3 29
    , Date.date 2018 3 30
    , Date.date 2018 5 1
    , Date.date 2018 5 2
    , Date.date 2018 8 15
    , Date.date 2018 10 12
    , Date.date 2018 11 1
    , Date.date 2018 12 6
    , Date.date 2018 12 24
    , Date.date 2018 12 25
    ]


esacHolidays2017 : List Date.Date
esacHolidays2017 =
    [ Date.date 2017 12 6
    , Date.date 2017 12 8
    , Date.date 2017 12 25
    ]


esacHolidays : List Date.Date
esacHolidays =
    esacHolidays2017 ++ esacHolidays2018 ++ esacHolidays2019


ecsatHolidays2019 : List Date.Date
ecsatHolidays2019 =
    [ Date.date 2019 1 1
    ]


ecsatHolidays2018 : List Date.Date
ecsatHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 3 30
    , Date.date 2018 4 2
    , Date.date 2018 5 7
    , Date.date 2018 5 28
    , Date.date 2018 8 27
    , Date.date 2018 12 24
    , Date.date 2018 12 25
    , Date.date 2018 12 26
    , Date.date 2018 12 27
    , Date.date 2018 12 28
    , Date.date 2018 12 31
    ]


ecsatHolidays2017 : List Date.Date
ecsatHolidays2017 =
    [ Date.date 2017 12 25
    , Date.date 2017 12 26
    , Date.date 2017 12 27
    , Date.date 2017 12 28
    , Date.date 2017 12 29
    ]


ecsatHolidays : List Date.Date
ecsatHolidays =
    ecsatHolidays2017 ++ ecsatHolidays2018 ++ ecsatHolidays2019


eacHolidays2019 : List Date.Date
eacHolidays2019 =
    [ Date.date 2019 1 1
    ]


eacHolidays2018 : List Date.Date
eacHolidays2018 =
    [ Date.date 2018 1 1
    , Date.date 2018 3 30
    , Date.date 2018 4 2
    , Date.date 2018 5 1
    , Date.date 2018 5 10
    , Date.date 2018 5 21
    , Date.date 2018 5 31
    , Date.date 2018 12 24
    , Date.date 2018 10 3
    , Date.date 2018 11 1
    , Date.date 2018 11 2
    , Date.date 2018 12 25
    ]


eacHolidays2017 : List Date.Date
eacHolidays2017 =
    [ Date.date 2017 12 25
    , Date.date 2017 12 26
    ]


eacHolidays : List Date.Date
eacHolidays =
    eacHolidays2017 ++ eacHolidays2018 ++ eacHolidays2019
