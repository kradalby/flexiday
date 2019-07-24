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

estecHolidays2020 : List Date.Date
estecHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 4 13
    , Date.date 2020 4 27
    , Date.date 2020 5 5
    , Date.date 2020 5 21
    , Date.date 2020 6 1
    , Date.date 2020 12 24
    , Date.date 2020 12 25
    , Date.date 2020 12 28
    , Date.date 2020 12 29
    , Date.date 2020 12 30
    , Date.date 2020 12 31
    ]

estecHolidays2019 : List Date.Date
estecHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 4 19
    , Date.date 2019 4 22
    , Date.date 2019 5 30
    , Date.date 2019 6 10
    , Date.date 2019 12 23
    , Date.date 2019 12 24
    , Date.date 2019 12 25
    , Date.date 2019 12 26
    , Date.date 2019 12 27
    , Date.date 2019 12 30
    , Date.date 2019 12 31
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
    estecHolidays2017 ++ estecHolidays2018 ++ estecHolidays2019 ++ estecHolidays2020

hqHolidays2020 : List Date.Date
hqHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 1 2
    , Date.date 2020 1 3
    , Date.date 2020 4 13
    , Date.date 2020 5 1
    , Date.date 2020 5 21
    , Date.date 2020 7 14
    , Date.date 2020 12 25
    , Date.date 2020 12 28
    , Date.date 2020 12 29
    , Date.date 2020 12 30
    , Date.date 2020 12 31
    ]

hqHolidays2019 : List Date.Date
hqHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 4 22
    , Date.date 2019 5 1
    , Date.date 2019 5 30
    , Date.date 2019 6 10
    , Date.date 2019 8 15
    , Date.date 2019 11 1
    , Date.date 2019 12 25
    , Date.date 2019 12 26
    , Date.date 2019 12 27
    , Date.date 2019 12 30
    , Date.date 2019 12 31
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
    hqHolidays2017 ++ hqHolidays2018 ++ hqHolidays2019 ++ hqHolidays2020

esrinHolidays2020 : List Date.Date
esrinHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 1 6
    , Date.date 2020 4 13
    , Date.date 2020 5 1
    , Date.date 2020 6 2
    , Date.date 2020 12 8
    , Date.date 2020 12 24
    , Date.date 2020 12 25
    , Date.date 2020 12 28
    , Date.date 2020 12 29
    , Date.date 2020 12 30
    , Date.date 2020 12 31
    ]


esrinHolidays2019 : List Date.Date
esrinHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 4 22
    , Date.date 2019 4 25
    , Date.date 2019 5 1
    , Date.date 2019 8 15
    , Date.date 2019 12 24
    , Date.date 2019 12 25
    , Date.date 2019 12 26
    , Date.date 2019 12 27
    , Date.date 2019 12 30
    , Date.date 2019 12 31
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
    esrinHolidays2017 ++ esrinHolidays2018 ++ esrinHolidays2019 ++ esrinHolidays2020

esocHolidays2020 : List Date.Date
esocHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 2 24
    , Date.date 2020 4 10
    , Date.date 2020 4 13
    , Date.date 2020 5 1
    , Date.date 2020 5 21
    , Date.date 2020 5 22
    , Date.date 2020 6 1
    , Date.date 2020 6 11
    , Date.date 2020 6 12
    , Date.date 2020 12 25
    , Date.date 2020 12 31
    ]

esocHolidays2019 : List Date.Date
esocHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 4 19
    , Date.date 2019 4 22
    , Date.date 2019 5 1
    , Date.date 2019 5 30
    , Date.date 2019 5 31
    , Date.date 2019 6 10
    , Date.date 2019 6 20
    , Date.date 2019 6 21
    , Date.date 2019 10 3
    , Date.date 2019 12 25
    , Date.date 2019 12 26
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
    esocHolidays2017 ++ esocHolidays2018 ++ esocHolidays2019 ++ esocHolidays2020

esacHolidays2020 : List Date.Date
esacHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 1 6
    , Date.date 2020 4 9
    , Date.date 2020 4 10
    , Date.date 2020 5 1
    , Date.date 2020 5 15
    , Date.date 2020 10 12
    , Date.date 2020 11 2
    , Date.date 2020 12 7
    , Date.date 2020 12 8
    , Date.date 2020 12 24
    , Date.date 2020 12 25
    ]

esacHolidays2019 : List Date.Date
esacHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 1 7
    , Date.date 2019 4 18
    , Date.date 2019 4 19
    , Date.date 2019 5 1
    , Date.date 2019 5 2
    , Date.date 2019 8 15
    , Date.date 2019 11 1
    , Date.date 2019 12 6
    , Date.date 2019 12 9
    , Date.date 2019 12 24
    , Date.date 2019 12 25
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
    esacHolidays2017 ++ esacHolidays2018 ++ esacHolidays2019 ++ esacHolidays2020

ecsatHolidays2020 : List Date.Date
ecsatHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 4 10
    , Date.date 2020 4 13
    , Date.date 2020 5 8
    , Date.date 2020 5 25
    , Date.date 2020 8 31
    , Date.date 2020 12 24
    , Date.date 2020 12 25
    , Date.date 2020 12 28
    , Date.date 2020 12 29
    , Date.date 2020 12 30
    , Date.date 2020 12 31
    ]

ecsatHolidays2019 : List Date.Date
ecsatHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 4 19
    , Date.date 2019 4 22
    , Date.date 2019 5 6
    , Date.date 2019 5 27
    , Date.date 2019 8 26
    , Date.date 2019 12 24
    , Date.date 2019 12 25
    , Date.date 2019 12 26
    , Date.date 2019 12 27
    , Date.date 2019 12 30
    , Date.date 2019 12 31
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
    ecsatHolidays2017 ++ ecsatHolidays2018 ++ ecsatHolidays2019 ++ ecsatHolidays2020

eacHolidays2020 : List Date.Date
eacHolidays2020 =
    [ Date.date 2020 1 1
    , Date.date 2020 4 10
    , Date.date 2020 4 13
    , Date.date 2020 5 1
    , Date.date 2020 5 21
    , Date.date 2020 5 22
    , Date.date 2020 6 1
    , Date.date 2020 6 11
    , Date.date 2020 6 12
    , Date.date 2020 12 24
    , Date.date 2020 12 25
    , Date.date 2020 12 31
    ]

eacHolidays2019 : List Date.Date
eacHolidays2019 =
    [ Date.date 2019 1 1
    , Date.date 2019 4 19
    , Date.date 2019 4 22
    , Date.date 2019 5 1
    , Date.date 2019 5 30
    , Date.date 2019 6 10
    , Date.date 2019 6 20
    , Date.date 2019 10 3
    , Date.date 2019 10 4
    , Date.date 2019 11 1
    , Date.date 2019 12 25
    , Date.date 2019 12 26
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
    eacHolidays2017 ++ eacHolidays2018 ++ eacHolidays2019 ++ eacHolidays2020
