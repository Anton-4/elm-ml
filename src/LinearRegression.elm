module LinearRegression exposing (..)

import Array exposing (Array)

type alias  LinReg =
    { slope : Float
    , yIntercept : Float
    }

fit : Array -> Array -> LinReg
fit x y =
    LinReg { slope = 0, yIntercept = 0}