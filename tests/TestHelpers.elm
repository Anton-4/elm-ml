module TestHelpers exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))


floatEqual : Float -> Float -> Expectation
floatEqual a b =
    Expect.within (Absolute 0.0001) a b


floatEqualPrecise : Float -> Float -> Expectation
floatEqualPrecise a b =
    Expect.within (Absolute 0.0000000001) a b
