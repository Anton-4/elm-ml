module TestHelpers exposing (..)

import Expect exposing (FloatingPointTolerance(..), Expectation, pass, fail)

floatEqual : Float -> Float -> Expectation
floatEqual a b =
    Expect.within (Absolute 0.0001) a b