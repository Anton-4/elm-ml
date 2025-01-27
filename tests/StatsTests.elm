module StatsTests exposing (meanTest, stdTest)

import Array
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Matrix exposing (Vector)
import Stats exposing (mean, std)
import Test exposing (Test, describe, test)
import TestHelpers exposing (floatEqual)


vec1 : Vector
vec1 =
    [ 0, 1, 2, 3 ]


vec2 : Vector
vec2 =
    [ -3, -2, -1, 0, 1, 2, 3 ]


vec3 : Vector
vec3 =
    [ -1, 0, 0.5 ]


vec4 : Vector
vec4 =
    [ 0 ]


meanTest : Test
meanTest =
    describe "mean"
        [ test "vec1" <|
            \_ -> floatEqual (mean vec1) 1.5
        , test "vec2" <|
            \_ -> floatEqual (mean vec2) 0
        , test "vec3" <|
            \_ -> floatEqual (mean vec3) -0.166666
        , test "vec4" <|
            \_ -> floatEqual (mean vec4) 0
        ]


stdTest : Test
stdTest =
    describe "standard deviation"
        [ test "vec1" <|
            \_ -> floatEqual (std vec1) 1.118034
        , test "vec2" <|
            \_ -> floatEqual (std vec2) 2.0
        , test "vec3" <|
            \_ -> floatEqual (std vec3) 0.62361
        , test "vec4" <|
            \_ -> floatEqual (std vec4) 0
        ]
