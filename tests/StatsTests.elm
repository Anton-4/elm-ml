module StatsTests exposing (meanTest, stdTest)

import Array
import Expect exposing (FloatingPointTolerance(..), Expectation)
import Matrix exposing (Vector)
import Test exposing (Test, describe, test)
import Stats exposing (mean, std)



vec1 : Vector
vec1 =
    Array.fromList [ 0, 1, 2, 3]


vec2 : Vector
vec2 =
    Array.fromList [ -3, -2, -1, 0, 1, 2, 3]

vec3 : Vector
vec3 =
    Array.fromList [ -1, 0, 0.5]


vec4 : Vector
vec4 =
    Array.fromList [ 0 ]


floatEqual : Float -> Float -> Expectation
floatEqual a b =
    Expect.within (Absolute 0.0001) a b

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
            \_ -> floatEqual (std vec3) 0.623610
        ,test "vec4" <|
            \_ -> floatEqual (std vec4) 0 
        ]
