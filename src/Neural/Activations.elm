module Neural.Activations exposing (..)

type Activation =
    Tanh
    --Relu

tanh : Float -> Float
tanh x =
    (e^(2*x) - 1) / (e^(2*x) + 1)


tanhDer : Float -> Float
tanhDer x =
    let
        coshSquare = cosh x ^2
    in
        (coshSquare - (sinh x ^2)) / coshSquare


sinh : Float -> Float
sinh x =
    (1 - e^(-2*x)) / (2*(e^ -x))

cosh : Float -> Float
cosh x =
    (1 + e^(-2*x)) / (2*(e^ -x))