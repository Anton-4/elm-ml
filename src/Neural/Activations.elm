module Neural.Activations exposing (..)

type Activation =
    Tanh
    | Sigmoid
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


sigmoid : Float -> Float
sigmoid x =
    1/(1 + e^ -x)


sigmoidDer : Float -> Float
sigmoidDer x =
    let
        sigm = sigmoid x
    in
        sigm * (1 - sigm)


sinh : Float -> Float
sinh x =
    (1 - e^(-2*x)) / (2*(e^ -x))

cosh : Float -> Float
cosh x =
    (1 + e^(-2*x)) / (2*(e^ -x))