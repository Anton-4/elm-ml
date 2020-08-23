module NeuralTests exposing (denseNetTrain, neuralNetInit)

import Array
import Expect exposing (Expectation)
import Helper exposing (nxt)
import List.Extra exposing (last)
import Matrix
import Neural.Activations exposing (Activation(..))
import Neural.Layers exposing (LayerType(..), dense)
import Neural.Net exposing (NeuralNet, initNet)
import Random
import Test exposing (Test, describe, test)
import TestHelpers exposing (floatEqualPrecise)


seed0 : Random.Seed
seed0 =
    Random.initialSeed 42


denseNet1Res : Result String NeuralNet
denseNet1Res =
    initNet seed0
        [ dense 2 4 Sigmoid
        , dense 4 1 Sigmoid
        ]


denseNet2Res : Result String NeuralNet
denseNet2Res =
    initNet seed0
        [ dense 2 4 Tanh
        , dense 4 1 Tanh
        ]


neuralNetInit : Test
neuralNetInit =
    describe "neural net initialization"
        [ test "denseNet1" <|
            \_ -> Expect.ok denseNet1Res
        ]


testForwardPass : Result String NeuralNet -> Float -> Expectation
testForwardPass netRes expectedOutput =
    case netRes of
        Ok nn ->
            let
                forwardNNRes =
                    Matrix.fromList [ [ 0.1, -0.1 ] ]
                        |> nxt (Neural.Net.forward nn)
            in
            case forwardNNRes of
                Ok forwardNN ->
                    case last forwardNN of
                        Just layerRes ->
                            case layerRes of
                                Ok layer ->
                                    let
                                        calcedOutputMaybe =
                                            Matrix.get 0 0 layer.lastOutput
                                    in
                                    case calcedOutputMaybe of
                                        Just output ->
                                            floatEqualPrecise output expectedOutput

                                        Nothing ->
                                            Expect.fail "failed to get first element of last layer lastOutput"

                                Err e ->
                                    Expect.fail e

                        Nothing ->
                            Expect.fail "failed to get last element of forwardNN layer list"

                Err e ->
                    Expect.fail e

        Err e ->
            Expect.fail e


denseNetTrain : Test
denseNetTrain =
    describe "dense net training"
        [ test "single forward pass sigmoid denseNet1Res" <|
            \_ ->
                testForwardPass denseNet1Res 0.5008823684026859
        , test "single forward pass tanh denseNet2Res" <|
            \_ ->
                testForwardPass denseNet2Res -0.0000724614666
        ]
