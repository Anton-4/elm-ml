module NeuralTests exposing (denseNetTrain, neuralNetInit)

import Expect exposing (Expectation)
import Helper exposing (nxt)
import List.Extra exposing (last)
import Matrix
import Neural.Activations exposing (Activation(..))
import Neural.Layers exposing (LayerType(..), dense)
import Neural.Net exposing (NeuralNet, backward, forward, initNet, loss, train)
import Random
import String exposing (String)
import Test exposing (Test, describe, test)
import TestHelpers exposing (equalMatrices, expectAll, floatEqualPrecise)


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
                        |> nxt (\inputsMat -> Neural.Net.forward inputsMat nn)
            in
            case forwardNNRes of
                Ok forwardNN ->
                    case last forwardNN of
                        Just layer ->
                            let
                                calcedOutputMaybe =
                                    Matrix.get 0 0 layer.lastOutput
                            in
                            case calcedOutputMaybe of
                                Just output ->
                                    floatEqualPrecise output expectedOutput

                                Nothing ->
                                    Expect.fail "failed to get first element of last layer lastOutput"

                        Nothing ->
                            Expect.fail "failed to get last element of forwardNN layer list"

                Err e ->
                    Expect.fail <| "Forward pass failed: " ++ e

        Err e ->
            Expect.fail e


compareWeights : List (List (List Float)) -> NeuralNet -> Expectation
compareWeights expectedWeightsList net =
    if List.length expectedWeightsList == List.length net then
        let
            expectList =
                List.map2
                    (\expectedWeights layer ->
                        let
                            weightMatixRes =
                                Matrix.fromList expectedWeights
                        in
                        case weightMatixRes of
                            Ok weightMatrix ->
                                equalMatrices weightMatrix layer.weights

                            Err e ->
                                Expect.fail <| "Failed to construct matrix from expectedWeights: " ++ e
                    )
                    expectedWeightsList
                    net
        in
        expectAll expectList

    else
        Expect.fail <|
            "ExpectedWeightsList had length "
                ++ String.fromInt (List.length expectedWeightsList)
                ++ ", but the neural net had "
                ++ String.fromInt (List.length net)
                ++ " layers, both must be equal."


testForwardAndBackward : Result String NeuralNet -> List (List (List Float)) -> Expectation
testForwardAndBackward netRes expectedWeights =
    case netRes of
        Ok nn ->
            let
                inputsMatRes =
                    Matrix.fromList [ [ 0.1, -0.1 ] ]

                labelsMatRes =
                    Matrix.fromList [ [ 0.75 ] ]

                forwardNNRes =
                    inputsMatRes
                        |> nxt (\inputsMat -> Neural.Net.forward inputsMat nn)
            in
            case inputsMatRes of
                Ok inputsMat ->
                    case forwardNNRes of
                        Ok forwardNN ->
                            case labelsMatRes of
                                Ok labelsMat ->
                                    let
                                        backwardNet =
                                            backward inputsMat labelsMat forwardNN
                                    in
                                    case backwardNet of
                                        Ok net ->
                                            compareWeights expectedWeights net

                                        Err e ->
                                            Expect.fail <| "Backward pass failed: " ++ e

                                Err e ->
                                    Expect.fail <| "Failed to construct label matrix: " ++ e

                        Err e ->
                            Expect.fail <| "Forward pass failed: " ++ e

                Err e ->
                    Expect.fail <| "Failed to construct inputs Matrix: " ++ e

        Err e ->
            Expect.fail e


testTrainingReducesLoss : Result String NeuralNet -> Expectation
testTrainingReducesLoss netRes =
    let
        inputsMatRes =
            Matrix.fromList [ [ 0.1, -0.1 ] ]

        labelsMatRes =
            Matrix.fromList [ [ 0.75 ] ]

        -- forwardNNRes =
        --         inputsMatRes
        --         |> nxt (\inputsMat -> Neural.Net.forward inputsMat nn)
        --         |> nxt (\net -> backward )
    in
    case inputsMatRes of
        Ok inputsMat ->
            case netRes of
                Ok net ->
                    case labelsMatRes of
                        Ok labelsMat ->
                            let
                                epochs =
                                    10

                                nnRes =
                                    train inputsMat labelsMat epochs net
                            in
                            case nnRes of
                                Ok trainedNet ->
                                    let
                                        finalLossRes =
                                            loss inputsMat labelsMat trainedNet
                                    in
                                    case finalLossRes of
                                        Ok finalLoss ->
                                            Expect.lessThan 0.01 finalLoss

                                        Err e ->
                                            Expect.fail e

                                Err e ->
                                    Expect.fail <| "Training failed: " ++ e

                        Err e ->
                            Expect.fail <| "Failed to construct label matrix: " ++ e

                Err e ->
                    Expect.fail <| "Forward pass failed: " ++ e

        Err e ->
            Expect.fail <| "Failed to construct inputs Matrix: " ++ e


denseNetTrain : Test
denseNetTrain =
    describe "dense net training"
        [ test "single forward pass sigmoid denseNet1Res" <|
            \_ ->
                testForwardPass denseNet1Res 0.5008823684026859
        , test "single forward pass tanh denseNet2Res" <|
            \_ ->
                testForwardPass denseNet2Res -0.0000724614666

        -- , test "single forward pass + backward pass sigmoid denseNet1Res" <|
        --     \_ ->
        --         testForwardAndBackward denseNet1Res [
        --                                                 [
        --                                                     [0.013001526,0.0162896,-0.0016999,-0.0124315]
        --                                                     ,[-0.0104796,-0.0042441,0.0054476,-0.000631]
        --                                                 ]
        --                                                 ,[
        --                                                     [-0.0744111]
        --                                                     ,[-0.0713704798]
        --                                                     ,[-0.0458169]
        --                                                     ,[-0.050501186]
        --                                                 ]
        --                                             ]
        , test "loss below 0.01 after 10 epochs (denseNet1Res)" <|
            \_ ->
                testTrainingReducesLoss denseNet1Res
        ]
