module MatrixBench exposing (main)

import Benchmark exposing (benchmark, describe, Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Random
import Random.Float exposing (normal)
import Matrix exposing (randMatrix)
import Matrix exposing (randMatrix)


suite : Benchmark
suite =
    describe "Matrix Operations"
        genBenchmarks -- last run: 116/s optimized



seed2 : Random.Seed
seed2 =
    Random.initialSeed 44


seed3 : Random.Seed
seed3 =
    Random.initialSeed 45


genBenchmarks : List Benchmark
genBenchmarks =
    let
        floatGen =
            normal 0 0.5
        matARes =
            Random.step (randMatrix 784 30 floatGen) seed2

        matBRes =
            Random.step (randMatrix 30 1 floatGen) seed3
    in  
        case matARes of
            (Ok matA, _) ->
                case matBRes of
                    (Ok matB, _) ->
                        [
                            benchmark "multiply 784x30 30x1" <|
                            \_ -> Matrix.mul matA matB
                        ]
                    _ ->
                        []
            _ ->
                []


main : BenchmarkProgram
main =
    program suite