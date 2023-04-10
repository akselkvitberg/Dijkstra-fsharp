open System
open BenchmarkDotNet.Running
open Dijkstra
open dijkstra.Benchmarks

let edges =
    [
        Edge (A,B,4)
        Edge (A,H,8)
        Edge (B,A,4)
        Edge (B,C,8)
        Edge (B,H,11)
        Edge (C,B,8)
        Edge (C,D,7)
        Edge (C,F,4)
        Edge (C,I,2)
        Edge (D,C,7)
        Edge (D,E,9)
        Edge (D,F,14)
        Edge (E,D,9)
        Edge (E,F,10)
        Edge (F,C,4)
        Edge (F,D,14)
        Edge (F,E,10)
        Edge (F,G,2)
        Edge (G,F,2)
        Edge (G,H,1)
        Edge (G,I,6)
        Edge (H,A,8)
        Edge (H,B,11)
        Edge (H,G,1)
        Edge (H,I,7)
        Edge (I,C,2)
        Edge (I,G,6)
        Edge (I,H,7)
    ]
    
let result = findShortestPath edges A E

match result with
| Ok(distance, _, shortestPath) -> printfn $"Distance: {distance}, %A{shortestPath}   "
| Error errorValue -> printfn $"{errorValue}"

BenchmarkRunner.Run<Benchmarks>() |> ignore