open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open Dijkstra


let reverseEdges edges =
    edges
    |> List.collect (fun (a,b,w) -> [Edge(a,b,w);Edge(b,a,w)])
    

    
//let shortestPath = findShortestPath edges A B

[<MemoryDiagnoser>]
type Benchmarks() =
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
    
    [<Benchmark>]
    member this.Dijkstra () = [
        findShortestPath edges A B 
        findShortestPath edges A C 
        findShortestPath edges A D 
        findShortestPath edges A E 
        findShortestPath edges A F 
        findShortestPath edges A G 
        findShortestPath edges A H 
        findShortestPath edges A I
        ]
    
//BenchmarkRunner.Run<Benchmarks>() |> ignore

[0..10000]
|> Seq.iter (fun _ -> Benchmarks().Dijkstra () |> ignore)
//let shortestPath = Benchmarks().Dijkstra ()
//printfn $"%A{shortestPath}"