module dijkstra.Benchmarks

open BenchmarkDotNet.Attributes
open Dijkstra

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
        
    
    let getPermutations (list: 'T list) : ('T * 'T) list =
        let rec loop items acc =
            match items with
            | [] -> acc
            | x :: xs ->
                let pairs = List.map (fun y -> (x, y)) xs
                loop xs (pairs @ acc)
        loop list []

    let permutations =
        [A;B;C;D;E;F;G;H;I]
        |> getPermutations
    
    [<Benchmark>]
    member this.Dijkstra () = [
        permutations
        |> List.map (fun (start, stop) -> findShortestPath edges start stop)
        ]
    
    [<Benchmark>]
    member this.DijkstraOld () = [
        permutations
        |> List.map (fun (start, stop) -> DijkstraOld.findShortestPath edges start stop)
    ]