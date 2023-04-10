module dijkstra.DijkstraOld

let findShortestPath edges start destination =
    
    let graph =
        edges
        |> Seq.groupBy (fun (node,_,_) -> node)
        |> Seq.map (fun (sourceNode, edges) -> sourceNode, [for _, toNode, weight in edges do (toNode, weight)] |> List.sortBy snd )
        |> Map.ofSeq
    
    let edgesOf node =
        graph
        |> Map.tryFind node
        |> Option.defaultValue []
    
    let rec visitNextNode unvisited visited =
        let head = unvisited |> Map.toSeq |> Seq.sortBy snd |> Seq.tryHead          // find the node with the shortest distance to the start node 
        match head with
        | None -> visited                                                           // If we don't have any more nodes to process, we have processed the entire reachable graph
        | Some (node, distance) ->
            let visited' = Map.add node distance visited                            // Add the node to the list of visited nodes
            
            let edges =                                                             // Find the edges of this node 
                edgesOf node 
                |> Seq.filter (fst >> visited.ContainsKey >> not)         // Remove visited nodes from edge list
                |> Seq.map (fun (n, weight) -> n, distance + weight)                // Add current distance to weight to find total distance from start node through current node 
            
            let getBestDistance distance =                                          // Function to determine if we have a better path to a current node than previous 
                function
                    | Some value when distance >= value -> Some value               // existing distance is better than the new one
                    | _ -> Some distance                                            // We don't have a path to this node yet, or existing distance is worse than new 
            
            let unvisited' =
                edges
                |> Seq.fold (fun acc (n, d) -> acc |> Map.change n (getBestDistance d)) unvisited // Update unvisited map with potentially new values
                |> Map.remove node                                                                          // Remove the currently processed node
            
            if node = destination then visited'                                     // If we have processed the destination we have found the optimal path
            else visitNextNode unvisited' visited'                                  // If not, we try again untill we find the destination or we don't have any more nodes to process
    
    visitNextNode ([start, 0] |> Map.ofList) Map.empty