module Dijkstra

open dijkstra

type Node = A | B | C | D | E | F | G | H | I
type Edge = Node * Node * int

let findShortestPath (edges:Edge list) (start:Node) (destination:Node) =
    
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
        let head, unvisited = unvisited |> PriorityQueue.pop                                    // find the node with the shortest distance to the start node
        
        match head with
        | None -> visited                                                                       // If we don't have any more nodes to process, we have processed the entire reachable graph
        | Some(_, node, _) when visited |> Map.containsKey node -> visitNextNode unvisited visited // Already visited this node with a shorter distance 
        | Some (distance, node, previousNode) ->
            let unvisited' =                                                                    // Find the edges of this node 
                edgesOf node
                |> Seq.filter (fst >> visited.ContainsKey >> not)                               // Remove visited nodes from edge list
                |> Seq.fold (fun acc (n, d) -> acc |> PriorityQueue.push (d + distance, n, Some node)) unvisited      // Update unvisited map with potentially new values

            let visited' = Map.add node (distance, previousNode) visited                                        // Add the node to the list of visited nodes            
            if node = destination then visited'                                                 // If we have processed the destination we have found the optimal path
            else visitNextNode unvisited' visited'                                              // If not, we try again untill we find the destination or we don't have any more nodes to process
    
    let visitedNodes = visitNextNode ([0, start, None] |> PriorityQueue.fromList) Map.empty
    
    let rec retractSteps node =
        seq {
            match Map.tryFind node visitedNodes with
            | None -> ()
            | Some (_, Some prevNode) ->
                node
                yield! retractSteps prevNode
            | _ ->
                node
        }
        
    match Map.tryFind destination visitedNodes with
    | None -> Error $"No path from {start} to {destination}"
    | Some (distance, _) ->
        Ok (distance, visitedNodes, retractSteps destination |> Seq.rev |> Seq.toList)