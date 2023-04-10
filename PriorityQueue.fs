module dijkstra.PriorityQueue

type HeapNode<'T> =
    | Empty
    | Node of value: 'T * left: HeapNode<'T> * right: HeapNode<'T>

let rec private merge<'T when 'T : comparison> (heap1: HeapNode<'T>) (heap2: HeapNode<'T>) : HeapNode<'T> =
    match heap1, heap2 with
    | Empty, _ -> heap2
    | _, Empty -> heap1
    | Node (value1, left1, right1), Node (value2, _, _) when value1 <= value2 ->
        let mergedRight = merge right1 heap2
        Node (value1, mergedRight, left1)
    | _, _ ->
        merge heap2 heap1

let push<'T when 'T : comparison> (value: 'T) (heap: HeapNode<'T>) : HeapNode<'T> =
    merge (Node (value, Empty, Empty)) heap

let peek<'T> (heap: HeapNode<'T>) : 'T option =
    match heap with
    | Empty -> None
    | Node (value, _, _) -> Some value

let pop<'T when 'T : comparison> (heap: HeapNode<'T>) : 'T option * HeapNode<'T> =
    match heap with
    | Empty -> None, Empty
    | Node (value, left, right) -> (Some value), merge left right

let fromList<'T when 'T : comparison> (values: 'T list) : HeapNode<'T> =
    List.fold (fun acc value -> push value acc) Empty values
    
let fromArray<'T when 'T : comparison> (values: 'T array) : HeapNode<'T> =
    Array.fold (fun acc value -> push value acc) Empty values
    
let fromSeq<'T when 'T : comparison> (values: 'T seq) : HeapNode<'T> =
    Seq.fold (fun acc value -> push value acc) Empty values