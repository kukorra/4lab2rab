open System

type Tree =
    | Node of string * Tree option * Tree option
    | Empty

let rec insert tree value =
    match tree with
    | Empty -> Node(value, None, None)
    | Node(v, left, right) when value < v -> Node(v, Some(insert (defaultArg left Empty) value), right)
    | Node(v, left, right) -> Node(v, left, Some(insert (defaultArg right Empty) value))

let rec collectNodesWithTwoLeaves tree acc =
    match tree with
    | Node(value, Some(Node(_, None, None)), Some(Node(_, None, None))) -> 
        value :: acc
    | Node(_, left, right) ->
        let accWithLeft = Option.fold (fun acc node -> collectNodesWithTwoLeaves node acc) acc left
        let accWithRight = Option.fold (fun acc node -> collectNodesWithTwoLeaves node acc) accWithLeft right
        accWithRight
    | Empty -> acc

let rec printTree tree indent =
    match tree with
    | Empty -> ()
    | Node(value, left, right) ->
        printfn "%s%s" indent value
        let newIndent = indent + "  "
        left |> Option.iter (fun t -> printTree t newIndent)
        right |> Option.iter (fun t -> printTree t newIndent)

[<EntryPoint>]
let main argv =
    let rec inputLoop tree =
        printf "Введите строку (или пустую строку для завершения): "
        let input = Console.ReadLine()
        if String.IsNullOrWhiteSpace input then tree
        else inputLoop (insert tree input)
    
    let tree = inputLoop Empty
    printfn "\nИсходное дерево:"
    printTree tree ""
    
    let nodesWithTwoLeaves = collectNodesWithTwoLeaves tree []
    printfn "\nУзлы с двумя листьями: %A" nodesWithTwoLeaves
    
    0 
