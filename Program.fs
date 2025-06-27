open System

type BinaryTree =
    | Empty
    | Node of string * BinaryTree * BinaryTree

let rec insert tree value =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) when value < v -> Node(v, insert left value, right)
    | Node(v, left, right) -> Node(v, left, insert right value)

let rec collectNodesWithTwoLeaves tree acc =
    match tree with
    | Node(value, Node(_, Empty, Empty), Node(_, Empty, Empty)) -> 
        value :: acc
    | Node(_, left, right) ->
        let acc = collectNodesWithTwoLeaves left acc
        collectNodesWithTwoLeaves right acc
    | Empty -> acc

let rec depth tree =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (depth left) (depth right)

let rec printTree tree lCur tCur level =
    match tree with
    | Node(inf, left, right) -> 
        Console.SetCursorPosition(lCur, tCur)
        printf "%s" inf  
        if left <> Empty then 
            Console.SetCursorPosition(lCur - (5 / level), tCur + 1)
            printf "/"
        if right <> Empty then 
            Console.SetCursorPosition(lCur + (inf.Length + 1), tCur + 1)
            printf "\\"
        printTree left (lCur - (12 / level)) (tCur + 2) (level + 1)
        printTree right (lCur + (12 / level)) (tCur + 2) (level + 1)
    | Empty -> ()

let rec inputLoop tree =
    printf "Введите строку (или пустую строку для завершения): "
    let input = Console.ReadLine()
    if String.IsNullOrWhiteSpace input then tree
    else inputLoop (insert tree input)

[<EntryPoint>]
let main argv =
    Console.Clear()
    let tree = inputLoop Empty
    
    printfn "\nИсходное дерево:"
    let currentPos = Console.CursorTop
    let x = Console.WindowWidth / 3
    let y = currentPos + 1
    printTree tree x y 1
    Console.SetCursorPosition(0, y + 2 * depth tree + 2)

    let nodesWithTwoLeaves = collectNodesWithTwoLeaves tree []
    printfn "\nУзлы с двумя листьями: %A" nodesWithTwoLeaves
    
    0
