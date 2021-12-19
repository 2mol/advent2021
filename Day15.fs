module Day15

open Extensions

#nowarn "25"

open System
open System.Collections.Generic
open System.Collections


let inputRaw =
    System.IO.File.ReadLines "inputs/input15.txt"
    |> Seq.toArray
    |> Array.map Seq.toArray
    |> Array2D.ofArrays
    |> Array2D.map (Char.GetNumericValue >> uint)

let addAndWrap c m n =
    let sum = c + (uint)m + (uint)n
    if sum <= 9u then sum else sum - 9u

let replicator x y =
    let m = x / Array2D.length1 inputRaw // which offset am I in?
    let n = y / Array2D.length2 inputRaw
    let origx = x % Array2D.length1 inputRaw
    let origy = y % Array2D.length2 inputRaw
    let c = inputRaw[origx, origy]
    addAndWrap c m n

let input =
    Array2D.init
        (5 * Array2D.length1 inputRaw)
        (5 * Array2D.length2 inputRaw)
        replicator


type Node = string
type Cost = uint

let getNeighbours x y =
    let tryGet (i,j) : option<Node * uint>=
        try Some input[i,j] |> Option.map (fun cost -> sprintf "%i-%i" i j, cost)
        with _ -> None

    [ x+1, y; x-1, y; x, y+1; x, y-1 ]
    |> List.choose tryGet
    |> Set.ofList


// ----------------------------------------------------------------------------

let xmax = Array2D.length1 input - 1
let ymax = Array2D.length2 input - 1

let graph : Dictionary<Node, (Node*Cost) Set> =
    [
        for x in 0..xmax do
        for y in 0..ymax do
        let neighbours =
            getNeighbours x y
            // |> Set.map (fun (node, c) -> (node, addAndWrap c m n))
        yield sprintf "%i-%i" x y , neighbours
    ]
    |> dict |> Dictionary

let goalNode = sprintf "%i-%i" xmax ymax

// ----------------------------------------------------------------------------

// let sizeX = Array2D.length1 input
// let sizeY = Array2D.length2 input

// printfn "%i-%i" sizeX sizeY

// let getNeighboursOffset x y m n =
//     let tryGet (i,j) : option<Node * uint>=
//         try
//             Some input[i % sizeX, j % sizeY]
//             |> Option.map (
//                 fun cost ->
//                     sprintf "%i-%i" (i + n * sizeX) (j + m * sizeY)
//                     , addAndWrap cost m n
//                 )
//         with _ -> None

//     [ x+1, y; x-1, y; x, y+1; x, y-1 ]
//     |> List.filter (fun (i, j) -> i + n * sizeX < 5*sizeX && j + m * sizeY < 5*sizeY)
//     |> List.choose tryGet
//     |> Set.ofList


// let xmax = Array2D.length1 input - 1
// let ymax = Array2D.length2 input - 1

// let graph : Dictionary<Node, (Node*Cost) Set> =
//     [
//         for n in 0..4 do
//         for m in 0..4 do
//         yield!
//             [
//                 for x in 0..xmax do
//                 for y in 0..ymax do
//                 let neighbours = getNeighboursOffset x y m n
//                 yield (sprintf "%i-%i" (x + n * Array2D.length1 input) (y + m * Array2D.length2 input)), neighbours
//             ]
//     ]
//     |> dict |> Dictionary

// let goalNode = sprintf "%i-%i" (5*Array2D.length1 input - 1) (5*Array2D.length2 input - 1)

// ----------------------------------------------------------------------------

let initDistanceList : (Node*Cost) list=
    ("0-0", 0u) :: [for kv in graph do if kv.Key <> "0-0" then yield (kv.Key, UInt32.MaxValue - 10u)]

printfn "goal node: %s\nsize: %A" goalNode (List.length initDistanceList)

let manhatten (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let rec djikstra (queue : PriorityQueue<Node, Cost>) (distances : Dictionary<Node, Cost>) : Cost =
    let u = queue.Dequeue()
        // |> List.minBy (fun e -> distances[e])

    // printfn "looking at %A, cost %A" u (distances[u])
    // printfn "fronteer %A" fronteer
    if u = goalNode then
        distances[u]
    else
        // let neighbours =
        //     graph[u]
        //     |> Set.filter (fun (v, _) -> List.contains v queue)

        let _ =
            [
                for (v, cost) in graph[u] do
                // if v
                let alt = distances[u] + cost
                if alt < distances[v] then
                    distances[v] <- alt
                    queue.Enqueue(v, alt)
            ]

        djikstra queue distances


let solution2 =
    while true do
        // let initDist = new Dictionary<Node, Cost>()
        let initDist = initDistanceList |> dict |> Dictionary
        let queue = new PriorityQueue<Node, Cost>()
        // [for (k, v) in initDistanceList do queue.Enqueue(k, v)] |> ignore
        queue.Enqueue("0-0", 0u)
        printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"
        djikstra queue initDist
        |> printfn "%A"
