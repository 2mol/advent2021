module Day15

open Extensions

#nowarn "25"

open System
open System.Collections.Generic
open System.Collections


let input =
    System.IO.File.ReadLines "inputs/input15.txt"
    |> Seq.toArray
    |> Array.map Seq.toArray
    |> Array2D.ofArrays
    |> Array2D.map (Char.GetNumericValue >> uint)


type Node = int*int
type Cost = uint

let getNeighbours x y =
    let tryGet (i,j) : option<Node * uint>=
        try Some input[i,j] |> Option.map (fun cost -> (i,j), cost)
        with _ -> None

    [ x+1, y; x-1, y; x, y+1; x, y-1 ]
    |> List.choose tryGet
    |> Set.ofList

let xmax = Array2D.length1 input - 1
let ymax = Array2D.length2 input - 1

let addAndWrap c m n =
    let sum = c + (uint)m + (uint)n
    if sum <= 9u then sum else sum - 9u

let graph : IDictionary<Node, (Node*Cost) Set> =
    [
        for x in 0..xmax do
        for y in 0..ymax do
        let neighbours =
            getNeighbours x y
            // |> Set.map (fun (node, c) -> (node, addAndWrap c m n))
        yield (x , y ) , neighbours
    ]
    |> dict

let goalNode = (xmax, ymax)

let initDistanceList : (Node*Cost) list=
    ((0,0), 0u) :: [for kv in graph do if kv.Key <> (0,0) then yield (kv.Key, UInt32.MaxValue - 10u)]

let initQueue = List.map fst initDistanceList

let manhatten (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let rec djikstra (queue : Node list) (distances : Dictionary<Node, Cost>) (fronteer : Node) : Cost =
    let u =
        queue
        |> List.minBy (fun e -> distances[e])
    // printfn "looking at %A, cost %A" u (distances[u])
    // printfn "fronteer %A" fronteer
    if u = goalNode then
        distances[u]
    else
        let neighbours =
            graph[u]
            |> Set.filter (fun (v, _) -> List.contains v queue)

        let _ =
            [
                for (v, cost) in neighbours do
                let alt = distances[u] + cost
                if alt < distances[v] then
                    // yield v, alt
                    // distances.Remove(v)
                    distances[v] <- alt
                    // printfn "found a shorter path to %A: %i" v distances[v]
            ]

        let newFronteer =
            ( max (fst fronteer) (fst u - 9)
            , max (snd fronteer) (snd u - 9)
            )

        let newQueue =
            List.filter ((<>) u) queue

        // List.fold (fun () (k,v) -> distances[k] = v |> ignore) () altDistances

        djikstra newQueue distances newFronteer


let solution2 =
    while true do
        // let initDist = new Dictionary<Node, Cost>()
        let initDist = initDistanceList |> dict |> Dictionary
        printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"
        djikstra initQueue initDist (0,0)
        |> printfn "%A"
