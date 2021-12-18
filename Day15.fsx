#load "Extensions.fsx"
open Extensions

#nowarn "25"

open System
open System.Collections.Generic


// live reload with:
// -----------------
// echo Day15.fsx | entr -n dotnet fsi /_

printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"

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
        for n in 0..4 do
        for m in 0..4 do
        yield!
            [
                for x in 0..xmax do
                for y in 0..ymax do
                let neighbours =
                    getNeighbours x y
                    |> Set.map (fun (node, c) -> (node, addAndWrap c m n))
                yield (x + n * xmax, y + n * ymax) , neighbours
            ]
    ]
    |> dict

let goalNode = (5 * xmax, 5 * ymax)

let initDist : Map<Node, Cost> =
    ((0,0), 0u) :: [for kv in graph do if kv.Key <> (0,0) then yield (kv.Key, UInt32.MaxValue - 10u)]
    |> Map.ofList

let initQueue : Set<Node> = Map.keys initDist |> Set.ofSeq

let manhatten (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let rec djikstra (queue : Set<Node>) (distances : Map<Node, Cost>) (fronteer : Node) : Cost =
    let u =
        queue
        |> Set.toList
        |> List.minBy (fun e -> Map.find e distances)
    // printfn "looking at %A, cost %A" u (Map.find u distances)
    // printfn "fronteer %A" fronteer
    if u = goalNode then
        Map.find u distances
    else
        let neighbours =
            graph[u]
            |> Set.filter (fun (v, _) -> Set.contains v queue)

        let altDistances =
            [
                for (v, cost) in neighbours do
                let alt = Map.find u distances + cost
                if alt < Map.find v distances then
                    // printfn "found a shorter path to %A: %i" v alt
                    yield v, alt
            ] |> Map.ofList

        let newFronteer =
            ( max (fst fronteer) (fst u - 9)
            , max (snd fronteer) (snd u - 9)
            )

        let newQueue =
            Set.remove u queue
            |> Set.filter (fun (x, y) -> x >= fst fronteer && y >= snd fronteer)

        djikstra newQueue (Map.merge distances altDistances) newFronteer


djikstra initQueue initDist (0,0)
|> printfn "%A"
