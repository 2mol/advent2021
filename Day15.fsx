#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

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


let graph : Map<Node, (Node*Cost) Set> =
    [
        for x in 0..(Array2D.length1 input - 1) do
        for y in 0..(Array2D.length2 input - 1) do
        yield (x, y) , getNeighbours x y
    ]
    |> Map.ofList

let startNode = (0,0)
let goalNode = (Array2D.length1 input - 1, Array2D.length2 input - 1)

let initDist : Map<Node, Cost> =
    graph
    // hacky bullshit to act as "infinity", but without overflowing
    // in the `dist[bla] + 9` case
    |> Map.mapValues (fun _ -> UInt32.MaxValue - 10u)
    |> Map.add (0,0) 0u

let initQueue : Set<Node> = Map.keys initDist |> Set.ofSeq

let rec djikstra (queue : Set<Node>) (distances : Map<Node, Cost>) =
    let u =
        Set.toList queue
        |> List.minBy (fun e -> Map.find e distances)

    if u = goalNode then
        Map.find u distances
    else
        let neighbours =
            Map.find u graph
            |> Set.filter (fun (v, _) -> Set.contains v queue)

        let altDistances =
            [
                for (v, cost) in neighbours do
                let alt = Map.find u distances + cost
                if alt < Map.find v distances then
                    printfn "found a shorter path to %A: %i" v alt
                    yield v, alt
            ] |> Map.ofList

        djikstra (Set.remove u queue) (Map.merge distances altDistances)


djikstra initQueue initDist
|> printfn "%A"
