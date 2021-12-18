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


let initDist : Map<Node, Cost> =
    graph
    |> Map.mapValues (fun _ -> UInt32.MaxValue)
    |> Map.add (0,0) 0u

let initQueue : Set<Node> = Map.keys initDist |> Set.ofSeq

let rec djikstra (queue : Set<Node>) (dist : Map<Node, Cost>) =
    let u =
        Set.toList queue
        |> List.minBy (fun e -> Map.find e dist)
    let newQueue = Set.remove u queue
    let neighbours =
        Map.find u graph
        |> Set.filter (fun (n, _) -> Set.contains n newQueue)
    ()


initDist
|> printfn "%A"
