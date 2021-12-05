module Day05

open System
open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input5.txt"
    |> Seq.toArray

type Point = {
    X : int
    Y : int
}

type Line = {
    From : Point
    To : Point
}

let parse (line : string) =
    let [|left; right|] = line.Split(" -> ")
    let [|x1; y1|] = left.Split(",") |> Array.map int
    let [|x2; y2|] = right.Split(",") |> Array.map int
    { From = { X=x1; Y=y1 }
      To   = { X=x2; Y=y2 }
    }

let markPointOnMap (ventsMap : Map<Point, int>) (point : Point) : Map<Point, int> =
    match Map.tryFind point ventsMap with
    | Some bla -> Map.add point (bla + 1) ventsMap
    | None -> Map.add point 1 ventsMap

let linePoints (line : Line) : Point seq =
    if line.From.X = line.To.X then
        let minY = min line.From.Y line.To.Y
        let maxY = max line.From.Y line.To.Y
        seq { for y in [minY..maxY] do yield {X=line.From.X; Y=y} }
    else if line.From.Y = line.To.Y then
        let minX = min line.From.X line.To.X
        let maxX = max line.From.X line.To.X
        seq { for x in [minX..maxX] do yield {X=x; Y=line.From.Y} }
    else
        // failwith <| sprintf "the problem description was a lie!\n%A" line
        Seq.empty

let accumulate (ventsMap : Map<Point, int>) (line : Line) : Map<Point, int> =
    let points = linePoints line
    Seq.fold markPointOnMap ventsMap points

let solution1 =
    input
    |> Array.map parse
    |> Array.fold accumulate Map.empty
    |> Map.filter (fun _ n -> n >= 2)
    |> Map.count
    |> sprintf "%A"

// ----------------------------------------------------------------------------

let range (i : int) (j : int) : int list =
    if i < j then [i..j] else [i .. -1 .. j]

let linePoints2 (line : Line) : Point seq =
    if line.From.X = line.To.X then
        seq { for y in range line.From.Y line.To.Y do yield {X=line.From.X; Y=y} }
    else if line.From.Y = line.To.Y then
        seq { for x in range line.From.X line.To.X do yield {X=x; Y=line.From.Y} }
    else
        let xRange = range line.From.X line.To.X
        let yRange = range line.From.Y line.To.Y
        seq {
            for x, y in Seq.zip xRange yRange do
            yield {X=x; Y=y}
        }

let accumulate2 (ventsMap : Map<Point, int>) (line : Line) : Map<Point, int> =
    let points = linePoints2 line
    Seq.fold markPointOnMap ventsMap points

let solution2 =
    input
    |> Array.map parse
    |> Array.fold accumulate2 Map.empty
    |> Map.filter (fun _ n -> n >= 2)
    |> Map.count
    |> sprintf "%A"
