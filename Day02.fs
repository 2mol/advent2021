module Day02

open System.IO

#nowarn "25"

let lineParse (line : string) : (string * int) =
    let [| dir; dist |] = line.Split " "
    (dir, (int)dist)

let input =
    File.ReadLines "inputs/day2.txt"
    |> Seq.toList
    |> List.map lineParse

type Position = {
    Dist: int
    Depth: int
}

let aggregate currentPos (dir, dist) =
    match dir with
    | "forward" -> { currentPos with Dist = currentPos.Dist + dist}
    | "down" -> { currentPos with Depth = currentPos.Depth + dist }
    | "up" -> { currentPos with Depth = currentPos.Depth - dist}
    | _ -> failwith "unrec pos"

let solution1 =
    let pos = List.fold aggregate {Dist = 0; Depth = 0} input
    pos.Dist * pos.Depth
    |> string

let solution2 =
    ""
