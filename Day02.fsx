#nowarn "25"
open System.IO

// "parsing", lol
let lineParse (line : string) : (string * int) =
    let [| dir; dist |] = line.Split " "
    (dir, (int)dist)

let input =
    File.ReadLines "inputs/day2.txt"
    |> Seq.toList
    |> List.map lineParse

type Position = { Dist: int; Depth: int }

let aggregate currentPos (dir, dist) =
    match dir with
    | "forward" -> { currentPos with Dist = currentPos.Dist + dist}
    | "down" -> { currentPos with Depth = currentPos.Depth + dist }
    | "up" -> { currentPos with Depth = currentPos.Depth - dist}

let pos1 = List.fold aggregate {Dist = 0; Depth = 0} input

pos1.Dist * pos1.Depth
|> printfn "day2-1: %A"

type PositionAndAim = {
    Dist: int
    Depth: int
    Aim: int
}

let aggregate2 (currentPos : PositionAndAim) (dir, aimOrDist) =
    match dir with
    | "forward" ->
        { currentPos with
            Dist = currentPos.Dist + aimOrDist
            Depth = currentPos.Depth + (currentPos.Aim * aimOrDist)
        }
    | "down" -> { currentPos with Aim = currentPos.Aim + aimOrDist }
    | "up" -> { currentPos with Aim = currentPos.Aim - aimOrDist}

let pos2 = List.fold aggregate2 {Dist = 0; Depth = 0; Aim = 0} input

pos2.Dist * pos2.Depth
|> printfn "day2-2: %A"
