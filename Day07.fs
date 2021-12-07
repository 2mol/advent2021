module Day07

open System
open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input7.txt"
    |> Seq.head
    |> fun str -> str.Split(",")
    |> Array.map int

let calcCost (positions : int seq) (target : int) : int =
    Seq.map (fun pos -> abs (pos - target)) positions
    |> Seq.sum

let solution1 =
    Seq.map (calcCost input) [0..1941]
    |> Seq.min
    |> sprintf "%A"


let rec escalatingCost n =
    if n = 0 then 0
    else n + escalatingCost (n - 1)

let calcCost' (positions : int seq) (target : int) : int =
    positions
    |> Seq.map (fun pos -> abs (pos - target))
    |> Seq.map escalatingCost
    |> Seq.sum

let solution2 =
    Seq.map (calcCost' input) [0..1941]
    |> Seq.min
    |> sprintf "%A"
