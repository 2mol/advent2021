module Prev_Day01

open System.IO

let entries =
    File.ReadLines "inputs/2020_day1.txt"
    |> Seq.map int
    |> Set.ofSeq

let solution1 =
    entries
    |> Set.filter (fun el -> Set.contains (2020 - el) entries)
    |> Set.fold (*) 1
    |> string

let solution2 =
    entries
    |> Set.filter (fun el -> Set.contains (2020 - el) entries)
    // |> Set.fold (*) 1
    |> string
