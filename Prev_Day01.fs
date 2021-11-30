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
    seq {
        for el1 in entries do
        for el2 in entries do
        if Set.contains (2020 - el1 - el2) entries
            then yield el1, el2
    }
    |> Seq.toList
    |> List.head
    |> fun (a, b) -> a * b * (2020 - a - b)
    |> string
