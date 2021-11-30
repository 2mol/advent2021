module Prev_Day01

open System.IO

let solution1 =
    let entries =
        File.ReadLines "inputs/2020_day1a.txt"
        |> Seq.map int
        |> Set.ofSeq

    entries
    |> Set.filter (fun el -> Set.contains (2020 - el) entries)
    |> Set.fold (*) 1


let solution2 = 0
