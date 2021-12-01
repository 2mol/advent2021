module Day01

open System.IO

let entries =
    File.ReadLines "inputs/day1.txt"
    |> Seq.map int
    |> Seq.toList

let solution1 =
    List.zip entries[.. 1998] (List.tail entries)
    |> List.map (fun (a, b) -> a < b)
    |> List.filter id
    |> List.length
    |> string

// let solution2 =
//     seq {
//         for el1 in entries do
//         for el2 in entries do
//         if Set.contains (2020 - el1 - el2) entries
//             then yield el1, el2
//     }
//     |> Seq.toList
//     |> List.head
//     |> fun (a, b) -> a * b * (2020 - a - b)
//     |> string
