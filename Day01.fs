module Day01

open System.IO

let entries =
    File.ReadLines "inputs/day1.txt"
    |> Seq.map int
    |> Seq.toList // this is necessary to consume all the lines upfront!

let largerThanPreviousCount list =
    Seq.zip list (Seq.tail list)
    |> Seq.map (fun (a, b) -> a < b)
    |> Seq.filter id
    |> Seq.length

let solution1 =
    largerThanPreviousCount entries
    |> string

let solution2 =
    let depthSums =
        Seq.windowed 3 entries
        |> Seq.map Array.sum

    largerThanPreviousCount depthSums
    |> string
