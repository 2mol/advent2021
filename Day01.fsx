open System.IO

let entries =
    File.ReadLines "inputs/day1.txt"
    |> Seq.toList // this is necessary to consume all the lines upfront!
    |> List.map int

let largerThanPreviousCount numbers =
    Seq.zip numbers (Seq.tail numbers)
    |> Seq.map (fun (a, b) -> a < b)
    |> Seq.filter id
    |> Seq.length

largerThanPreviousCount entries
|> printfn "day1-1: %A"

let depthSums =
    Seq.windowed 3 entries
    |> Seq.map Array.sum

largerThanPreviousCount depthSums
|> printfn "day1-2: %A"
