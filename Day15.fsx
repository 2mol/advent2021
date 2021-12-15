#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// echo Day15.fsx | entr -n dotnet fsi /_

printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input15.txt"
    |> Seq.toArray

input
|> printfn "%A"
