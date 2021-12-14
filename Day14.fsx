#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// ls Day14.fsx | entr -n dotnet fsi Day14.fsx


let input =
    System.IO.File.ReadLines "inputs/input14.txt"
    |> Seq.toList

let template = List.head input

let parseRule line =
    let [|a; b|] = String.split " -> " line
    a, b[0]

let rules =
    input[2..]
    |> List.map parseRule
    |> Map.ofList

let insert (str : string) (charPair : char array) =
    match Map.tryFind (String.Concat(charPair)) rules with
    | Some middleChar -> str + String.Concat([|middleChar; charPair[1]|])
    | None -> str + (sprintf "%c" charPair[1])

let iterate (str : string) =
    str
    |> Seq.windowed 2
    |> Seq.fold insert (sprintf "%c" str[0])

applyN 10 iterate template
|> Seq.countBy id
|> Seq.minBy snd
// |> String.length
|> printfn "%A"
