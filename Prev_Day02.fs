module Prev_Day02

open System
open System.IO

type LineSpec = {
    Min: int
    Max: int
    Letter: Char
    Password: string
}

let splitLine (line : string) =
    let min, rest =
        match line.Split "-" with
        | [| min; rest |] -> min, rest
        | _ -> failwith "fuck"
    let max, charColon, password =
        match rest.Split " " with
        | [| max; charColon; password |] -> max, charColon, password
        | _ -> failwith "fuck"
    {
        Min = (int)min
        Max =  (int)max
        Letter = charColon.Chars(0)
        Password = password //|> Seq.toArray
    }

let isValid (lineSpec : LineSpec) : bool =
    let charCount =
        Seq.countBy id lineSpec.Password
        |> Map.ofSeq
        |> Map.tryFind lineSpec.Letter
        |> Option.defaultValue 0

    charCount >= lineSpec.Min && charCount <= lineSpec.Max

let entries =
    File.ReadLines "inputs/2020_day2.txt"
    |> Seq.map splitLine
    |> List.ofSeq

let solution1 =
    entries
    |> List.filter isValid
    |> List.length
    |> string

let isValid2 (lineSpec : LineSpec) : bool =
    let firstLetter =
        lineSpec.Password[lineSpec.Min - 1] = lineSpec.Letter

    let secondLetter =
        lineSpec.Password[lineSpec.Max - 1] = lineSpec.Letter

    if firstLetter && secondLetter
    then false
    else firstLetter || secondLetter

let solution2 =
    entries
    |> List.filter isValid2
    |> List.length
    |> string
