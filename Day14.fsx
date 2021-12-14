#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// ls Day14.fsx | entr -n dotnet fsi Day14.fsx

printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input14.txt"
    |> Seq.toList

let inputString = List.head input

let parseRule line =
    let [|a; b|] = String.split " -> " line
    a, b[0]

let rulesMap =
    input[2..]
    |> List.map parseRule
    |> Map.ofList

let rec tallyOfDepth (n : int) : Map<string,Map<char,int64>> =
    if n = 0 then
        Map.map (fun k _ -> Seq.countBy id k |> Seq.map (Tuple.mapSnd int64) |> Map.ofSeq) rulesMap
    else
        let previousTally = tallyOfDepth (n-1)

        rulesMap
        |> Map.map (
            fun k v ->
                let pair1 = String.Concat([|k[0]; v|])
                let pair2 = String.Concat([|v; k[1]|])
                let count1 = Map.find pair1 previousTally
                let count2 = Map.find pair2 previousTally

                Map.mergeCounts count1 count2
                |> Map.mergeCounts (Map.ofList [v, -1L])
        )

let solve (template : string) depth =
    Seq.windowed 2 template
    |> Seq.map (fun pair ->
        Map.find (String.Concat(pair)) (tallyOfDepth depth)
        |> Map.mergeCounts (Map.ofList [pair[0], -1L]) // make sure to not double count the overlaps
    )
    |> Map.mergeCountsMany
    |> Map.mergeCounts (Map.ofList [template[0], 1L])
    |> Map.toSeq
    |> fun s -> snd (Seq.maxBy snd s) - snd (Seq.minBy snd s)

solve inputString 10
|> printfn "day 14-1: %i"

solve inputString 40
|> printfn "day 14-2: %i"
