#load "Extensions.fsx"

open Extensions

#nowarn "25"

// live reload with:
// -----------------
// ls Day10.fsx | entr -n dotnet fsi Day10.fsx

let input =
    System.IO.File.ReadLines "inputs/input10.txt"
    |> Seq.toArray

let brackets =
    [
        '(', ')'
        '[', ']'
        '{', '}'
        '<', '>'
    ] |> Map.ofList

let bracketPoints =
    [
        ')', 3
        ']', 57
        '}', 1197
        '>', 25137
    ] |> Map.ofList

let isOpeningBracket c =
    Set.contains c <| Set.ofSeq (Map.keys brackets)

let rec findTheSHitBracket expectationStack incomingStack =
    match incomingStack with
    | [] -> None // failwithf "nooo %A" expectationStack
    | c::cs ->
        if isOpeningBracket c then
            let closer = Map.find c brackets
            findTheSHitBracket (closer::expectationStack) cs
        else
            // c is a closing bracket, it should be on top of the expectationStack
            match expectationStack with
            | [] -> Some c
            | x::expect when c = x -> findTheSHitBracket expect cs
            | _ -> Some c

input
|> Array.map List.ofSeq
|> Array.choose (findTheSHitBracket [])
|> Array.map (fun c -> Map.find c bracketPoints)
|> Array.sum
|> printfn "day10-1: %i"


let bracketAutocompletePoints =
    [
        ')', 1UL
        ']', 2UL
        '}', 3UL
        '>', 4UL
    ] |> Map.ofList

let rec findTheClosers expectationStack incomingStack : char list =
    match incomingStack with
    | [] -> expectationStack // failwithf "nooo %A" expectationStack
    | c::cs ->
        if isOpeningBracket c then
            let closer = Map.find c brackets
            findTheClosers (closer::expectationStack) cs
        else
            // c is a closing bracket, it should be on top of the expectationStack
            match expectationStack with
            | [] ->
                // we have a closing bracket, but didn't expect anything? => corrupted!
                failwith "we should have filtered out corrupted lines"
            | x::expect when c = x ->
                // we see exactly the character that we were expecting? Nice, consume both and continue.
                findTheClosers expect cs
            | _ ->
                // we have a closing bracket that mismatches the expected one? => corrupted.
                failwith "we should have filtered out corrupted lines"

let scoreLine chars =
    Seq.map (fun c -> Map.find c bracketAutocompletePoints) chars
    |> Seq.fold (fun accScore charScore -> accScore * 5UL + charScore) 0UL

input
|> Array.map List.ofSeq
|> Array.filter (fun line -> Option.isNone <| findTheSHitBracket [] line)
|> Array.map (findTheClosers [])
|> Array.map scoreLine
|> Array.sort
|> fun a -> a[24]
|> printfn "day10-2: %A"
