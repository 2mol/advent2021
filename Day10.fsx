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
|> printfn "%A"
