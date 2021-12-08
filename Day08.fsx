open System
open System.IO

#nowarn "25"

// live reload with:
// -----------------
// ls Day08.fsx | entr -n dotnet fsi Day08.fsx

let parse (line: string) =
    let [|left; right|] = line.Split(" | ")
    left.Split(" "), right.Split(" ")

let input =
    File.ReadLines "inputs/input8.txt"
    |> Seq.toArray
    |> Array.map parse

// 1 -> 2 segments
// 4 -> 4 segments
// 7 -> 3 segments
// 8 -> 7 segments

let uniqueSegmentCounts =
    Set.ofList [2;4;3;7]

input
|> Array.map (fun (_, outputs) -> Array.filter (fun str -> Set.contains (Seq.length str) uniqueSegmentCounts) outputs |> Array.length)
|> Array.sum
|> printfn "day8-1: %i"

// ----------------------------------------------------------------------------

//   0 -> 6 segments
//^  1 -> 2 segments
//   2 -> 5 segments
//   3 -> 5 segments
//^  4 -> 4 segments
//   5 -> 5 segments
//   6 -> 6 segments
//^  7 -> 3 segments
//^  8 -> 7 segments
//   9 -> 6 segments

// type PatternDictionary = Map<string, int>

let addIfSome (key : 'Key) (value : 'T option) (m: Map<'Key,'T>) : Map<'Key,'T> =
    match value with
    | Some v -> Map.add key v m
    | None -> m

let detectObviousPattern (pattern : string) : int option =
    match Seq.length pattern with
    | 2 -> Some 1
    | 4 -> Some 4
    | 3 -> Some 7
    | 7 -> Some 8
    | _ -> None

let detectObviousPatterns patternDict pattern =
    addIfSome pattern (detectObviousPattern pattern) patternDict

let detectLength5and6 (pattern : string) (inversePatternDict : Map<int, string>) : int option =
    let charset = Set.ofSeq pattern
    let contains n =
        match Map.tryFind n inversePatternDict with
        | None -> false
        | Some charset' -> Set.isSubset (Set.ofSeq charset') charset

    let intersectionSize n =
        Map.tryFind n inversePatternDict
        |> Option.map (Set.ofSeq >> Set.intersect charset >> Set.count)
        |> Option.defaultValue 0

    match Seq.length pattern with
    | 5 ->
        if contains 1 then
            Some 3
        else if intersectionSize 4 = 3 then
            Some 5
        else
            Some 2
    | 6 ->
        if contains 4 then
            Some 9
        else if contains 1 then
            Some 0
        else
            Some 6
    | _ -> None

let reverse (m: Map<'Key,'T>) : Map<'T, 'Key> =
    Map.toList m
    |> List.map (fun (a,b) -> b,a)
    |> Map.ofList

let detectNoneObviousPatterns patternDict pattern =
    addIfSome pattern (detectLength5and6 pattern (reverse patternDict)) patternDict

let deriveDigitMapping measurements =
    let basicPatterns = Array.fold detectObviousPatterns Map.empty measurements
    Array.fold detectNoneObviousPatterns basicPatterns measurements

let solve (measurements, output) =
    let mapping =
        deriveDigitMapping measurements
        |> Map.toList
        |> List.map (fun (k, v) -> Set.ofSeq k, v)
        |> Map.ofList
    Array.map (fun str -> Map.tryFind (Set.ofSeq str) mapping) output
    |> Array.filter Option.isSome
    |> Array.map Option.get
    |> Array.fold (fun str n -> str + sprintf "%i" n) ""
    |> int

input
|> Array.map solve
|> Array.sum
|> printfn "day8-1: %A"
