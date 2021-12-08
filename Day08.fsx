#nowarn "25"

open System

// live reload with:
// -----------------
// ls Day08.fsx | entr -n dotnet fsi Day08.fsx

let parse1 (line: string) =
    let [|left; right|] = line.Split(" | ")
    left.Split(" "), right.Split(" ")

let input =
    System.IO.File.ReadLines "inputs/input8.txt"
    |> Seq.toArray

// 1 -> 2 segments
// 4 -> 4 segments
// 7 -> 3 segments
// 8 -> 7 segments

let uniqueSegmentCounts =
    Set.ofList [2;4;3;7]

input
|> Array.map parse1
|> Array.map (fun (_, outputs) -> Array.filter (fun str -> Set.contains (Seq.length str) uniqueSegmentCounts) outputs |> Array.length)
|> Array.sum
|> printfn "day8-1: %i"

// ----------------------------------------------------------------------------
// ----------- functions that _should_ be in the standard library -------------

let split (sep : string) (str : string) : string array = str.Split(sep)

let reverse (m: Map<'Key,'T>) : Map<'T, 'Key> =
    Map.toList m
    |> List.map (fun (a,b) -> b,a)
    |> Map.ofList

let mapInsertOption (key : 'Key) (value : 'T option) (m: Map<'Key,'T>) : Map<'Key,'T> =
    match value with
    | Some v -> Map.add key v m
    | None -> m

// ----------------------------------------------------------------------------

//   0 -> 6 segments
//*  1 -> 2 segments
//   2 -> 5 segments
//   3 -> 5 segments
//*  4 -> 4 segments
//   5 -> 5 segments
//   6 -> 6 segments
//*  7 -> 3 segments
//*  8 -> 7 segments
//   9 -> 6 segments

// 5 segments -> {2, 3, 5}
// 6 segments -> {0, 6, 9}

let deduceObviousDigit (chars : char Set) : int option =
    match Seq.length chars with
    | 2 -> Some 1
    | 4 -> Some 4
    | 3 -> Some 7
    | 7 -> Some 8
    | _ -> None

let deduceLength5and6 (chars : char Set) (digitToChars : Map<int, char Set>) : int option =
    let intersectionSizeWith n =
        Map.find n digitToChars
        |> Set.intersect chars
        |> Set.count

    match Set.count chars with
    | 5 ->
        if intersectionSizeWith 1 = 2 then
            Some 3
        else if intersectionSizeWith 4 = 3 then
            Some 5
        else
            Some 2
    | 6 ->
        if intersectionSizeWith 4 = 4 then
            Some 9
        else if intersectionSizeWith 1 = 2 then
            Some 0
        else
            Some 6
    | _ -> None

let deduceObviousDigits (charsToDigit : Map<char Set, int>) (chars : char Set) : Map<char Set, int> =
    mapInsertOption chars (deduceObviousDigit chars) charsToDigit

let deduceNonObviousDigits (charsToDigit : Map<char Set, int>) (chars : char Set) : Map<char Set, int> =
    mapInsertOption chars (deduceLength5and6 chars (reverse charsToDigit)) charsToDigit

let deduceAllDigits (measurements : char Set array) : Map<char Set, int> =
    let simpleCharsToDigit = Array.fold deduceObviousDigits Map.empty measurements
    let allCharsToDigit = Array.fold deduceNonObviousDigits simpleCharsToDigit measurements
    allCharsToDigit

let solve (measurements : char Set array, output) : int =
    let mapping = deduceAllDigits measurements
    let digitCharacters = Array.choose (fun chars -> Map.tryFind chars mapping) output
    String.Join("", digitCharacters) |> int

let parse (line: string) : char Set array * char Set array=
    let [|left; right|] =
        split " | " line
        |> Array.map (split " " >> Array.map Set.ofSeq)
    left, right

input
|> Array.map (parse >> solve)
|> Array.sum
|> printfn "day8-2: %i"
