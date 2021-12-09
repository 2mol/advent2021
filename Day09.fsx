#nowarn "25"

open System

// live reload with:
// -----------------
// ls Day09.fsx | entr -n dotnet fsi Day09.fsx

let input =
    System.IO.File.ReadLines "inputs/input9.txt"
    |> Seq.toArray

// ----------- ~ Functions that should be in the standard library ~ -----------

let foldi
    (folder: (int -> 'State -> 'T -> 'State))
    (state : 'State)
    (array : 'T array)
    : 'State
    =
    Array.indexed array
    |> Array.fold (fun state (i, a) -> folder i state a) state

// ----------------------------------------------------------------------------

let addChar (i : int) (j : int) (heightMap : Map<int*int, int>) (heightChar : char) : Map<int*int, int> =
    Map.add (i, j) (heightChar.ToString() |> int) heightMap

let ingestLine (i : int) (heightMap : Map<int*int, int>) (line : string) : Map<int*int, int> =
    Array.ofSeq line
    |> foldi (addChar i) heightMap

let ingestLines (lines : string array) : Map<int*int, int> =
    foldi ingestLine Map.empty lines

let findMinima (map : Map<int*int, int>) : int array =
    let isMinimum (i, j) v0 =
        [
            (i+1, j  )
            (i  , j+1)
            (i-1, j  )
            (i  , j-1)
        ]
        |> List.choose (fun k -> Map.tryFind k map)
        |> List.filter (fun v1 -> v1 <= v0)
        |> List.isEmpty

    Map.filter isMinimum map
    |> Map.values
    |> Array.ofSeq
    |> Array.map ((+) 1)

input
|> ingestLines
|> findMinima
|> Array.sum
|> printfn "day9-1: %A"
