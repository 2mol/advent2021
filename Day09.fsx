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

let mapMerge m1 m2 =
    List.concat [Map.toList m1; Map.toList m2] |> Map.ofList

// ----------------------------------------------------------------------------

let addChar (i : int) (j : int) (heightMap : Map<int*int, int>) (heightChar : char) : Map<int*int, int> =
    Map.add (i, j) (heightChar.ToString() |> int) heightMap

let ingestLine (i : int) (heightMap : Map<int*int, int>) (line : string) : Map<int*int, int> =
    Array.ofSeq line
    |> foldi (addChar i) heightMap

let ingestLines (lines : string array) : Map<int*int, int> =
    foldi ingestLine Map.empty lines

let findMinima' (map : Map<int*int, int>) : int array =
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
|> findMinima'
|> Array.sum
|> printfn "day9-1: %A"

// ----------------------------------------------------------------------------

let findMinima (map : Map<int*int, int>) : Map<int*int, int> =
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

let rec traverseBasin
    (map : Map<int*int, int>)
    (currentCoords : int*int)
    (currentElement : int)
    (exploredCoords : (int*int) Set)
    (exploredBasin : Map<int*int, int>)
    : Map<int*int, int>
    =
    // let exploredCoords = Map.keys exploredBasin |> Set.ofSeq
    let (x, y) = currentCoords
    let newCoords =
        [ (x+1, y); (x, y+1); (x-1, y); (x, y-1) ]
        |> Set.ofSeq
        |> fun coords -> Set.difference coords exploredCoords
    let nextBasinCells =
        Set.toList newCoords
        |> List.choose (fun coord ->
            match Map.tryFind coord map with
            | Some v -> Some (coord, v)
            | None -> None
        )
        |> List.filter (snd >> (fun el -> el >= currentElement && el < 9))

    // printfn "additional basin cells: %A" nextBasinCells

    let folder basin (nextCoords, nextElement) =
        traverseBasin map nextCoords nextElement (Set.add nextCoords exploredCoords) basin
        |> Map.add nextCoords nextElement
        |> mapMerge basin

    if List.length nextBasinCells = 0 then
        Map.add currentCoords currentElement exploredBasin
    else
        List.fold folder exploredBasin nextBasinCells

let traverseFromLowPoints
    (map : Map<int*int, int>)
    // : Map<int*int, int> seq
    =
    let lowPoints = findMinima map |> Map.toArray // |> Array.take 2
    let basinSize (coords, point) =
        traverseBasin map coords point (Set.singleton coords) (Map.ofList [(coords, point)])
        |> Map.toArray
        |> Array.map snd
        |> Array.length
    Array.map basinSize lowPoints
    |> Array.sort
    |> Array.rev
    |> Array.take 3
    |> Array.fold (*) 1

input
|> ingestLines
|> traverseFromLowPoints
// |> List.map (snd >> (+) 1) |> List.sum
|> printfn "day9-2: %A"
