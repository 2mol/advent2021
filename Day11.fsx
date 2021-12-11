#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// ls Day11.fsx | entr -n dotnet fsi Day11.fsx

printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"
// printfn "hulllo we are hele in \u001b[1mHello World!\u001b[0m so that pinrintrubwbias"
// printfn "hulllo we are hele in \u001b[7mHello World!\u001b[0m so that pinrintrubwbias"
// printfn "hulllo we are hele in \u001b[2mHello World!\u001b[0m so that pinrintrubwbias"

let input =
    System.IO.File.ReadLines "inputs/input11.txt"
    |> Seq.toArray

let addChar (i : int) (j : int) (heightMap : Map<int*int, int>) (heightChar : char) : Map<int*int, int> =
    Map.add (i, j) (heightChar.ToString() |> int) heightMap

let ingestLine (i : int) (heightMap : Map<int*int, int>) (line : string) : Map<int*int, int> =
    Array.ofSeq line
    |> Array.foldi (addChar i) heightMap

let ingestLines (lines : string array) : Map<int*int, int> =
    Array.foldi ingestLine Map.empty lines

// ----------------------------------------------------------------------------

let bumpNeighbours map (x,y) =
    map
    |> Map.change (x-1,y-1) (Option.map ((+) 1))
    |> Map.change (x,  y-1) (Option.map ((+) 1))
    |> Map.change (x+1,y-1) (Option.map ((+) 1))
    |> Map.change (x,  y-1) (Option.map ((+) 1))
    // no (x, y), because we don't need to bump ourselves
    |> Map.change (x,  y+1) (Option.map ((+) 1))
    |> Map.change (x+1,y-1) (Option.map ((+) 1))
    |> Map.change (x+1,y  ) (Option.map ((+) 1))
    |> Map.change (x+1,y+1) (Option.map ((+) 1))

let rec stepFlashes octopuses =
    octopuses
    |> Map.map (fun _ v -> if v >= 9 then 0 else v)

let step (flashCount, octopuses) stepIndex =
    (flashCount, octopuses)

// ----------------------------------------------------------------------------

let makeBold (boldguy : int) (i : int) : string=
    if i = boldguy then
        sprintf "\u001b[1m%i\u001b[0m" i
    else
        sprintf "\u001b[2m%i\u001b[0m" i

let showoff map =
    let lines =
        seq {
            for x in [0..9] do
            let chars =
                Map.filter (fun (xn, _) _ -> xn = x) map
                |> Map.toList
                |> List.sortBy (fst >> snd)
                |> List.map (snd >> makeBold 0)
            yield String.concat "" chars
        }
    String.concat "\n" lines

let map =
    input
    |> ingestLines
// |> fun octopuses -> List.fold step (0, octopuses) [1.100]
// |> fst
printfn "%s" <| showoff map

// |> fun m -> bumpNeighbours m (0,0)
// |> showoff
// |> printfn "%s"

// List.fold (
//     fun a b ->
//         printfn "%i" a
//         System.Threading.Thread.Sleep 50
//         a+b
//     ) 0 [1..100]
