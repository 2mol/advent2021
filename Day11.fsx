#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// ls Day11.fsx | entr -n dotnet fsi Day11.fsx

Console.Clear()
Console.SetCursorPosition(0, Console.CursorTop)

// printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"
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

let makeBold (i : int) : string =
    if i = 0 then
        sprintf "\u001b[1m%i\u001b[0m" i
    else
        sprintf "\u001b[2m%i\u001b[0m" i

let makeBold' (i : int) : string =
    let c =
        match i with
        | 0 -> '0'
        | 1 -> ' '
        | 2 -> ' '
        | 3 -> '·'
        | 4 -> '·'
        | 5 -> '·'
        | 6 -> 'o'
        | 7 -> 'o'
        | 8 -> '0'
        | 9 -> '0'
    if i = 0 then
        sprintf "\u001b[1m%c\u001b[0m" c
    else if i = 9 then
        sprintf "%c" c
    else
        sprintf "\u001b[2m%c\u001b[0m" c

let showoff map =
    let lines =
        seq {
            for x in [0..9] do
            let chars =
                Map.filter (fun (xn, _) _ -> xn = x) map
                |> Map.toList
                |> List.sortBy (fst >> snd)
                |> List.map (snd >> makeBold')
            yield String.concat "" chars
        }
    String.concat "\n" lines

// ----------------------------------------------------------------------------

let bumpNeighbours map (x,y) =
    map
    |> Map.change (x-1,y-1) (Option.map ((+) 1))
    |> Map.change (x,  y-1) (Option.map ((+) 1))
    |> Map.change (x+1,y-1) (Option.map ((+) 1))
    |> Map.change (x-1,y  ) (Option.map ((+) 1))
    // no (x, y), because we don't need to bump ourselves
    |> Map.change (x+1,y  ) (Option.map ((+) 1))
    |> Map.change (x-1,y+1) (Option.map ((+) 1))
    |> Map.change (x,  y+1) (Option.map ((+) 1))
    |> Map.change (x+1,y+1) (Option.map ((+) 1))

let rec stepFlashes haveFlashed octopuses =
    let aboutToFlash =
        Map.filter (fun _ v -> v > 9) octopuses
        |> Map.keys
        |> Set.ofSeq
        |> fun s -> Set.difference s haveFlashed

    if Set.isEmpty aboutToFlash then
        haveFlashed, Map.map (fun _ v -> if v > 9 then 0 else v) octopuses
    else
        let bumpedFuckers =
            Set.fold (fun m coord -> bumpNeighbours m coord) octopuses aboutToFlash
        stepFlashes (Set.union haveFlashed aboutToFlash) bumpedFuckers

let rec step n (flashCount, octopuses) =
    Console.SetCursorPosition(0, Console.CursorTop + 4)
    printfn "%s\n\n\n" <| showoff octopuses
    System.Threading.Thread.Sleep 150

    if n <= 0 then
        (flashCount, octopuses)
    else
        let haveFlashed, nextopuses =
            Map.mapValues ((+) 1) octopuses
            |> stepFlashes Set.empty

        let flashes = Set.count haveFlashed

        step (n-1) (flashCount + flashes, nextopuses)

// ----------------------------------------------------------------------------

let octopuses =
    input
    |> ingestLines
// |> fun octopuses -> List.fold step (0, octopuses) [1.100]
// |> fst

// printfn "%s" <| showoff octopuses

let (flashCount, _) = step 100 (0, octopuses)

Console.SetCursorPosition(0, Console.CursorTop + 14)
printf "day 11-1: %i\n\n" flashCount

// step <| step (0, octopuses)

// for i in [1..100] do
//    let bla = step (0, octopuses)

// |> fun m -> bumpNeighbours m (0,0)
// |> showoff
// |> printfn "%s"

// List.fold (
//     fun a b ->
//         printfn "%i" a
//         System.Threading.Thread.Sleep 50
//         a+b
//     ) 0 [1..100]
