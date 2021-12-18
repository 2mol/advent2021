#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// echo Day15.fsx | entr -n dotnet fsi /_

printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input15.txt"
    |> Seq.toArray
    |> Array.map Seq.toArray
    |> Array2D.ofArrays
    |> Array2D.map (Char.GetNumericValue >> int)

let rec bruteForce (pathSoFar, costSoFar) grid =
    let (x, y) = List.head pathSoFar
    if x < 0 || x >= Array2D.length1 grid || y < 0 || y >= Array2D.length2 grid then
        // illegal move
        []
    else if (x, y) = (Array2D.length1 grid - 1, Array2D.length2 grid - 1) then
        // reached the end, we're done
        // printfn "found a path!\n %A" (pathSoFar, costSoFar)
        [pathSoFar, costSoFar]
    else
        let nextMoves =
            [ x+1, y; x-1, y; x, y+1; x, y-1 ]
            |> List.filter (fun coord -> not (List.contains coord pathSoFar))

        let branchingPaths =
            [for coord in nextMoves do yield! bruteForce (coord::pathSoFar, costSoFar + grid[x, y]) grid]

        if List.isEmpty branchingPaths then
            []
        else
            let cheapestPath, cheapestPathCost =
                List.minBy snd branchingPaths

            [cheapestPath, cheapestPathCost]

let rec gentleForce (pathSoFar, costSoFar) grid map =
    let (x, y) = List.head pathSoFar
    match Map.tryFind coord map with
    | Some (path, cost) -> (path, cost)
    | None ->
        let minimalPath =
            bruteForce ([(0,0)], 0) grid
            |> List.minBy snd


input[2.., 2..]
// |> bruteForce ([(0,0)], 0)
// |> List.minBy snd
|> printfn "%A"
