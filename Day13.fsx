#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// ls Day13.fsx | entr -n dotnet fsi Day13.fsx


let input =
    System.IO.File.ReadLines "inputs/input13.txt"
    |> Seq.toList

let idx =
    input
    |> List.findIndex String.IsNullOrEmpty

let parseCoords line =
    let [|a; b|] = String.split "," line
    int a, int b

let parseFold line =
    let [|a; b|] = String.split "=" line
    a, int b

let coords =
    input[0..idx-1]
    |> List.map parseCoords
// let folds = input[idx+1..]

let maxx = 1 + fst (List.maxBy fst coords)
let maxy = 1 + snd (List.maxBy snd coords)

let grid = Array2D.create maxx maxy '.'

for (x, y) in coords do
    grid[x,y] <- '#'

// for i in [0..maxx-1] do
//     for j in [0..maxy-1] do
//         printf "%c" grid[i,j]
//     printfn ""

let half1 = grid[0..654, *]
let half2 = grid[656.., *]

// for i in [0..maxx-1] do
//     for j in [0..maxy-1] do
//         printf "%c" grid[i,j]
//     printfn ""

Array2D.iteri (
    fun i j v ->
        if v = '#' then
            half1[Array2D.length1 half1 - i, j] <- v
) half2

half1
|> Array2D.flatten
|> Array.sumBy (fun a -> if a = '#' then 1 else 0)
|> printfn "%A"
