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
    let [|instruction; splitCoord|] = String.split "=" line
    instruction, int splitCoord

let coords =
    input[0..idx-1]
    |> List.map parseCoords

let folds =
    input[idx+1..]
    |> List.map parseFold

let maxx = 1 + fst (List.maxBy fst coords)
let maxy = 1 + snd (List.maxBy snd coords)

let grid = Array2D.create maxx maxy '.'

for (x, y) in coords do
    grid[x,y] <- '#'

let foldAlongX xSplit (g : char[,]) =
    let half1 = g[0..xSplit-1, *]
    let half2 = g[xSplit+1.., *]

    Array2D.iteri (
        fun i j v ->
            if v = '#' then
                half1[Array2D.length1 half1 - i - 1, j] <- v
    ) half2

    half1

let foldAlongY ySplit (g : char[,]) =
    let half1 = g[*, 0..ySplit-1]
    let half2 = g[*, ySplit+1..]

    Array2D.iteri (
        fun i j v ->
            if v = '#' then
                half1[i, Array2D.length2 half1 - j - 1] <- v
    ) half2

    half1

let printGrid (g : char[,]) =
    for j in [0..Array2D.length2 g - 1] do
        for i in [0..Array2D.length1 g - 1] do
            printf "%c" g[i,j]
        printfn ""

let calcMarks (g : char[,]) =
    Array2D.flatten g
    |> Array.sumBy (fun a -> if a = '#' then 1 else 0)

let foldPaper (g : char[,]) (instruction, splitCoord) =
    match instruction with
    | "fold along x" -> foldAlongX splitCoord g
    | "fold along y" -> foldAlongY splitCoord g

List.fold foldPaper grid folds
|> printGrid
