module Day04

open System
open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input4.txt"
    |> Seq.toArray

// type Cell = { Number : string; Marked : bool }

// let initCell n = { Number = n; Marked = false }

let parseBoards (lines : string array) : int array array =
    // TODO: full boards
    lines[1..5]
    |> Array.map (fun str ->
        str.Trim().Split " "
        |> Array.filter (not << String.IsNullOrEmpty)
        |> Array.map int
    )


let boardWins (board : int array array) (calledNumbers : int array) : (int * int) option =
    None

let solution1 =
    let numbers = (Array.head input).Split "," |> Array.map int
    let boards = parseBoards <| Array.tail input
    printfn "%A" boards
    // seq {
    //     for n in numbers do
    //     for b in boards do
    //     if boardIsWinner b then
    //     yield b
    // }
    ""

let solution2 = ""
