module Day04

open System
open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input4.txt"
    |> Seq.toArray

type Board = int array array

let rec parseBoards (lines : string array) : Board list =
    if Array.length lines < 6 then
        []
    else
        let firstBoard =
            lines[1..5]
            |> Array.map (fun str ->
                str.Trim().Split " "
                |> Array.filter (not << String.IsNullOrEmpty)
                |> Array.map int
            )
        firstBoard :: parseBoards lines[6..]

let boardWins (calledNumbers : int array) (board : Board) : int option =
    let rowSlices = seq { for i in 0..4 do yield (array2D board)[i,*]}
    let colSlices = seq { for i in 0..4 do yield (array2D board)[*,i]}
    let slices = Seq.append rowSlices colSlices
    let allNumbersAreCalled slice =
        Array.map (fun el -> Array.contains el calledNumbers) slice
        |> Array.forall id

    let winningSlices =
        seq {for slice in slices do if allNumbersAreCalled slice then yield slice}

    let unmarkedSum () =
        Array.concat board
        |> Array.filter (fun el -> not <| Array.contains el calledNumbers)
        |> Array.sum

    Seq.tryHead winningSlices
    |> Option.map (fun slice -> unmarkedSum () * Seq.last calledNumbers)

let solution1 =
    let numbers = (Array.head input).Split "," |> Array.map int
    let boards = parseBoards <| Array.tail input
    seq {
        for i in 0 .. Array.length numbers - 1 do
        for b in boards do
        let result = boardWins numbers[..i] b
        if Option.isSome result then yield result
    }
    |> Seq.head
    |> fun (Some bla) -> bla
    |> sprintf "%A"

let solution2 = ""
