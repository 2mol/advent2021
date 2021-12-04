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

    let sliceIsWin slice =
        Array.map (fun el -> Array.contains el calledNumbers) slice
        |> Array.forall id

    let winningSlices =
        seq {for slice in slices do if sliceIsWin slice then yield slice}

    let unmarkedSum () =
        Array.concat board
        |> Array.filter (fun el -> not <| Array.contains el calledNumbers)
        |> Array.sum

    Seq.tryHead winningSlices
    |> Option.map (fun slice -> unmarkedSum () * Seq.last calledNumbers)

let rec boardsInOrderOfWins (numbers : int array) (calledIndex : int) (boards : Board list) : int list =
    if Seq.isEmpty boards || calledIndex > Seq.length numbers then List.empty
    else
        let winners =
            seq {
                for board in boards do
                let result = boardWins numbers[..calledIndex] board
                if Option.isSome result then yield (board, Option.get result)
            }

        if Seq.isEmpty winners then
            boardsInOrderOfWins numbers (calledIndex + 1) boards
        else
            let newBoards =
                List.except (Seq.map fst winners) boards

            List.append
                (Seq.map snd winners |> List.ofSeq)
                (boardsInOrderOfWins numbers (calledIndex + 1) newBoards)

let solution1 =
    let numbers = (Array.head input).Split "," |> Array.map int
    let boards = parseBoards <| Array.tail input

    boardsInOrderOfWins numbers 0 boards
    |> Seq.head
    |> sprintf "day4-1: %A"

let solution2 =
    let numbers = (Array.head input).Split "," |> Array.map int
    let boards = parseBoards <| Array.tail input

    boardsInOrderOfWins numbers 0 boards
    |> Seq.last
    |> sprintf "day4-2: %A"
