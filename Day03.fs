module Day03

open System
open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input3.txt"
    |> Seq.toList

let MeasurementLength = 12

let aggregate (counter : int array) (str : string) =
    Array.zip counter (Seq.toArray str)
    |> Array.map (
        fun (count, digit) ->
            match digit with
            | '1' -> count + 1
            | _ -> count
        )

let solution1 =
    List.fold aggregate (Array.zeroCreate MeasurementLength) input
    |> Array.map (fun n -> n > List.length input / 2 )
    |> Array.map (fun b -> if b then "1" else "0")
    |> String.concat ""
    |> fun b -> Convert.ToInt32(b, 2)
    |> fun n -> n * (n ^^^ (pown 2 MeasurementLength - 1))
    |> sprintf "%A"


// --------------------------------------


let solution2 = ""
