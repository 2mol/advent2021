#load "Extensions.fsx"
#nowarn "25"

open System
open Extensions

// live reload with:
// -----------------
// echo Day20.fsx | entr -n dotnet fsi --langversion:preview /_

printfn "---------- \u001b[91mlet's go\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input20sm.txt"
    |> Seq.toArray

let enhancement =
    input[0]
    |> Seq.map ((=) '#')
    |> Seq.toArray

let image =
    input[2..]
    |> Array.map (Seq.toArray)
    |> Array2D.ofArrays
    |> Array2D.map ((=) '#')


let draw a =
    let l1 = Array2D.length1 a
    let l2 = Array2D.length2 a
    [
        for j in [0..l2-1] do
        yield [
            for i in [0..l1-1] do
            if a[i,j] then '#' else '.'
        ]
    ]
    |> List.map (fun cs -> String.Concat(cs))
    |> String.concat "\n"

let extend n a =
    let l1 = Array2D.length1 a + 2*n
    let l2 = Array2D.length2 a + 2*n
    Array2D.init l1 l2
        (fun i j ->
            if i < n || j < n || i > l1-1-n || j > l2-1-n then
                false
            else
                a[i-n,j-n]
        )

// let ENHANCE a =
//     let l1 = Array2D.length1 a
//     let l2 = Array2D.length2 a

//     Array2D.init l1 l2
//         (fun i j ->
//             a[]
//         )

    // for i in [0..l1-1] do
    //     for j in [0..l2-1] do
    //         let
    //         a[i,j] <- false


image
|> extend 2
// |> ENHANCE
|> draw
|> printfn "%s"
// |> printfn "day20-1: %A"
