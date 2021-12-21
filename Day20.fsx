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

let ENHANCE a =
    let l1 = Array2D.length1 a
    let l2 = Array2D.length2 a

    let ae =
        extend 2 a

    let enhance i j _ =
        if i < 1 || j < 1 || i > l1+2 || j > l2+2 then
            false
        else
            let bidx =
                ae[i-1..i+1,j-1..j+1]
                |> Array2D.transpose
                |> Array2D.flatten
                |> Array.map (fun b -> if b then '1' else '0')
                |> fun cs -> String.Concat(cs)
            let idx = Convert.ToInt32(bidx, 2)
            printfn "%A" (i, j, bidx, idx)
            enhancement[idx]

    (Array2D.mapi enhance ae)[1..l1+2,1..l2+2]


    // for i in [0..l1-1] do
    //     for j in [0..l2-1] do
    //         let
    //         a[i,j] <- false


image
// |> extend 2
|> ENHANCE
|> ENHANCE
|> draw
|> printfn "%s"
// |> printfn "day20-1: %A"
