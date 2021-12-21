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


// let draw a =
//     [ for y in [ymin..ymax] do
//         yield [
//             for x in [xmin..xmax] do
//             if x=0 && y=0 then
//                 yield 'S'
//             else if List.contains (x, y) shotTrail then
//                 yield '#'
//             else if x >= txmin && x <= txmax && y >= tymin && y <= tymax then
//                 yield 'T'
//             else
//                 yield '.'
//         ]
//     ]
//     |> List.map (fun cs -> String.Concat(cs))
//     |> String.concat "\n"

let extend a =
    let l1 = Array2D.length1 a + 2
    let l2 = Array2D.length2 a + 2
    Array2D.init l1 l2
        (fun i j ->
            if i = 0 || j = 0 then
                false
            else if i = l1 || j = l2 then
                false
            else
                a[i-1,j-1]
        )


image
|> printfn "day20-1: %A"
