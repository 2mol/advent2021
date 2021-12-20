#load "Extensions.fsx"
#nowarn "25"

open System
open Extensions

// live reload with:
// -----------------
// echo Day17.fsx | entr -n dotnet fsi /_

printfn "---------- \u001b[33mlet's go\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input17small.txt"
    |> Seq.head

let parse (str : string) : (int*int)*(int*int)=
    let [|[|txmin;txmax|]; [|tymin;tymax|]|] =
        str.TrimStart(Array.ofSeq "target area: x=")
        |> String.split ", y="
        |> Array.map (String.split ".." >> Array.map int)
    (txmin, txmax), (tymin, tymax)

let (txmin, txmax), (tymin, tymax) = parse input

let draw shotTrail =
    let shotTrailxs = List.map fst shotTrail
    let shotTrailys = List.map snd shotTrail
    let xmin = 0
    let xmax = List.max <| txmax :: shotTrailxs
    let ymin = List.min <| tymin :: shotTrailys
    let ymax = List.max <| 0 :: tymax :: shotTrailys

    [ for y in [ymin..ymax] do
        yield [
            for x in [xmin..xmax] do
            if x=0 && y=0 then
                yield 'S'
            else if List.contains (x, y) shotTrail then
                yield '#'
            else if x >= txmin && x <= txmax && y >= tymin && y <= tymax then
                yield 'T'
            else
                yield '.'
        ]
    ]
    |> List.map (fun cs -> String.Concat(cs))
    |> List.rev // y is kinda flipped, it's a bit weird
    |> String.concat "\n"

let rec shoot' (x, y) (vx, vy) =
    let new_x = x + vx
    let new_y = y + vy
    if new_y < tymin || new_x > txmax then
        []
    else
        let new_vx = max 0 (vx - 1)
        let new_vy = vy - 1
        (new_x, new_y) :: shoot' (new_x, new_y) (new_vx, new_vy)

let shoot = shoot' (0,0)

let hitsTarget trail =
    let (x, y) = List.last trail
    x >= txmin && x <= txmax && y >= tymin && y <= tymax

let shot = shoot (6,9)

draw shot
|> printfn "%s"

hitsTarget shot
|> printfn "day17-1: %A"

printfn "------------ \u001b[32mdone\u001b[0m ------------\n"
