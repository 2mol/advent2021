#load "Extensions.fsx"
#nowarn "25"

open System
open Extensions

// live reload with:
// -----------------
// echo Day17.fsx | entr -n dotnet fsi --langversion:preview /_

printfn "---------- \u001b[33mlet's go\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input17.txt"
    |> Seq.head

let parse (str : string) : ((int*int)*(int*int)) =
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
    if y < tymin || x > txmax then
        []
    else
        let new_x = x + vx
        let new_y = y + vy
        let new_vx = max 0 (vx - 1)
        let new_vy = vy - 1
        (new_x, new_y) :: shoot' (new_x, new_y) (new_vx, new_vy)

let shoot = shoot' (0,0)

let rec shoot2' (x, y) (vx, vy) =
    if x >= txmin && x <= txmax && y >= tymin && y <= tymax then
        Some (x, y)
    else if y < tymin || x > txmax then
        None
    else
        let new_x = x + vx
        let new_y = y + vy
        let new_vx = max 0 (vx - 1)
        let new_vy = vy - 1
        shoot2' (new_x, new_y) (new_vx, new_vy)

let shoot2 = shoot2' (0,0)

let hitsTarget (trail : (int*int) list) =
    let (x, y) = trail[^1] // second-to-last, due to the way shot trail is constructed.
    x >= txmin && x <= txmax && y >= tymin && y <= tymax

// let shot = shoot (11,97)

let shots =
    [
        for y in [-210..220] do
        for x in [1..116] do
        // if hitsTarget shot then
        match shoot2 (x,y) with
        | Some _ -> yield x, y //, (List.maxBy snd shot)
        | None -> yield! []
    ]

// y > 1 (want it to stagnate, and anyway it wants to be large)
// x in [11..14]
// y >= 97

// draw shot
// |> printfn "%s"

// (hitsTarget shot, List.map snd shot |> List.max)
// |> printfn "day17-1: %A"


// for s in shots do
//     let x, y, maxCoord = s
//     printfn "%i %i: %A" x y maxCoord

List.length shots
|> printfn "day17-1: %A"

printfn "------------ \u001b[32mdone\u001b[0m ------------\n"
