#load "Extensions.fsx"
open Extensions

open System

#nowarn "25"

// live reload with:
// -----------------
// ls Day12.fsx | entr -n dotnet fsi Day12.fsx

let parse line =
    let [|a; b|] = String.split "-" line
    a, b

let input =
    System.IO.File.ReadLines "inputs/input12.txt"
    |> Seq.map parse

let canTraverse currentPath (str:string) =
    if Char.IsUpper str[0] then true
    else not <| List.contains str currentPath

let rec traverse currentPath map =
    let currentNode = List.head currentPath
    if currentNode = "end" then
        [ List.rev currentPath ]
    else
        let nextNodes =
            Map.tryFind currentNode map
            |> Option.defaultValue []
            |> List.filter (canTraverse currentPath)

        [ for node in nextNodes do yield! traverse (node::currentPath) map ]

// input
let map1 = Seq.fold (fun m (k,v) -> Map.addToList k v m) Map.empty input
let map = Seq.fold (fun m (k,v) -> Map.addToList k v m) map1 (Seq.map Tuple.flip input)

map
|> traverse ["start"]
|> List.length
|> printfn "%A"
