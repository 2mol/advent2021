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

let canTraverse1 currentPath (str:string) =
    if Char.IsUpper str[0] then true
    else not <| List.contains str currentPath

let rec traverse1 currentPath map =
    let currentNode = List.head currentPath
    if currentNode = "end" then
        [ List.rev currentPath ]
    else
        let nextNodes =
            Map.tryFind currentNode map
            |> Option.defaultValue []
            |> List.filter (canTraverse1 currentPath)

        [ for node in nextNodes do yield! traverse1 (node::currentPath) map ]

// input
let map1 = Seq.fold (fun m (k,v) -> Map.addToList k v m) Map.empty input
let map = Seq.fold (fun m (k,v) -> Map.addToList k v m) map1 (Seq.map Tuple.flip input)

map
|> traverse1 ["start"]
|> List.length
|> printfn "day12-1: %i"

// ----------------------------------------------------------------------------

let canTraverse pathSoFar (nextNode:string) =
    if Char.IsUpper nextNode[0] then
        true
    else
        // let notVisitedYet = not <| List.contains str pathSoFar
        let smallCavesVisitorCount =
            pathSoFar
            |> List.filter (fun (str:string) -> Char.IsLower str[0])
            |> List.countBy id
            |> Map.ofList

        // printfn "%A" smallCavesVisitorCount

        let canVisit =
            (nextNode <> "start") && (
                false
                || Set.ofSeq (Map.values smallCavesVisitorCount) = Set.empty
                || Set.ofSeq (Map.values smallCavesVisitorCount) = set [1]
                || not (Map.containsKey nextNode smallCavesVisitorCount)
            )
        canVisit

let rec traverse currentPath map =
    let currentNode = List.head currentPath
    if currentNode = "end" then
        // printfn "%s" <| String.concat "," (List.rev currentPath)
        [ List.rev currentPath ]
    else
        let nextNodes =
            Map.tryFind currentNode map
            |> Option.defaultValue []
            |> List.filter (canTraverse currentPath)

        [ for node in nextNodes do yield! traverse (node::currentPath) map ]

map
|> traverse ["start"]
|> List.length
|> printfn "day12-2: %A"
