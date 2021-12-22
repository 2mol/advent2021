#r "nuget: FParsec, 1.1.1"
#load "Extensions.fsx"
#nowarn "25"

open System
open FParsec
open Extensions

// live reload with:
// -----------------
// echo Day18.fsx | entr -n dotnet fsi --langversion:preview /_

printfn "---------- \u001b[34mlet's go\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input18sm.txt"
    |> Seq.toList


type Tree =
    | Branch of Tree * Tree
    | Leaf of int

let (tree : Parser<Tree, unit>), treeRef = createParserForwardedToRef()
let leaf : Parser<Tree, unit> = pint32 |>> Leaf
let branch = (tree <|> leaf) .>>. (skipChar ',' >>. (tree <|> leaf)) |>> Branch
do treeRef.Value <- between (pchar '[') (pchar ']') branch

// run tree "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
// |> printfn "%A"

let parserToOption presult =
    match presult with
    | Success (v, _, _) -> Some v
    | Failure _ -> None

let inputNumbers =
    input
    |> List.choose (run tree >> parserToOption)
    // |> printfn "%A"

type Direction = Left | Right

let add t1 t2 =
    Branch (t1, t2)

let rec look t path =
    match path, t with
    | [], _ -> t
    | Left::pathTail, Branch (t1, t2) -> look t1 pathTail
    | Right::pathTail, Branch (t1, t2) -> look t2 pathTail
    | _ -> failwith "can't reach the spot"

let rec replace mainT path subT =
    match path, mainT with
    | [], _ ->
        subT
    | Left::subPath, Branch (t1, t2) ->
        let newT1 = replace t1 subPath subT
        Branch (newT1, t2)
    | Right::subPath, Branch (t1, t2) ->
        let newT2 = replace t2 subPath subT
        Branch (t1, newT2)
    | _ -> failwith "can't reach the spot"


let rec findExploders t (path : Direction list) : ((Direction list)*(int*int)) list =
    match t with
    | Branch (Leaf a, Leaf b) -> []
    | _ -> [[],(0,0)]

let rec reduce t = t
