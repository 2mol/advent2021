#r "nuget: FParsec, 1.1.1"
#load "Extensions.fsx"
#nowarn "25"

open System
open Extensions

// live reload with:
// -----------------
// echo Day18.fsx | entr -n dotnet fsi --langversion:preview /_

printfn "---------- \u001b[34mlet's go\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input18sm.txt"
    |> Seq.toList

// ----------------------- data type and parser ------------------------------

type Tree =
    | Branch of Tree * Tree
    | Leaf of int

let rec treeToString tree =
    match tree with
    | Leaf a -> sprintf "%i" a
    | Branch (t1, t2) -> sprintf "[%s,%s]" (treeToString t1) (treeToString t2)

module Parse =
    open FParsec
    let (tree : Parser<Tree, unit>), treeRef = createParserForwardedToRef()
    let leaf : Parser<Tree, unit> = pint32 |>> Leaf
    let branch = (tree <|> leaf) .>>. (skipChar ',' >>. (tree <|> leaf)) |>> Branch
    do treeRef.Value <- between (pchar '[') (pchar ']') branch

    let toOption presult =
        match presult with
        | Success (v, _, _) -> Some v
        | Failure _ -> None

    let parse = run tree >> toOption

// Parse.parse "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
// |> Option.get
// |> treeToString
// |> printfn "%A"

let inputNumbers =
    input
    |> List.choose Parse.parse
    // |> printfn "%A"

// ------------------------ tree algorithms ----------------------------------

type Direction =
    Left | Right

let opposite dir =
    if dir = Left then Right else Left

let rec look path tree =
    match path, tree with
    | [], _ -> tree
    | Left::pathTail, Branch (t1, _) -> look pathTail t1
    | Right::pathTail, Branch (_, t2) -> look pathTail t2
    | _ -> failwith <| sprintf "can't reach the spot to look %s - %A" (treeToString tree) path

let rec replace path subTree tree =
    match path, tree with
    | [], _ ->
        subTree
    | Left::subPath, Branch (t1, t2) ->
        let newT1 = replace subPath subTree t1
        Branch (newT1, t2)
    | Right::subPath, Branch (t1, t2) ->
        let newT2 = replace subPath subTree t2
        Branch (t1, newT2)
    | _ -> failwith "can't reach the spot to replace"

let rec goFurthest dir tree =
    match tree with
    | Leaf _ -> []
    | Branch (t1, t2) ->
        match dir with
        | Left -> Left :: goFurthest dir t1
        | Right -> Right :: goFurthest dir t2

let rec somethingExplodey (path : Direction list) tree : Tree =
    match look path tree with
    | Branch (Leaf left, Leaf right) ->
        replace path (Leaf 0) tree
        |> fun t ->
            // to go to the next left, find the last crossing that _wasn't_ left,
            // and go left there instead
            if List.contains Right path then
                let idx = List.findIndexBack ((=) Left) path
                let pathStub = List.append path[..idx-1] [Left]
                let leftPath = goFurthest Left (look pathStub tree)
                let leftValPath = List.append pathStub leftPath
                let (Leaf leftVal) = look leftValPath tree
                replace leftValPath (Leaf (leftVal + left)) t
            else
                t
        |> fun t ->
            // to go to the next right, find the last crossing that _wasn't_ right,
            // and go right there instead
            if List.contains Left path then
                let idx = List.findIndexBack ((=) Left) path
                let pathStub = List.append path[..idx-1] [Right]
                let leftPath = goFurthest Left (look pathStub tree)
                let rightValPath = List.append pathStub leftPath
                let (Leaf rightVal) = look rightValPath tree
                replace rightValPath (Leaf (rightVal + right)) t
            else
                t
    | Branch (Branch _, _) ->
        somethingExplodey (Left::path) tree
    | Branch (Leaf _, Branch _) ->
        somethingExplodey (Right::path) tree

Parse.parse "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
|> Option.get
|> somethingExplodey [Left;Left;Left;Left]
// |> look [Left; Left; Left; Right]
|> treeToString
|> printfn "%A"

// --------------------------- addition rules etc -----------------------------

let rec reduce t = t

let add t1 t2 =
    Branch (t1, t2)
