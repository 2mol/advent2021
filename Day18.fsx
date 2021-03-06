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
    System.IO.File.ReadLines "inputs/input18.txt"
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

let rec look path tree =
    match path, tree with
    | [], _ -> tree
    | Left::pathTail, Branch (t1, _) -> look pathTail t1
    | Right::pathTail, Branch (_, t2) -> look pathTail t2
    | _ -> failwith <| sprintf "can't reach the spot to look %s - %A" (treeToString tree) path

let rec allPaths' partialPath tree : Direction list list =
    match look partialPath tree with
    | Leaf _ ->
        let otherPaths =
            if List.contains Left partialPath then
                let idx = List.findIndexBack ((=) Left) partialPath
                let newPartialPath = List.append partialPath[..idx-1] [Right]
                allPaths' newPartialPath tree
            else
                // all Rights -> we reached the end
                []
        partialPath :: otherPaths
    | Branch _ ->
        allPaths' (List.append partialPath [Left]) tree

let allPaths tree = allPaths' [] tree

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

let explode (path : Direction list) tree : Tree =
    match look path tree with
    | Branch (Leaf left, Leaf right) ->
        replace path (Leaf 0) tree
        |> fun t ->
            // to go to the next left, find the last crossing that _wasn't_ left,
            // and go left there instead
            if List.contains Right path then
                let idx = List.findIndexBack ((=) Right) path
                let pathStub = List.append path[..idx-1] [Left]
                let rightPath = goFurthest Right (look pathStub tree)
                let leftValPath = List.append pathStub rightPath
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
    | _ -> failwith "you gave me the wrong explosion coordinates"

let exploders tree =
    [
        for path in (allPaths tree) do
        match look path[..^1] tree with
        | Branch (Leaf _, Leaf _) when List.length path > 4 -> yield path[..^1]
        | _ -> yield! []
    ]
    |> List.distinct

let split tree =
    let splitPaths =
        [
            for path in (allPaths tree) do
            match look path tree with
            | Leaf n when n >= 10 -> yield path, n
            | _ -> yield! []
        ]
    if List.length splitPaths > 0 then
        let down n = (float)n / 2. |> floor |> int
        let up n = (float)n / 2. |> ceil |> int

        let path, n = List.head splitPaths

        replace path (Branch (Leaf (down n), Leaf (up n))) tree
    else
        tree

let rec reduce tree =
    let reducedTree =
        exploders tree
        |> List.fold (fun t p ->
            // printfn "exploding %s" (treeToString t)
            explode p t
        ) tree
        |> split
    if reducedTree = tree then
        tree
    else
        reduce reducedTree

// Parse.parse "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
// |> Option.get
// |> reduce
// // |> explode [Left;Left;Left;Left]
// // |> explode [Left;Right;Right;Left]
// // |> look [Left; Left; Left; Right]
// |> treeToString
// |> printfn "%A"

// --------------------------- addition rules etc -----------------------------

let add t1 t2 =
    reduce <| Branch (t1, t2)

let rec magnitude tree =
    match tree with
    | Leaf n -> n
    | Branch (t1, t2) -> 3 * magnitude t1 + 2 * magnitude t2

// Parse.parse "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
// |> Option.get
// |> magnitude
// |> printfn "%A"

List.tail inputNumbers
|> List.fold add (List.head inputNumbers)
// |> treeToString
// |> printfn "%A"
|> magnitude
|> printfn "day18-1: %i"


List.allPairs inputNumbers inputNumbers
|> List.map (fun (a, b) -> add a b)
|> List.map magnitude
|> List.max
|> printfn "day18-2: %i"
