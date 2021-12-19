// ----------- ~ Functions that should be in the standard library ~ -----------

module Extensions

let always = (fun x _ -> x)

let applyN (n : int) (f: 'T -> 'T) (initialState : 'T) : 'T =
    // Note: applyN 0 will naturally just return initialState.
    List.fold (fun state _ -> f state) initialState [1..n]

module Array =
    let foldi (folder: (int -> 'State -> 'T -> 'State)) (state : 'State) (array : 'T array)
        : 'State =
        Array.indexed array
        |> Array.fold (fun state (i, a) -> folder i state a) state

module Array2D =
    let flatten (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray

    let ofArrays (a : 'T array array) =
        Array2D.init (Array.length (Array.head a)) (Array.length a) (fun i j -> a[j][i])

module Tuple =
    let flip (a, b) = (b, a)
    let mapFst f (a, b) = (f a, b)
    let mapSnd f (a, b) = (a, f b)

module Map =
    let merge map1 map2 =
        Map.fold (fun map k v -> Map.add k v map) map1 map2
        // List.concat [Map.toList map1; Map.toList map2] |> Map.ofList

    let mergeMany maps =
        Seq.map Map.toSeq maps
        |> Seq.concat
        |> Map.ofSeq

    // TODO: figure out how to make this generic, i.e. work with _all_ types that support (+)
    // In ./Day14.fsx there were problems with the generic type signature:
    // let mergeCounts map1 map2 =
    let mergeCounts (map1 : Map<'Key, int64>) (map2 : Map<'Key, int64>) : Map<'Key, int64> =
        let folder acc k v =
            match Map.tryFind k acc with
            | Some v0 -> Map.add k (v0 + v) acc
            | None -> Map.add k v acc
        Map.fold folder map1 map2

    let mergeCountsMany (maps : Map<'Key, int64> seq) : Map<'Key, int64> =
        Seq.fold mergeCounts Map.empty maps

    let mapValues f m =
        Map.map (fun _ v -> f v) m

    // TODO: this might be confusing w.r.t List.reverse, so it could also be renamed Map.flip
    let reverse (map : Map<'Key,'T>) : Map<'T, 'Key> =
        Map.toList map
        |> List.map Tuple.flip
        |> Map.ofList

    let addOption (key : 'Key) (value : 'T option) (map: Map<'Key,'T>) : Map<'Key,'T> =
        match value with
        | Some v -> Map.add key v map
        | None -> map

    let addToList (key : 'Key) (value : 'T) (map: Map<'Key,'T list>)
        : Map<'Key,'T list> =
        if not <| Map.containsKey key map then
            Map.add key [value] map
        else
            Map.change key (Option.map (fun tail -> value::tail)) map

module String =
    let split (sep : string) (str : string) : string array = str.Split(sep)

    // let reverse (s : string) = s |> Seq.toArray |> Array.rev |> System.String
