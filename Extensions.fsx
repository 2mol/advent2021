// ----------- ~ Functions that should be in the standard library ~ -----------

let always = (fun x _ -> x)

module Array =
    let foldi
        (folder: (int -> 'State -> 'T -> 'State))
        (state : 'State)
        (array : 'T array)
        :
        'State
        =
        Array.indexed array
        |> Array.fold (fun state (i, a) -> folder i state a) state

module Array2D =
    let flatten (arr: 'T [,]) = arr |> Seq.cast<'T> |> Seq.toArray

module Tuple =
    let flip (a, b) = (b, a)

module Map =
    let merge m1 m2 =
        List.concat [Map.toList m1; Map.toList m2] |> Map.ofList

    let mapValues f m =
        Map.map (fun _ v -> f v) m

    let reverse (map : Map<'Key,'T>) : Map<'T, 'Key> =
        Map.toList map
        |> List.map Tuple.flip
        |> Map.ofList

    let addOption (key : 'Key) (value : 'T option) (map: Map<'Key,'T>) : Map<'Key,'T> =
        match value with
        | Some v -> Map.add key v map
        | None -> map

    let addToList
        (key : 'Key)
        (value : 'T)
        (map: Map<'Key,'T list>)
        :
        Map<'Key,'T list>
        =
        if not <| Map.containsKey key map then
            Map.add key [value] map
        else
            Map.change key (Option.map (fun tail -> value::tail)) map

module String =
    let split (sep : string) (str : string) : string array = str.Split(sep)
