// ----------- ~ Functions that should be in the standard library ~ -----------

module Array =
    let foldi
        (folder: (int -> 'State -> 'T -> 'State))
        (state : 'State)
        (array : 'T array)
        : 'State
        =
        Array.indexed array
        |> Array.fold (fun state (i, a) -> folder i state a) state

module Map =
    let merge m1 m2 =
        List.concat [Map.toList m1; Map.toList m2] |> Map.ofList

    let reverse (m: Map<'Key,'T>) : Map<'T, 'Key> =
        Map.toList m
        |> List.map (fun (a,b) -> b,a)
        |> Map.ofList

    let insertOption (key : 'Key) (value : 'T option) (m: Map<'Key,'T>) : Map<'Key,'T> =
        match value with
        | Some v -> Map.add key v m
        | None -> m

module String =
    let split (sep : string) (str : string) : string array = str.Split(sep)
