module Day06

open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input6.txt"
    |> Seq.head
    |> fun str -> str.Split(",")
    |> Array.map int

let step (fish : int seq) : int seq =
    let newFish =
        fish
        |> Seq.filter ((=) 0)
        |> Seq.map (fun _ -> 8)
    let agedFish =
        fish
        |> Seq.map (fun n -> if n = 0 then 6 else n - 1)
    Seq.append agedFish newFish

let applyN (n : int) (f: 'T -> 'T) (initialState : 'T) : 'T =
    // Note: applyN 0 will naturally just return initialState.
    List.fold (fun state _ -> f state) initialState [1..n]

let rec reproduceN (n : int) (fish : int seq) : int =
    let (div, rem) = System.Math.DivRem(n, 7)
    if div = 0 then
        // implies n < 7
        applyN n step fish
        |> Seq.length
    else if div = 1 then
        // exactly one full cycle, plus some remainder
        // implies n < 14
        // => then we have one full copy of `fish`, plus...? (a shifted copy, plus ITS offspring)
        let g1 = Seq.length (applyN rem step fish)
        let g2 = Seq.length (applyN rem step (Seq.map ((+) 2) fish))
        g1 + g2
    else
        // we are AT LEAST doing 2 full cycles
        // List.map (???) [div.. -1 .. 0]

        // div * (reproduceN (n - 7) fish)
        if rem <= 2 then
            Seq.length fish
        else
            // evolve the first offspring group
            applyN (rem - 2) step fish |> Seq.length

let rec countN (n : int) (fish : int seq) : int =
    let (div, rem) = System.Math.DivRem(n, 7)
    if div = 0 then
        let a = Seq.length (Seq.filter (fun n -> n - rem < 0) fish)
        let b = Seq.length fish
        a + b
    else if div = 1 then
        // Seq.map () (seq {})
        countN rem fish + countN (n - 7 * div - 2) fish
    else
        countN (n - 7) fish + countN (n - 9) fish


let solution1 =
    // first generation evolution:
    // 1. (div) how many full cycles did the group do?
    //    -> there are this many offspring groups of the original group
    // 2. (mod) what is left, i.e. how many additional steps do we evolve the group
    // let (div, rem) = System.Math.DivRem(80, 7)
    // 3. After a full cycle, there is a copy of the group
    //    BUT WITH 2 added to each of their counts!
    // 4. this new group should evolve 2 more times, and then it's
    //    a copy of the original group again.

    // [3;4;3;1;2]
    // |> applyN 18 step
    input
    |> countN 80
    // |> Seq.length
    |> sprintf "%A"



let calcResidual (fish : int seq) (rem : int) =
    Seq.filter (fun n -> n - rem < 0) fish
    |> Seq.length

let rec solve (cache : Map<int, uint64>) (fish : int seq) (n : int) : uint64 * Map<int, uint64> =
    match Map.tryFind n cache with
        | Some value ->
            value, cache
        | None ->
            let (div, rem) = System.Math.DivRem(n, 7)
            let groupCopies = max div 0

            let selfPlusResidual = (Seq.length fish) + calcResidual fish rem

            let folder (acc, subCache) newN =
                let (subResult, subSubCache) = solve subCache fish newN
                let combinedCache =
                    Map.fold (fun acc key value -> Map.add key value acc) subCache subSubCache

                acc + subResult, combinedCache

            let children, newCache =
                Array.zeroCreate groupCopies
                |> Array.mapi (fun i _ -> n - 9 - 7 * i)
                |> Array.rev
                |> Array.fold folder (0UL, cache)

            let solution = (uint64) selfPlusResidual + (uint64) children

            solution, Map.add n solution newCache

let solution2 =
    // let input = [3;4;3;1;2]
    solve Map.empty input 256
    |> fst // + remainders
    |> sprintf "%i"
