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

// let solution1 =
//     List.fold (
//         fun fish i ->
//         printfn "at generation %i" i
//         step fish
//     ) input [1..80]
//     // step (step input) reduce
//     // |> Seq.length
//     |> toList
//     |> sprintf "%A"

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

    // let (div, rem) = System.Math.DivRem(n, 7)
    // if div = 0 then
    //     applyN rem step fish
    //     |> Seq.length
    // else
    //     let copiesOfOriginalGroup = div
    //     0

let rec countN (n : int) (fish : int seq) : int =
    let (div, rem) = System.Math.DivRem(n, 7)
    if div = 0 then
        Seq.length fish + Seq.length (Seq.filter (fun n -> n - rem < 0) fish)
    else if div = 1 then
        // Seq.map () (seq {})
        countN rem fish + countN (n - 7 * div - 2) fish
    else
        countN (n - 7) fish + countN (n - 9) fish

// 1 cycle -> double  og og
// 2 (1 more) -> quadr og og og og


let solution1 =
    // first generation evolution:
    // 1. (div) how many full cycles did the group do?
    //    -> there are this many offspring groups of the original group
    // 2. (mod) what is left, i.e. how many additional steps do we evolve the group
    let (div, rem) = System.Math.DivRem(80, 7)
    // 3. After a full cycle, there is a copy of the group
    //    BUT WITH 2 added to each of their counts!
    // 4. this new group should evolve 2 more times, and then it's
    //    a copy of the original group again.

    [3;4;3;1;2]
    // |> applyN 18 step
    input
    |> countN 80
    // |> Seq.length
    |> sprintf "%A"


let solution2 = ""
