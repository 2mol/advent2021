module Day06

open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input6.txt"
    |> Seq.head
    |> fun str -> str.Split(",")
    |> Array.map uint64

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

let rec countN (n : int) (fish : int seq) : uint64 =
    let (div, rem) = System.Math.DivRem(n, 7)
    if div = 0 then
        let a = Seq.length (Seq.filter (fun n -> n - rem < 0) fish)
        let b = Seq.length fish
        uint64 a + uint64 b
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

    [3;4;3;1;2]
    // |> applyN 18 step
    // input
    |> countN 80
    // |> Seq.length
    |> sprintf "%A"


let rec countGroupCopies (n : int) : int  =
    // if n < 9 then
    //     0 // not yet one "full" copy of the group
    // else
    //     1 + countGroupCopies (n - 9)
    max (n/7) 0

let rec copiesGenX (fish : int seq) (n : int) : int  =
    let (div, rem) = System.Math.DivRem(n, 7)
    let groupCopies = max div 0

    if groupCopies = 0 then
        5 + reproduceN rem fish
    else
        let offSpringCount =
            Array.zeroCreate groupCopies
            |> Array.mapi (
                fun groupIdx _ -> n - (groupIdx * 9)
            )
            |> Array.map (copiesGenX fish)
        Array.sum offSpringCount


    // let offspringOffsets =
    //     [1..999]
    //     |> List.map (fun groupNumber -> groupNumber * 9)
    //     |> List.map (fun offset -> n - offset)

    // let (div, rem) = System.Math.DivRem(n, 7)
    // if gen = 0 then
    //     max div 0
    // else
    // else
    //     copiesGenX (n - gen * 9) (gen + 1)




// let countCopiesTrans n =
//     List.map (copiesGenX n) [0..10]
    // |> List.map (
    //     fun timesReproduced, residue -> 5*(1+timesReproduced)
    // )
    // List.map (fun x -> x % 7) [n.. -9 ..1]
    // let generationCounter generation =
    //     copiesPrimary (n - generation * 9)
    // let countGenerationFish (nCopies, remainder) =
    //     5 * nCopies + (reproduceN remainder [3;4;3;1;2] - 5)
    // List.map generationCounter [0..n/9-1]
    // |> List.map countGenerationFish
    // |> List.sum
    // |> (+) 5 // add the original list

    // 1 + copiesPrimary n + (copiesPrimary (n - 9)) +

// let rec countN' (n : int) (fish : int seq) : uint64 =
//     let (div, rem) = System.Math.DivRem(n, 7)
//     if div = 0 then
//         let a = Seq.length (Seq.filter (fun n -> n - rem < 0) fish)
//         let b = Seq.length fish
//         uint64 a + uint64 b
//     else
//         // countN (n - 7) fish + countN (n - 9) fish
//         let origCopies = pown 2UL (div + 1)
//         let origResidue = Seq.length (Seq.filter (fun n -> n - rem < 0) fish)
//         let origTotal = (uint64)(Seq.length fish) * origCopies + (uint64)origResidue
//         0UL

let solution2 =
    // [3;4;3;1;2]
    // input
    countCopiesTrans 18  // + remainders
    |> sprintf "%A"
