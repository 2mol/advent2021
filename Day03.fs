module Day03

open System
open System.IO

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

#nowarn "25"

// let input =
//     File.ReadLines "inputs/input3.txt"
//     |> Seq.toList

let MeasurementLength = 12

let aggregate (counter : int array) (str : string) =
    Array.zip counter (Seq.toArray str)
    |> Array.map (
        fun (count, digit) ->
            match digit with
            | '1' -> count + 1
            | _ -> count
        )

let solution1 input =
    List.fold aggregate (Array.zeroCreate MeasurementLength) input
    |> Array.map (fun n -> n > List.length input / 2 )
    |> Array.map (fun b -> if b then "1" else "0")
    |> String.concat ""
    |> fun b -> Convert.ToInt32(b, 2)
    |> fun n -> n * (n ^^^ (pown 2 MeasurementLength - 1))
    |> sprintf "%A"


// --------------------------------------

type BitCriterion = OxygenGenerator | CO2Scrubber

let rec solutionHelper (pos : int) (criterion : BitCriterion) (measurements : string seq) =
    if Seq.length measurements = 1 then
        Seq.head measurements
    else
        let discriminator =
            match criterion with
            | OxygenGenerator -> Seq.maxBy
            | CO2Scrubber -> Seq.minBy

        let counter = Seq.groupBy (fun (str : string) -> str[pos]) measurements

        let equalOccurrences =
            Seq.map (snd >> Seq.length) counter
            |> Set.ofSeq
            |> Set.count
            |> (=) 1

        let subMeasurements =
            if equalOccurrences && criterion = OxygenGenerator then
                Map.ofSeq counter |> Map.find '1'
            else if equalOccurrences && criterion = CO2Scrubber then
                Map.ofSeq counter |> Map.find '0'
            else
                discriminator (snd >> Seq.length) counter |> snd

        solutionHelper (pos + 1) criterion subMeasurements

let solution2 input =
    [solutionHelper 0 OxygenGenerator input; solutionHelper 0 CO2Scrubber input]
    |> List.map (fun b -> Convert.ToInt32(b, 2))
    |> List.fold (*) 1
    |> sprintf "%A"

// ----------------------------------------------------------

let rec solutionHelperNoBitCriterion (pos : int) (desiredChar : char) (measurements : string seq) =
    if Seq.length measurements = 1 then
        Seq.head measurements
    else
        let discriminator =
            match desiredChar with
            | '1' -> Seq.maxBy
            | '0' -> Seq.minBy

        let counter = Seq.groupBy (fun (str : string) -> str[pos]) measurements

        let equalOccurrences =
            Seq.map (snd >> Seq.length) counter
            |> Set.ofSeq
            |> Set.count
            |> (=) 1

        let subMeasurements =
            if equalOccurrences then
                Map.ofSeq counter |> Map.find desiredChar
            else
                discriminator (snd >> Seq.length) counter |> snd

        solutionHelperNoBitCriterion (pos + 1) desiredChar subMeasurements

let solution2NoBitCriterion input =
    [solutionHelperNoBitCriterion 0 '1' input; solutionHelperNoBitCriterion 0 '0' input]
    |> List.map (fun b -> Convert.ToInt32(b, 2))
    |> List.fold (*) 1
    |> sprintf "%A"

// ----------------------------------------------------------

let rec solutionHelperOpt (pos : int) (desiredChar : char) (measurements : string seq) =
    if Seq.length measurements = 1 then
        Seq.head measurements
    else
        let counter = Seq.groupBy (fun (str : string) -> str[pos]) measurements

        let equalOccurrences =
            Seq.map (snd >> Seq.length) counter
            |> Set.ofSeq
            |> Set.count
            |> (=) 1

        match desiredChar, equalOccurrences with
        | _, true ->
            counter
            |> Seq.filter (fst >> (=) desiredChar)
            |> Seq.head
        | '0', _ ->
            Seq.minBy (snd >> Seq.length) counter
        | '1', _ ->
            Seq.maxBy (snd >> Seq.length) counter
        |> snd
        |> solutionHelperOpt (pos + 1) desiredChar

let solution2Opt input =
    [solutionHelperOpt 0 '1' input; solutionHelperOpt 0 '0' input]
    |> List.map (fun b -> Convert.ToInt32(b, 2))
    |> List.fold (*) 1
    |> sprintf "%A"

// ----------------------------------------------------------

[<MemoryDiagnoser>]
type Benchmark2 () =
    member __.Input =
        File.ReadLines "/Users/juri/code/advent2021/inputs/input3.txt"
        |> Seq.toList

    [<Benchmark(Baseline=true)>]
    member self.Solution2Initial() = solution2 self.Input

    [<Benchmark>]
    member self.Solution2NoBitCriterion() = solution2NoBitCriterion self.Input

    [<Benchmark>]
    member self.Solution2Opt() = solution2Opt self.Input
