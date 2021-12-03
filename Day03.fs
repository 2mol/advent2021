module Day03

open System
open System.IO

#nowarn "25"

let input =
    File.ReadLines "inputs/input3.txt"
    |> Seq.toList

let MeasurementLength = 12

let aggregate (counter : int array) (str : string) =
    Array.zip counter (Seq.toArray str)
    |> Array.map (
        fun (count, digit) ->
            match digit with
            | '1' -> count + 1
            | _ -> count
        )

let solution1 =
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

let solution2 =
    [solutionHelper 0 OxygenGenerator input; solutionHelper 0 CO2Scrubber input]
    |> List.map (fun b -> Convert.ToInt32(b, 2))
    |> List.fold (*) 1
    |> sprintf "%A"
