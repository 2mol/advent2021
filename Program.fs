open BenchmarkDotNet.Running

// let solutions : list<string * string> =
//     [
//         // "day 1-1", Day01.solution1
//         // "day 1-2", Day01.solution2
//         // "day 2-1", Day02.solution1
//         // "day 2-2", Day02.solution2
//         "day 3-1", Day03.solution1
//         "day 3-2", Day03.solution2
//     ]

// for day, solution in solutions do
//     printfn "%s: %s" day solution

[<EntryPoint>]
let main _ =
    let summary = BenchmarkRunner.Run<Day03.Benchmark2>()
    printfn "%A" summary

    0 // return an integer exit code
