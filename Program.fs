let solutions : list<string * string> =
    [
        "2020 day 1-1", Prev_Day01.solution1
        "2020 day 1-2", Prev_Day01.solution2
        "2020 day 2-1", Prev_Day02.solution1
        "2020 day 2-2", Prev_Day02.solution2
    ]

for day, solution in solutions do
    printfn "%s: %s" day solution
