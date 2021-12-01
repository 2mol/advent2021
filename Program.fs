let solutions : list<string * string> =
    [
        "day 1-1", Day01.solution1
        "day 1-2", Day01.solution2
    ]

for day, solution in solutions do
    printfn "%s: %s" day solution
