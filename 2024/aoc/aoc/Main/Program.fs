module Program
open aoc.Day4.Solution

[<EntryPoint>]
let main args =
    printfn $"Advent of Code - Day {day}"
    printfn ""
    
    let solution1 = solve1
    printfn "Part 1:"
    printfn "%A" solution1
    printfn ""
    
    let solution2 = solve2
    printfn "Part 2:"
    printfn "%A" solution2
    0