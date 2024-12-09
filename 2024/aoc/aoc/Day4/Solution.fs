module aoc.Day4.Solution
open System
open aoc.Utility
let day = "4"

let processInput =
    let lines = readLines 4 true
    lines |> Array.map(fun line -> line)
    |> Array.toList

let checkForwards (str: string) (pos: int*int) =
    Seq.skip (fst pos) str
    |> Seq.toArray
    |> String

let solve1 =
    let xs = processInput
    let cols = xs.Head.Length
    let rows = xs.Length
    printfn "rows: %d, cols: %d" rows cols
    for i in 0..rows-1 do
        for j in 0..cols-1 do
            let pos = (j, i)
            let str = xs.[i]
            let forward = checkForwards str pos
            printfn "forward: %s" forward

let solve2 = "2"
