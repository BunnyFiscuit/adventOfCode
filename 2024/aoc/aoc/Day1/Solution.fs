module aoc.Day1.Solution
open aoc.Utility
let day = "1"
let processInput =
    let lines = readLines 1 false
    let left, right =
        lines
        |> Array.map (fun line -> 
            let parts = line.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
            int parts.[0], int parts.[1])
        |> Array.unzip
    Array.toList left, Array.toList right

let solve1 =
    let left, right = processInput
    let ds =
        List.zip (List.sort left) (List.sort right)
        |> List.map (fun (l, r) -> abs (l - r))  
    List.sum ds
        

let solve2 =
    let left, right = processInput
    let ds =
        left
        |> List.map (fun n -> List.where (fun m -> m = n) right) |> List.map (fun ms -> List.sum ms)
    List.sum ds