module aoc.Utility
open System.IO

// readLines : int -> bool -> string array
let readLines day test =
    let path = if test then $"Day{day}/test.txt" else $"Day{day}/input.txt"
    File.ReadAllLines(path)
 
let readTest day = 
    File.ReadAllLines($"Day{day}/test.txt")