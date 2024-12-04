module aoc.Day3.Solution
open System.Text.RegularExpressions
open aoc.Utility
let day = "3"

let processInput =
    let lines = readLines 3 false
    lines |> Array.map(fun line -> line)
    |> Array.toList
    
let getValues (mul: string) =
    let ps = mul.Replace("mul", "").Replace("(","").Replace(")","").Split(",")
    (int ps.[0], int ps.[1])

let solve1 =
    processInput
    |> List.map(fun line -> Regex.Matches(line, "mul[(]\d{1,3},\d{1,3}[)]"))
    |> List.map(fun m -> m |> Seq.map(fun x -> x.Value) |> Seq.toList)
    |> List.concat
    |> List.map(fun mul -> getValues mul)
    |> List.map(fun (x, y) -> x * y)
    |> List.sum

let filterMatches (arr: string list) =
    let rec loop (arr: string list) (pick: bool) =
        match arr with
        | [] -> []
        | "do()" :: xs -> loop xs true
        | "don't()" :: xs -> loop xs false
        | x :: xs ->
            if pick then x::loop xs pick
            else loop xs pick
    loop arr true

let solve2 =
    processInput
    |> List.map(fun line -> Regex.Matches(line, "mul[(]\d{1,3},\d{1,3}[)]|do[(][)]|don't[(][)]")) 
    |> List.map(fun m -> m |> Seq.map(fun x -> x.Value) |> Seq.toList)
    |> List.concat
    |> filterMatches
    |> List.map(fun mul -> getValues mul)
    |> List.map(fun (x, y) -> x * y)
    |> List.sum