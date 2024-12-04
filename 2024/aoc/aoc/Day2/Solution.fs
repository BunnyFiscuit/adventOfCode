module aoc.Day2.Solution
open aoc.Utility
let day = "2"

let processInput =
    let lines = readLines 2 false
    lines |> Array.map(fun line ->
        line.Split([|' '; '\t'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map int)
        |> Array.toList

let isAllDecrease (arr: int list) =
    let rec loop (arr: int list) =
        match arr with
        | [] -> true
        | _::[] -> true
        | x::y::xs -> if x > y && x-y <= 3 then loop(y::xs) else false
    loop arr

let isAllIncrease (arr: int list) =
    let rec loop (arr: int list) =
        match arr with
        | [] -> true
        | _::[] -> true
        | x::y::xs -> if x < y && y-x <= 3 then loop (y::xs) else false
        
    loop arr

let allIncreasing (arr: int list) =
    arr |> List.pairwise |> List.forall (fun (x, y) -> x < y && y-x <= 3 && y-x > 0)
    
let allDecreasing (arr: int list) =
    arr |> List.pairwise |> List.forall (fun (x, y) -> x > y && x-y <= 3 && x-y > 0)
    
let isAllDecreaseOrIncrease (arr: int list) = allIncreasing arr || allDecreasing arr

let solve1 =
    processInput
    |> List.filter (fun x -> isAllDecreaseOrIncrease x)
    |> List.length

let isAllDecrease2 (arr: int list) =
    let rec loop (arr2: int list) (badfound: bool) =
        match arr2 with
        | [] -> true
        | _::[] -> true
        | x::y::xs ->
            // Regular progression: x -> y is valid
            if x > y && x - y > 0 && x - y <= 3 then loop (y::xs) badfound
            // Skip the second element: x -> z
            elif not badfound then
                if allDecreasing (y::xs) then true
                else loop (x::xs) true
            else
                //printfn "❌ DECR: %A - %A" arr arr2
                false
    loop arr false
    
let isAllIncrease2 (arr: int list) =
    let rec loop (arr2: int list) (badfound: bool) =
        match arr2 with
        | [] -> true
        | _::[] -> true
        | x::y::xs ->
            // Regular progression: x -> y is valid
            if x < y && y - x > 0 && y - x <= 3 then loop (y::xs) badfound
            // Skip the second element: x -> z
            elif not badfound then
                if allIncreasing (y::xs) then true
                else loop (x::xs) true
            else
                //printfn "❌ INCR: %A - %A" arr arr2
                false
    loop arr false

let solve2 =
    processInput
    |> List.filter (fun x ->
        //printfn "Checking %A" x
        let decCheck = isAllDecrease2 x
        //if decCheck then printfn "✅ DECR: %A" x
        let incCheck = isAllIncrease2 x
        //if incCheck then printfn "✅ INCR: %A" x
        //printfn ""
        decCheck || incCheck)
    |> List.length