open System
open System.IO

module Bracket =

    type BracketShape = Round | Square | Curly | Angle

    type BracketDirection = Opening | Closing

    type BracketType = { Shape: BracketShape; Direction: BracketDirection; Value: uint64 }

    let parse character =
        match character with
        | '(' -> { Shape = Round; Direction = Opening; Value = 1UL }
        | '[' -> { Shape = Square; Direction = Opening; Value = 2UL }
        | '{' -> { Shape = Curly; Direction = Opening; Value = 3UL }
        | '<' -> { Shape = Angle; Direction = Opening; Value = 4UL }
        | ')' -> { Shape = Round; Direction = Closing; Value = 3UL }
        | ']' -> { Shape = Square; Direction = Closing; Value = 57UL }
        | '}' -> { Shape = Curly; Direction = Closing; Value = 1197UL }
        | '>' -> { Shape = Angle; Direction = Closing; Value = 25137UL }
        | c -> failwith $"Invalid character: '{c}'"

module Day10 =

    let readLinesFromFile path =
        File.ReadAllLines(path)
        |> Seq.map (fun line -> [ for c in line.ToCharArray() do c ])

    type LineEvaluationResult =
        | Correct
        | Incomplete of uint64
        | Corrupted of uint64

    let rec evaluate stack characters = 
        match characters with
        | head :: tail -> 
            let bracket = Bracket.parse head
            if bracket.Direction = Bracket.Opening then
                // Opening bracket => push and continue
                tail |> evaluate (bracket :: stack)
            else
                // Closing bracket => pop and validate
                match stack with
                | top :: rest ->                    
                    if top.Shape = bracket.Shape then
                        // Valid closing bracket => continue
                        tail |> evaluate rest
                    else
                        // Invalid closing bracket => corrupted line
                        Corrupted bracket.Value
                | [] -> failwith("Stack is empty")
        | [] -> 
            if stack.Length = 0 then
                // Stack is empty => correct line
                Correct
            else
                // Stack is not empty => incomplete line
                Incomplete (stack |> List.fold (fun score bracket -> score * 5UL + bracket.Value) 0UL)

let corruptedLinesTotalScore =
    Day10.readLinesFromFile "input.txt"
    |> Seq.map (fun characters -> characters |> Day10.evaluate [])
    |> Seq.map (fun result ->
        match result with
        | Day10.Corrupted score -> score
        | _ -> 0UL)
    |> Seq.sum

let incompleteLinesTotalScored =
    Day10.readLinesFromFile "input.txt"
    |> Seq.map (fun characters -> characters |> Day10.evaluate [])
    |> Seq.map (fun result ->
        match result with
        | Day10.Incomplete score -> score
        | _ -> 0UL)
    |> Seq.filter (fun score -> score > 0UL)
    |> Seq.sort
    |> Seq.toList
    |> fun scores -> scores[scores.Length / 2]

printfn "%i" corruptedLinesTotalScore   // 299793
printfn "%i" incompleteLinesTotalScored // 3654963618