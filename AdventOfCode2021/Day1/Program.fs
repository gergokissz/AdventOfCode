open System.IO

let input = 
    File.ReadAllLines(@"input.txt")
    |> Seq.map int

let isIncrement (first, second) =
    second > first

let increments1 = 
    input
    |> Seq.pairwise 
    |> Seq.where isIncrement
    |> Seq.length

printfn "Increments1: %i" increments1

let increments3 = 
    input
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> Seq.pairwise 
    |> Seq.where isIncrement
    |> Seq.length

printfn "Increments3: %i" increments3