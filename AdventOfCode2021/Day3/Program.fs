open System
open System.IO

let lines = File.ReadAllLines("input.txt")

// Part 1 (needed some refactoring probably)

let countBits (state) (current: string) =
    (state, current.ToCharArray()) ||> Seq.map2 (fun s c -> if c = '1' then s + 1 else s - 1)

let foldToGammaAndEpsilon state (current: int) =
    if current > 0 then ( fst state + "1", snd state + "0" )
    else if current < 0 then ( fst state + "0", snd state + "1" )
    else failwith "Invalid state"

let result = 
    lines 
    |> Seq.fold countBits (Seq.initInfinite (fun _ -> 0)) 
    |> Seq.fold foldToGammaAndEpsilon ("", "")

let gamma = Convert.ToUInt32(fst result, 2)
let epsilon = Convert.ToUInt32(snd result, 2)

printfn "%i" (gamma * epsilon)

// Part 2

// Group lines by character at index, sort the groups by the number of occurrences (+ the character to handle equality)
// and select one group by the selector (e.g. the first or the last - for most and least common bits respectively)
let findLines selector (index: int) (lines : seq<string>) =
    lines 
    |> Seq.groupBy (fun line -> line[index]) 
    |> Seq.sortBy (fun (bit, lines) -> lines |> Seq.length, bit)
    |> selector
    |> snd

// Apply filter by increasing index until the sequence contains one item only
let rec whileNotSingleton (filter: int -> seq<string> -> seq<string>) (index: int) (lines: seq<string>) =
    let linesNext = filter index lines
    if Seq.length linesNext > 1 then
        whileNotSingleton filter (index + 1) linesNext
    else
        Seq.last linesNext

// Get O2 and CO2 values by using Seq.last (most common bit) and Seq.head (least common bit) as selector
let temp1 = lines |> whileNotSingleton (findLines Seq.last) 0
let temp2 = lines |> whileNotSingleton (findLines Seq.head) 0
   
let o2 = Convert.ToUInt32(temp1, 2)
let co2 = Convert.ToUInt32(temp2, 2)

printfn "%i" (o2 * co2)