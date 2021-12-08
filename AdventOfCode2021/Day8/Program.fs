open System
open System.IO

module SevenSegment =

    type Note = { Patterns: seq<Set<char>>; Digits: seq<Set<char>> }

    let readNotesFromFile path =

        let splitToCharSets (text: string) =
            text.Trim().Split(' ') |> Seq.map (fun word -> set (word.ToCharArray()))

        File.ReadAllLines(path)
        |> Seq.map (fun line ->
            let parts = line.Split('|')
            let patterns = parts[0] |> splitToCharSets
            let digits = parts[1] |> splitToCharSets
            { Patterns = patterns; Digits = digits })

    let decodeNoteDigits note = 

        // Patterns 1, 4, 7 and 8 are trivial based on their length
        let pattern1 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 2)
        let pattern4 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 4)
        let pattern7 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 3)
        let pattern8 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 7)
        // Define pattern bd as: pattern 4 \ pattern 7
        let setBD = pattern7 |> Set.difference pattern4
        // Define pattern eg as: pattern 8 \ pattern 4 \ pattern 7
        let setEG = pattern7 |> Set.difference pattern4 |> Set.difference pattern8
        // Pattern 3 is of length 5 and contains pattern 1
        let pattern3 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 5 && (pattern1 |> Set.isSuperset p))
        // Pattern 6 is of length 6 and does not contain pattern 1
        let pattern6 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 6 && not (pattern1 |> Set.isSuperset p))
        // Pattern 5 is of length 5 and contains pattern bd (and it is not pattern 3)
        let pattern5 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 5 && (p <> pattern3) && (setBD |> Set.isSuperset p))
        // Pattern 2 is of length 5 and does not contain pattern bd (and it is not pattern 3)
        let pattern2 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 5 && (p <> pattern3) && not (setBD |> Set.isSuperset p))
        // Pattern 0 is of length 6 and contains pattern eg (and it is not pattern 6)
        let pattern0 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 6 && (p <> pattern6) && (setEG |> Set.isSuperset p))
        // Pattern 9 is of length 6 and does not contain pattern eg (and it is not pattern 6)
        let pattern9 = note.Patterns |> Seq.find (fun p -> (p |> Set.count) = 6 && (p <> pattern6) && not (setEG |> Set.isSuperset p)) 

        let matchWithPattern digit =
            match digit with
            | _ when digit = pattern0 -> "0"
            | _ when digit = pattern1 -> "1"
            | _ when digit = pattern2 -> "2"
            | _ when digit = pattern3 -> "3"
            | _ when digit = pattern4 -> "4"
            | _ when digit = pattern5 -> "5"
            | _ when digit = pattern6 -> "6"
            | _ when digit = pattern7 -> "7"
            | _ when digit = pattern8 -> "8"
            | _ when digit = pattern9 -> "9"
            | _ -> failwith "Invalid digit"

        int (note.Digits |> Seq.fold (fun result digit -> result + (digit |> matchWithPattern)) "")

let result1 = 
    SevenSegment.readNotesFromFile "input.txt"
    |> Seq.map (fun note -> note.Digits)
    |> Seq.concat
    |> Seq.where (fun digit ->[ 2; 3; 4; 7 ] |> Seq.contains (digit |> Set.count))
    |> Seq.length

let result2 =
    SevenSegment.readNotesFromFile "input.txt"
    |> Seq.map (fun note -> note |> SevenSegment.decodeNoteDigits)
    |> Seq.sum

printfn "%i" result1 // 476
printfn "%i" result2 // 1011823