open System
open System.IO

module Day14 =

    // Map char to int like A=0, B=1, C=2, ...
    let toIndex (character: char) = (int character) - 65
    
    // Map char pairs to int pairs like AA=00, BB=11, CC=22, ...
    type Pair = { I: int; J: int }

    type Input = { PolymerTemplate: char[];  PairInsertionRules: Map<Pair, Pair * Pair> }

    let toPair (characters: char * char) = 
        { I = fst characters |> toIndex; J = snd characters |> toIndex }

    let readInputFromFile path =

        let toPairInsertionRule (line: string) =
            let pair = (line[0], line[1]) |> toPair
            let insert1 = (line[0], line[6]) |> toPair
            let insert2 = (line[6], line[1]) |> toPair
            (pair, (insert1, insert2))

        let lines = File.ReadAllLines path
        let polymerTemplate = lines[0].ToCharArray()
        let pairInsertionRules = Map [ for rule in lines |> Array.skip 2 |> Array.map (fun line -> line |> toPairInsertionRule) do yield rule ]
        { PolymerTemplate = polymerTemplate; PairInsertionRules = pairInsertionRules }

    let solve iterationCount input =

        // Creates a 25x25 uint64[][] where the first index represents the first char of a pair,
        // the second index represents the second char of a pair and the value is the occurrences
        // of the pair within the polymer. After creation we initialize the array with the pairs in
        // the polymer template.
        let initializePolymer polymerTemplate =
            let polymer = Array.init 25 (fun _ -> Array.create 25 0UL)
            polymerTemplate
            |> Array.pairwise
            |> Array.iter (fun characters ->
                let pair = characters |> toPair
                polymer[pair.I][pair.J] <- polymer[pair.I][pair.J] + 1UL)
            polymer

        // Applies the pair insertion rules multiple times to the given polymer                
        let rec applyPairInsertionRules iterationCount pairInsertionRules polymer = 

            let applyPairInsertionRulesOnce pairInsertionRules (polymer: uint64[][]) =
                let ret = Array.init 25 (fun _ -> Array.create 25 0UL)
                for i = 0 to polymer.Length - 1 do
                    for j = 0 to polymer[i].Length - 1 do
                        match pairInsertionRules |> Map.tryFind { I = i; J = j } with
                        | Some rule -> 
                            let count = polymer[i][j]
                            ret[(fst rule).I][(fst rule).J] <- ret[(fst rule).I][(fst rule).J] + count
                            ret[(snd rule).I][(snd rule).J] <- ret[(snd rule).I][(snd rule).J] + count
                        | None -> ret[i][j] <- polymer[i][j]
                ret

            if iterationCount > 0 then
                polymer 
                |> applyPairInsertionRulesOnce pairInsertionRules
                |> applyPairInsertionRules (iterationCount - 1) pairInsertionRules 
            else 
                polymer

        // After applying the rules the last character of the polymer template needs to be added once
        let appendCharacter character (polymer: uint64[][]) =
            polymer[character |> toIndex][0] <- polymer[character |> toIndex][0] + 1UL
            polymer
        
        // Initialize the polymer, apply the pair insertion rules, then append the last character once.toIndex
        // The resulting polymer array contains the total number of each character pair in the polymer.
        // We can get the total count of each character by calculating the sum of each row in the table.
        let counts = 
            initializePolymer input.PolymerTemplate 
            |> applyPairInsertionRules iterationCount input.PairInsertionRules 
            |> appendCharacter (input.PolymerTemplate |> Array.last)
            |> Array.map Array.sum
            |> Array.where (fun value -> value > 0UL)
            |> Array.sort

        (counts |> Array.last) - (counts |> Array.head)

let input = Day14.readInputFromFile "input.txt"

let result1 = input |> Day14.solve 10
let result2 = input |> Day14.solve 40

printfn "%i" result1 // 3906
printfn "%i" result2 // 4441317262452
