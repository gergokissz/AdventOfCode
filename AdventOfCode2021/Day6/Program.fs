open System.IO

let period = 7  // Number of days to create a new lanternfish
let delay = 2   // Number of days till new generation starts first period

let readFile = 
    File.ReadAllLines("input.txt") 
    |> Seq.map (fun line -> line.Split(',')) 
    |> Seq.concat
    |> Seq.map int

let initPopulation numbers = 
    numbers
    |> Seq.fold (fun (array: uint64[]) x -> 
        array |> Array.updateAt x (array[x] + 1UL))
        (Array.create (period + delay) 0UL)

let rec evolvePopulation days array =

    let evolve1 (population: uint64[]) =
        let oldestCount = population[0]
        for i = 0 to population.Length - 2 do
            population[i] <- population[i + 1]
        population[population.Length - delay - 1] <- population[population.Length - delay - 1] + oldestCount
        population[population.Length - 1] <- oldestCount
        population

    match days with
    | 0 -> array
    | i -> evolvePopulation (i - 1) (array |> evolve1)

printfn "%A" (readFile |> initPopulation |> evolvePopulation 80 |> Array.sum)   // 360268
printfn "%A" (readFile |> initPopulation |> evolvePopulation 256 |> Array.sum)  // 1632146183902