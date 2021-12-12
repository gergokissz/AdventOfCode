open System
open System.IO

module Day11 =

    type Octopus = { EnergyLevel: int; Flashed: bool; FlashCount: int }

    type OctopusMap = { Octopuses: Octopus[][] } with
        
        member private this.tryEnergize i j =
            if (i >= 0 && j >= 0 && i <= this.Octopuses.Length - 1 && j <= this.Octopuses[i].Length - 1) then
                this.Octopuses[i][j] <- { this.Octopuses[i][j] with EnergyLevel = (this.Octopuses[i][j]).EnergyLevel + 1 }

        member private this.energize =
            for i = 0 to this.Octopuses.Length - 1 do
                for j = 0 to this.Octopuses[i].Length - 1 do
                    this.tryEnergize i j

        member private this.flash =
            let mutable hasAnyFlashed = false
            for i = 0 to this.Octopuses.Length - 1 do
                for j = 0 to this.Octopuses[i].Length - 1 do
                    if (this.Octopuses[i][j]).EnergyLevel > 9 && not (this.Octopuses[i][j]).Flashed then
                        this.Octopuses[i][j] <- { this.Octopuses[i][j] with FlashCount = (this.Octopuses[i][j]).FlashCount + 1; Flashed = true }
                        this.tryEnergize (i - 1) j
                        this.tryEnergize (i + 1) j
                        this.tryEnergize i (j - 1)
                        this.tryEnergize i (j + 1)
                        this.tryEnergize (i - 1) (j - 1)
                        this.tryEnergize (i - 1) (j + 1)
                        this.tryEnergize (i + 1) (j - 1)
                        this.tryEnergize (i + 1) (j + 1)
                        hasAnyFlashed <- true
            hasAnyFlashed

        member private this.reset =
            let mutable hasAllFlashed = true
            for i = 0 to this.Octopuses.Length - 1 do
                for j = 0 to this.Octopuses[i].Length - 1 do
                    if (this.Octopuses[i][j]).EnergyLevel > 9 then
                         this.Octopuses[i][j] <- { this.Octopuses[i][j] with EnergyLevel = 0; Flashed = false }
                    else
                        hasAllFlashed <- false
            hasAllFlashed

        member this.step =
            this.energize
            while this.flash do ()
            this.reset

    let readMapFromFile path =

        let parse character = { EnergyLevel = int (string character); Flashed = false; FlashCount = 0 }

        File.ReadAllLines(path)
        |> Seq.map (fun line -> [| for c in line.ToCharArray() do parse c |])
        |> Seq.toArray
        |> fun octopuses -> { Octopuses = octopuses; }

// Part 1

let map1 = Day11.readMapFromFile "input.txt"

for _ = 1 to 100 do map1.step |> ignore

let result1 =
    map1.Octopuses
    |> Seq.sumBy (fun row ->
        row
        |> Seq.sumBy (fun octopus ->
            octopus.FlashCount))

printfn "%i" result1 // 1688

// Part 2

let map2 = Day11.readMapFromFile "input.txt"

let rec stepWhileAllFlashes i (map: Day11.OctopusMap) =
    match map.step with
    | false -> map |> stepWhileAllFlashes (i + 1)
    | true -> i

let result2 = stepWhileAllFlashes 1 map2

printfn "%i" result2 // 403