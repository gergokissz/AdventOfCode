open System
open System.IO
open System.Collections.Generic

module Day15 = 

    type Point = { I: int; J: int } 

    let readRiskLevelsFromFile path = 
        File.ReadAllLines(path)
        |> Array.map (fun line -> [| for c in line.ToCharArray() do int (string c) |])

    let createRiskLevelMap (riskLevels: int[][]) scale =
        [|
            let h = riskLevels.Length
            let w = riskLevels[0].Length
            for i = 0 to h * scale - 1 do
                yield [|
                    for j = 0 to w * scale - 1 do
                        let riskLevel = riskLevels[i % h][j % w] + i / h + j / w
                        yield uint64 ((riskLevel - 1) % 9 + 1)
                |]
        |]

    let getLowestTotalRisk (riskLevelMap: uint64[][]) = 

        let source = { I = 0; J = 0 }
        let dest = { I = riskLevelMap.Length - 1; J = riskLevelMap[0].Length - 1 }

        let getNeigbours (point: Point) =
            [
                if point.I > source.I then yield { point with I = point.I - 1 }
                if point.I < dest.I then yield { point with I = point.I + 1 }
                if point.J > source.J then yield { point with J = point.J - 1 }
                if point.J < dest.J then yield { point with J = point.J + 1 }
            ]

        // See "Dijkstra's algorithm" on Wikipedia: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

        let Q = new PriorityQueue<Point, uint64>();
        Q.Enqueue(source, 0UL)

        let dist = new Dictionary<Point, uint64>();
        dist.Add(source, 0UL)

        let rec whileNotDestFound() =
            let u = Q.Dequeue()
            if (u <> dest) then
                for v in u |> getNeigbours do
                    let alt = dist[u] + riskLevelMap[v.I][v.J]
                    if alt < dist.GetValueOrDefault(v, UInt64.MaxValue) then
                        dist[v] <- alt
                        Q.Enqueue(v, alt)
                whileNotDestFound()
        
        whileNotDestFound()

        dist[dest]

let riskLevels = Day15.readRiskLevelsFromFile "input.txt"

let result1 = Day15.createRiskLevelMap riskLevels 1 |> Day15.getLowestTotalRisk
let result2 = Day15.createRiskLevelMap riskLevels 5 |> Day15.getLowestTotalRisk

printfn "%i" result1 // 388
printfn "%i" result2 // 2819