open System.IO

let crabPositions =
    File.ReadAllLines("input.txt")
    |> Seq.map (fun line -> line.Split(','))
    |> Seq.head
    |> Seq.map int
    |> Seq.toList

let minimalMovementCost (distanceToCost: int -> int) crabPositions =
    [0 .. crabPositions |> Seq.max]
    |> Seq.map (fun targetPosition ->
        crabPositions
        |> Seq.sumBy (fun sourcePosition -> distanceToCost (abs (sourcePosition - targetPosition))))
    |> Seq.min

let cost1 = crabPositions |> minimalMovementCost (fun distance -> distance)
let cost2 = crabPositions |> minimalMovementCost (fun distance -> distance * (distance + 1) / 2)

printfn "%i" cost1 // 345197
printfn "%i" cost2 // 96361606
