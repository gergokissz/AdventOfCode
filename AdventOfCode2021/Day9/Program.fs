open System
open System.IO

module Day9 =

    let readMapFromFile path = 
        File.ReadAllLines(path)
        |> Seq.map (fun line -> 
            [| for c in line.ToCharArray() do int (string c) |])
        |> Seq.toArray

    let findLowPointValues (map: int[][]) = [

        let isLowPoint i j (map: int[][]) =
            let left = (i = 0) || (map[i - 1][j] > map[i][j])
            let right = (i = map.Length - 1) || (map[i + 1][j] > map[i][j])
            let up = (j = 0) || (map[i][j - 1] > map[i][j])
            let down = (j = map[i].Length - 1) || (map[i][j + 1] > map[i][j])
            left && up && right && down

        for i = 0 to map.Length - 1 do    
            for j = 0 to map[i].Length - 1 do
                if map |> isLowPoint i j then
                    yield map[i][j]
    ]

    let findBasinSizes (map: int[][]) = [

        let rec basinSize i j size (map: int[][]) =
            if i < 0 || i > map.Length - 1 || j < 0 || j > map[i].Length - 1 then
                size // This point is not on the map
            else if map[i][j] < 0 then
                size // This point is already part of a basin
            else if map[i][j] = 9 then
                size // This point is a high point
            else
                map[i][j] <- -1 // Indicate that this point is already part of a basin                
                let left = map |> basinSize (i - 1) j size
                let right = map |> basinSize (i + 1) j size
                let up = map |> basinSize i (j - 1) size
                let down = map |> basinSize i (j + 1) size
                size + left + right + up + down + 1

        for i = 0 to map.Length - 1 do
            for j = 0 to map[i].Length - 1 do
                let size = map |> basinSize i j 0
                if size > 0 then
                    yield size
    ]

let sumOfRiskLevels =
    Day9.readMapFromFile "input.txt"
    |> Day9.findLowPointValues
    |> List.sumBy (fun x -> x + 1)

let prodOfTopThreeBasinSizes =
    Day9.readMapFromFile "input.txt"
    |> Day9.findBasinSizes
    |> List.sortDescending
    |> List.take 3
    |> List.fold (*) 1

printfn "%i" sumOfRiskLevels // 417
printfn "%i" prodOfTopThreeBasinSizes // 1148965