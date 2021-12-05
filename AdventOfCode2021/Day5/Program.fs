open System
open System.IO
open System.Text.RegularExpressions

module Vector = 

    type PointType = { X: int; Y: int }

    type VectorType = { A: PointType; B: PointType } with
        member this.DX = this.B.X - this.A.X
        member this.DY = this.B.Y - this.A.Y
        member this.IsDiagonal = this.DX <> 0 && this.DY <> 0
    
    let parseFile path =

        let regex = Regex("^(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)$", RegexOptions.Compiled)

        let parseLine line =
            let m = regex.Match(line)
            if m.Success then
                let x1 = int m.Groups["x1"].Value
                let y1 = int m.Groups["y1"].Value
                let x2 = int m.Groups["x2"].Value
                let y2 = int m.Groups["y2"].Value        
                { A = { X = x1; Y = y1 }; B = { X = x2; Y = y2 } }
            else
                failwith $"Invalid format: {line}"

        File.ReadAllLines(path) |> Seq.map parseLine

    let getCoveredPoints (vector: VectorType) = seq {
        let adx = Math.Abs vector.DX
        let ady = Math.Abs vector.DY
        let sdx = Math.Sign vector.DX
        let sdy = Math.Sign vector.DY
        for i = 0 to max adx ady do
            yield { X = vector.A.X + i * sdx; Y = vector.A.Y + i * sdy }
    }

let solve vectors =

    let addCoveredPoint (map: Map<Vector.PointType, int>) (point: Vector.PointType) =
        let mutable value = 0
        map.TryGetValue(point, &value) |> ignore
        map.Add(point, value + 1)

    vectors
    |> Seq.map Vector.getCoveredPoints
    |> Seq.concat
    |> Seq.fold addCoveredPoint Map.empty
    |> Seq.filter (fun kvp -> kvp.Value > 1)
    |> Seq.length

let vectors = Vector.parseFile "input.txt"

let result1 =
    vectors
    |> Seq.filter (fun vector -> not vector.IsDiagonal)
    |> solve
   
let result2 =
    vectors
    |> solve

printfn "%i" result1 // 5169
printfn "%i" result2 // 22083