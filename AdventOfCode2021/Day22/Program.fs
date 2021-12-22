open System
open System.Text.RegularExpressions
open System.IO

module Day22 =

    type Range = { Min: int64; Max: int64} with
        member this.length = this.Max - this.Min + 1L

    type Cuboid = { X: Range; Y: Range; Z: Range } with
        member this.volume = this.X.length * this.Y.length * this.Z.length

    type Step =
    | On of Cuboid
    | Off of Cuboid

    let createCuboid xmin xmax ymin ymax zmin zmax =
        { X = { Min = xmin; Max = xmax };
          Y = { Min = ymin; Max = ymax };
          Z = { Min = zmin; Max = zmax } }

    let initializationArea = 
        createCuboid -50L 50L -50L 50L -50L 50L

    let rebootArea = 
        createCuboid Int64.MinValue Int64.MaxValue Int64.MinValue Int64.MaxValue Int64.MinValue Int64.MaxValue

    let readStepsFromFile path = 
        let regex = new Regex("^(?<operation>(on)|(off))\sx=(?<xmin>-?\d+)\.\.(?<xmax>-?\d+),y=(?<ymin>-?\d+)\.\.(?<ymax>-?\d+),z=(?<zmin>-?\d+)\.\.(?<zmax>-?\d+)$")
        File.ReadAllLines(path)
        |> Seq.map (fun line ->
            let m = regex.Match(line)
            match m.Success with
            | true -> 
                let cuboid = 
                    createCuboid 
                        (int m.Groups["xmin"].Value) (int m.Groups["xmax"].Value)
                        (int m.Groups["ymin"].Value) (int m.Groups["ymax"].Value)
                        (int m.Groups["zmin"].Value) (int m.Groups["zmax"].Value)
                match m.Groups["operation"].Value with
                | "on" -> On cuboid
                | "off" -> Off cuboid
                | _ -> failwith "Invalid operation"
            | false -> failwith "Invalid input")

    let private overlaps (c1: Cuboid) (c2: Cuboid) =
        c1.X.Max >= c2.X.Min && c1.X.Min <= c2.X.Max &&
        c1.Y.Max >= c2.Y.Min && c1.Y.Min <= c2.Y.Max &&
        c1.Z.Max >= c2.Z.Min && c1.Z.Min <= c2.Z.Max

    let private intersectr (r1: Range) (r2: Range) =
        { Min = Math.Max(r1.Min, r2.Min); Max = Math.Min(r1.Max, r2.Max) }

    let private intersectc (c1: Cuboid) (c2: Cuboid) =
        if c1 |> overlaps c2 then
            let x = c1.X |> intersectr c2.X
            let y = c1.Y |> intersectr c2.Y
            let z = c1.Z |> intersectr c2.Z
            Some { X = x; Y = y; Z = z }
        else
            None

    // Subtracting the middle of a cuboid:
    //
    //   TOP:            BOTTOM:              FRONT:          BACK:                RIGHT:          LEFT:       
    //   -------------   -------------        -------------   -------------        -------------   -------------
    //   |   | 4 |   |   |   | 4 |   |        |   |   |   |   |   |   |   |        |           |   |           |
    //   |   |---|   |   |   |---|   |        |   |   |   |   |   |   |   |        |           |   |           |
    //   | 1 | 6 | 2 |   | 2 | 5 | 1 |        | 1 | 3 | 2 |   | 2 | 4 | 1 |        |     2     |   |     1     |
    //   |   |---|   |   |   |---|   |        |   |   |   |   |   |   |   |        |           |   |           |
    // Y |   | 3 |   |   |   | 3 |   | Y    Z |   |   |   |   |   |   |   | Z    Z |           |   |           | Z
    // | -------------   ------------- |    | -------------   ------------- |    | -------------   ------------- |
    // 0---X                       X---0    0---X                       X---0    0---Y                       Y---0
    
    let subtractAndSplit (c1: Cuboid) (c2: Cuboid) = [
        if c1 |> overlaps c2 then
            let x = c1.X |> intersectr c2.X
            let y = c1.Y |> intersectr c2.Y
            if c1.X.Min < c2.X.Min then yield { c1 with X = { Min = c1.X.Min; Max = c2.X.Min - 1L } } // 1
            if c1.X.Max > c2.X.Max then yield { c1 with X = { Min = c2.X.Max + 1L; Max = c1.X.Max } } // 2
            if c1.Y.Min < c2.Y.Min then yield { c1 with X = x; Y = { Min = c1.Y.Min; Max = c2.Y.Min - 1L } } // 3
            if c1.Y.Max > c2.Y.Max then yield { c1 with X = x; Y = { Min = c2.Y.Max + 1L; Max = c1.Y.Max } } // 4
            if c1.Z.Min < c2.Z.Min then yield { X = x; Y = y; Z = { Min = c1.Z.Min; Max = c2.Z.Min - 1L } } // 5
            if c1.Z.Max > c2.Z.Max then yield { X = x; Y = y; Z = { Min = c2.Z.Max + 1L; Max = c1.Z.Max } } // 6
        else
            yield c1
    ]

    let execute (area: Cuboid) (steps: seq<Step>) = 

        let turn (on: bool) (cuboid: Option<Cuboid>) (cuboidsTurnedOn: Cuboid list) = [
            match cuboid with
            | Some cuboid ->
                // Subtract/split the current cuboid from turned on cuboids to avoid overlaps
                for cuboidTurnedOn in cuboidsTurnedOn do 
                    yield! cuboid |> subtractAndSplit cuboidTurnedOn
                // Add the current cuboid only if this is an "on" step
                if on then yield cuboid
            | None -> yield! cuboidsTurnedOn                
        ]

        steps
        |> Seq.fold (fun cuboidsTurnedOn step ->
            match step with
            | On cuboid -> cuboidsTurnedOn |> turn true (cuboid |> intersectc area)
            | Off cuboid -> cuboidsTurnedOn |> turn false (cuboid |> intersectc area)) []
        |> Seq.map (fun cuboidTurnedOn -> cuboidTurnedOn.volume)
        |> Seq.sum

let input = Day22.readStepsFromFile "input.txt" |> Seq.toList

let result1 = input |> Day22.execute Day22.initializationArea

printfn "%i" result1 // 648023

let result2 = input |> Day22.execute Day22.rebootArea

printfn "%i" result2 // 1285677377848549