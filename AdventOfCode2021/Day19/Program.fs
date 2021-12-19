open System
open System.IO
open System.Collections.Generic

module Day19 = 

    type Vector = { X: int; Y: int; Z: int }

    type Rotation = { Name: string; Apply: Vector -> Vector }

    type Transformation = { Rotation: Rotation; Translation: Vector }

    type ScannerReading = Vector list

    // Note: names are not required, I only added them for debugging purposes
    let private rotations =
        [
            { Name = "+X+Y+Z"; Apply = fun (v: Vector) -> { X = v.X; Y = v.Y; Z = v.Z } }
            { Name = "+X+Z-Y"; Apply = fun (v: Vector) -> { X = v.X; Y = v.Z; Z = -v.Y } }
            { Name = "+X-Y-Z"; Apply = fun (v: Vector) -> { X = v.X; Y = -v.Y; Z = -v.Z } }
            { Name = "+X-Z+Y"; Apply = fun (v: Vector) -> { X = v.X; Y = -v.Z; Z = v.Y } }

            { Name = "-X+Y-Z"; Apply = fun (v: Vector) -> { X = -v.X; Y = v.Y; Z = -v.Z } }
            { Name = "-X+Z+Y"; Apply = fun (v: Vector) -> { X = -v.X; Y = v.Z; Z = v.Y } }
            { Name = "-X-Y+Z"; Apply = fun (v: Vector) -> { X = -v.X; Y = -v.Y; Z = v.Z } }
            { Name = "-X-Z-Y"; Apply = fun (v: Vector) -> { X = -v.X; Y = -v.Z; Z = -v.Y } }

            { Name = "+Y+X-Z"; Apply = fun (v: Vector) -> { X = v.Y; Y = v.X; Z = -v.Z } }
            { Name = "+Y+Z+X"; Apply = fun (v: Vector) -> { X = v.Y; Y = v.Z; Z = v.X } }
            { Name = "+Y-X+Z"; Apply = fun (v: Vector) -> { X = v.Y; Y = -v.X; Z = v.Z } }
            { Name = "+Y-Z-X"; Apply = fun (v: Vector) -> { X = v.Y; Y = -v.Z; Z = -v.X } }

            { Name = "-Y+X+Z"; Apply = fun (v: Vector) -> { X = -v.Y; Y = v.X; Z = v.Z } }
            { Name = "-Y+Z-X"; Apply = fun (v: Vector) -> { X = -v.Y; Y = v.Z; Z = -v.X } }
            { Name = "-Y-X-Z"; Apply = fun (v: Vector) -> { X = -v.Y; Y = -v.X; Z = -v.Z } }
            { Name = "-Y-Z+X"; Apply = fun (v: Vector) -> { X = -v.Y; Y = -v.Z; Z = v.X } }

            { Name = "+Z+X+Y"; Apply = fun (v: Vector) -> { X = v.Z; Y = v.X; Z = v.Y } }
            { Name = "+Z+Y-X"; Apply = fun (v: Vector) -> { X = v.Z; Y = v.Y; Z = -v.X } }
            { Name = "+Z-X-Y"; Apply = fun (v: Vector) -> { X = v.Z; Y = -v.X; Z = -v.Y } }
            { Name = "+Z-Y+X"; Apply = fun (v: Vector) -> { X = v.Z; Y = -v.Y; Z = v.X } }

            { Name = "-Z+X-Y"; Apply = fun (v: Vector) -> { X = -v.Z; Y = v.X; Z = -v.Y } }
            { Name = "-Z+Y+X"; Apply = fun (v: Vector) -> { X = -v.Z; Y = v.Y; Z = v.X } }
            { Name = "-Z-X+Y"; Apply = fun (v: Vector) -> { X = -v.Z; Y = -v.X; Z = v.Y } }
            { Name = "-Z-Y-X"; Apply = fun (v: Vector) -> { X = -v.Z; Y = -v.Y; Z = -v.X } }
        ]

    let private add (v1: Vector) (v2: Vector) =
        { X = v1.X + v2.X; Y = v1.Y + v2.Y; Z = v1.Z + v2.Z }

    let private sub (v1: Vector) (v2: Vector) =
        { X = v1.X - v2.X; Y = v1.Y - v2.Y; Z = v1.Z - v2.Z }

    let private transform (t: Transformation) (v: Vector) =
         add (v |> t.Rotation.Apply) t.Translation

    let readInput path =

        let splitBy (predicate: 'a -> bool) (sequence: seq<'a>): seq<seq<'a>> =
            seq {
                use enumerator = sequence.GetEnumerator()
                let hasMoreItems = ref true
                while hasMoreItems.Value do
                    let isNotSeparator = ref true
                    let block = [
                        while hasMoreItems.Value && isNotSeparator.Value do
                            hasMoreItems.Value <- enumerator.MoveNext()
                            if hasMoreItems.Value then
                                isNotSeparator.Value <- not (predicate enumerator.Current)
                                if isNotSeparator.Value then
                                    yield enumerator.Current
                    ]
                    if not (block |> List.isEmpty) 
                        then yield block }

        File.ReadAllLines(path) 
            |> splitBy (fun line -> line = "")
            |> Seq.map (fun lines -> 
                lines 
                    |> Seq.skip(1)
                    |> Seq.map (fun line ->
                        let parts = line.Split(',')
                        if parts.Length = 3 then
                            { X = int parts[0]; Y = int parts[1]; Z = int parts[2] }
                        else
                            failwith "Invalid coordinates")
                    |> Seq.toList)
            |> Seq.toList

    let private areOverlapping (scannerReading1: ScannerReading) (scannerReading2: ScannerReading) (transformation: Transformation) =
        let mutable matchCount = 0
        for v1 in scannerReading1 do
            for v2 in scannerReading2 do
                if matchCount < 12 then
                    if v1 = (v2 |> transform transformation) then
                        matchCount <- matchCount + 1
        matchCount > 11

    let private tryFindTransformation (scannerReading1: ScannerReading) (scannerReading2: ScannerReading) =
        let mutable transformation = None
        for rotation in rotations do
            for v1 in scannerReading1 do
                for v2 in scannerReading2 do
                    if transformation.IsNone then
                        let translation = sub v1 (v2 |> rotation.Apply)
                        let t = { Rotation = rotation; Translation = translation }
                        if areOverlapping scannerReading1 scannerReading2 t then
                            transformation <- Some t
        transformation

    let findTransformations (scannerReadings: ScannerReading list) =

        let transformations = Array.init scannerReadings.Length (fun i ->
            if i = 0 then
                [ { Rotation = rotations[0]; Translation = { X = 0; Y = 0; Z = 0; } } ]
            else
                List.empty<Transformation>)

        let rec fillTransformations (i: int) (t: Transformation list) =
            scannerReadings
            |> Seq.iteri (fun j scannerReadingJ ->
                if i <> j && transformations[j].Length = 0 then
                    match tryFindTransformation scannerReadings[i] scannerReadingJ with
                    | Some transformation -> 
                        transformations[j] <- transformation :: t
                        fillTransformations j transformations[j]
                    | None -> ())

        fillTransformations 0 transformations[0]

        transformations

    let rec applyTransformations (transformations: Transformation list) (vector: Vector) =
        match transformations with
        | transformation :: rest -> applyTransformations rest (vector |> transform transformation)
        | [] -> vector

let scannerReadings = Day19.readInput "input.txt"

let transformations = scannerReadings |> Day19.findTransformations

// Part 1

let beaconPositions = new HashSet<Day19.Vector>()

transformations
|> Seq.iteri (fun i t ->
    scannerReadings[i]
    |> Seq.iter (fun vector ->
        beaconPositions.Add(vector |> Day19.applyTransformations t) |> ignore))

printfn "%i" (beaconPositions.Count) // 318

// Part 2

let scannerPositions = 
    transformations
    |> Seq.map (fun transformation -> 
        { Day19.X = 0; Day19.Y = 0; Day19.Z = 0; } |> Day19.applyTransformations transformation)
    |> Seq.toList

let result2 =
    Seq.allPairs scannerPositions scannerPositions
    |> Seq.map (fun pair ->
        let dx = abs ((fst pair).X - (snd pair).X)
        let dy = abs ((fst pair).Y - (snd pair).Y)
        let dz = abs ((fst pair).Z - (snd pair).Z)
        dx + dy + dz)
    |> Seq.max

printfn "%i" result2 // 12166