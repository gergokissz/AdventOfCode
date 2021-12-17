open System
open System.IO
open System.Text.RegularExpressions

module Day17 = 

    type Point = { X: int; Y: int }
    
    type Rect = { TopLeft: Point; BottomRight: Point }

    let private contains (point: Point) (rect: Rect) =
        point.X >= rect.TopLeft.X &&
        point.X <= rect.BottomRight.X &&
        point.Y <= rect.TopLeft.Y &&
        point.Y >= rect.BottomRight.Y

    let private isBelow (rect: Rect) (point: Point) =
        point.Y < rect.BottomRight.Y

    let readTargetAreaFromFile path = 
        let text = File.ReadAllText(path)
        let regex = Regex "^target area: x=(?<xmin>\-?\d+)\.\.(?<xmax>\-?\d+), y=(?<ymin>\-?\d+)\.\.(?<ymax>\-?\d+)$"
        let m = regex.Match(text)
        if m.Success then
            { TopLeft = { X = int m.Groups["xmin"].Value; 
                          Y = int m.Groups["ymax"].Value; }; 
              BottomRight = { X = int m.Groups["xmax"].Value;
                             Y = int m.Groups["ymin"].Value; } }
        else
            failwith "Invalid input"

    let maxTrajectoryHeight (velocity: Point) (target: Rect) =

        let step (position: Point) (velocity: Point) =
            ({ X = position.X + velocity.X;
               Y = position.Y + velocity.Y },
             { X = velocity.X - sign velocity.X;
               Y = velocity.Y - 1 })

        let rec maxTrajectoryHeightRec (position: Point) (velocity: Point) (target: Rect) (maxHeight: Option<int>) =
            if target |> contains position then // Hit
                if maxHeight.IsSome then maxHeight else Some position.Y
            else if position |> isBelow target then // Miss
                None
            else // Continue
                let p, v = step position velocity
                let h = if maxHeight.IsNone || maxHeight.Value < p.Y then Some p.Y else maxHeight
                maxTrajectoryHeightRec p v target h

        maxTrajectoryHeightRec { X = 0; Y = 0 } velocity target None

let target = Day17.readTargetAreaFromFile "input.txt"

let maxTrajectoryHeights = [
    // Just use some guessed velocity range
    for x = -300 to 300 do
        for y = -300 to 300 do
            match Day17.maxTrajectoryHeight { X = x; Y = y } target with
            | Some value -> yield value
            | None -> ()
]

let result1 = maxTrajectoryHeights |> List.max
let result2 = maxTrajectoryHeights |> List.length

printfn "%i" result1 // 6555
printfn "%i" result2 // 4973