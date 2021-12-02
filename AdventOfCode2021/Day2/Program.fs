open System.IO

exception CommandError of string

let input =
    File.ReadAllLines(@"input.txt")
    |> Seq.map (fun line -> line.Split(' '))
    |> Seq.map (fun words -> (words[0], words[1] |> int))

type Position = { Horizontal: int; Depth: int; }

let aggregatePosition (position: Position) (command: string, x: int) =
    match command with
    | "forward" -> { position with Horizontal = position.Horizontal + x }
    | "up" -> { position with Depth = position.Depth - x }
    | "down" -> { position with Depth = position.Depth + x }
    | _ -> raise (CommandError("Invalid command: " + command))

let position = input |> Seq.fold aggregatePosition { Horizontal = 0; Depth = 0 }

printfn "%i" (position.Horizontal * position.Depth)

type PositionWithAim = { Horizontal: int; Depth: int; Aim: int }

let aggregatePositionWithAim (position: PositionWithAim) (command: string, x: int) =
    match command with
    | "forward" -> { position with Horizontal = position.Horizontal + x; Depth = position.Depth + x * position.Aim }
    | "up" -> { position with Aim = position.Aim - x }
    | "down" -> { position with Aim = position.Aim + x}
    | _ -> raise (CommandError("Invalid command: " + command))

let positionWithAim = input |> Seq.fold aggregatePositionWithAim { Horizontal = 0; Depth = 0; Aim = 0 }

printfn "%i" (positionWithAim.Horizontal * positionWithAim.Depth)
