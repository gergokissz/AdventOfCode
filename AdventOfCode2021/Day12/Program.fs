open System
open System.IO

module Day12 =

    type Cave = { Name: string; } with
        member this.isBig = Char.IsUpper(this.Name.ToCharArray()[0])
        member this.isStart = this.Name = "start"
        member this.isEnd = this.Name = "end"

    type Connection = { From: Cave; To: Cave }

    let readConnectionsFromFile path =
        File.ReadAllLines(path)
        |> Seq.map (fun line -> 
            let parts = line.Split('-')
            { From = { Name = parts[0] }; To = { Name = parts[1] }})
        |> Seq.toList

    let rec countPathsToEnd (start: Cave) (path: Cave list) (returnToSmallCaveAllowed: bool) (connections: Connection list) : int =

        let connectionsFromStart = connections |> List.where (fun connection -> connection.From = start || connection.To = start)

        connectionsFromStart |> List.sumBy (fun connection -> 
            let next = if start = connection.From then connection.To else connection.From
            if next.isEnd then 1
            else if next.isStart then 0
            else if next.isBig then connections |> countPathsToEnd next (start :: path) returnToSmallCaveAllowed
            else if not (path |> List.contains next) then connections |> countPathsToEnd next (start :: path) returnToSmallCaveAllowed                
            else if returnToSmallCaveAllowed then connections |> countPathsToEnd next (start :: path) false                
            else 0)

let connections = Day12.readConnectionsFromFile "input.txt"

let result1 = connections |> Day12.countPathsToEnd { Name = "start" } [] false
let result2 = connections |> Day12.countPathsToEnd { Name = "start" } [] true

printfn "%i" result1 // 4338
printfn "%i" result2 // 114189