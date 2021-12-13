open System
open System.IO
open System.Text.RegularExpressions

module Day13 =

    type Dot = { X: int; Y: int }

    type Fold = 
    | FoldAlongX of int
    | FoldAlongY of int

    type Manual = { Dots: Dot list; Folds: Fold list }

    let readManualFromFile path =

        let dotRegex = Regex("^(?<x>\d+),(?<y>\d+)$", RegexOptions.Compiled)
        let foldRegex = Regex("^fold along (?<axis>[xy])=(?<position>\d+)$", RegexOptions.Compiled)

        let tryParseDot line =
            match dotRegex.Match line with
            | m when m.Success -> Some { X = int m.Groups["x"].Value; Y = int m.Groups["y"].Value }
            | _ -> None

        let tryParseFold line =
            match foldRegex.Match line with
            | m when m.Success -> 
                let position = int m.Groups["position"].Value 
                match m.Groups["axis"].Value with
                | "x" -> Some (FoldAlongX(position))
                | "y" -> Some (FoldAlongY(position))
                | x -> failwith $"Invalid fold axis: {x}"
            | _ -> None

        File.ReadAllLines(path)
        |> Seq.fold (fun manual line -> 
            match tryParseDot line with
            | Some dot -> { manual with Dots =  dot :: manual.Dots }
            | None -> 
                match tryParseFold line with
                | Some fold -> { manual with Folds =  fold :: manual.Folds }
                | None -> manual
        ) { Dots = []; Folds = [] }
        |> fun manual -> { manual with Folds = manual.Folds |> List.rev }

    let foldOne (manual: Manual) =
        match manual.Folds with
        | currentFold :: remainingFolds -> 
            let dots = 
                manual.Dots
                |> List.map (fun dot -> 
                    match currentFold with
                    | FoldAlongX x when dot.X > x -> { dot with X = 2 * x - dot.X }
                    | FoldAlongY y when dot.Y > y -> { dot with Y = 2 * y - dot.Y }
                    | _ -> dot)
                |> List.distinct
            { Dots = dots; Folds = remainingFolds }
        | [] -> manual

    let rec foldAll (manual: Manual) =
        match manual.Folds with
        | _ :: _ -> manual |> foldOne |> foldAll
        | [] -> manual

    let print (dots: Dot list) =
        Console.Clear()
        for dot in dots do
            Console.SetCursorPosition(dot.X, dot.Y)
            Console.Write("#")
        Console.SetCursorPosition(0, 7)

let result1 = 
    Day13.readManualFromFile "input.txt"
    |> Day13.foldOne
    |> fun manual -> manual.Dots |> List.length

Day13.readManualFromFile "input.txt"
    |> Day13.foldAll
    |> fun manual -> manual.Dots |> Day13.print

// #  # ####   ## #  #   ## ###   ##    ##
// #  # #       # #  #    # #  # #  #    #
// #### ###     # ####    # #  # #       #
// #  # #       # #  #    # ###  #       #
// #  # #    #  # #  # #  # # #  #  # #  #
// #  # ####  ##  #  #  ##  #  #  ##   ##

printfn "%i" result1 // 647
