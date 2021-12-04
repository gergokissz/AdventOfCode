open System
open System.IO

module Seq =

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
                    then yield block
        }

module String =

    let splitToInt (separator: string) (text: string) =
        text.Split(separator, StringSplitOptions.RemoveEmptyEntries) |> Seq.map int

module Array2D =

    let row i (array: 'T[,]) =
        array[i..i, *]
        |> Seq.cast<'T>
    
    let col j (array: 'T[,]) =
        array[*, j..j]|> Seq.cast<'T>

    let flatten (array: 'T[,]) = seq {
        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                yield array[x, y]
    }

module Board =

    type FieldType = { Value: int; Marked: bool }

    type BoardType = { Size: int; Fields: FieldType[,]; mutable Result: int option }

    let create lines =
        let size = lines |> Seq.length
        let fields = Array2D.create size size { Value = 0; Marked = false }
        lines |> Seq.iteri (fun i line ->
            line
            |> String.splitToInt " "
            |> Seq.iteri (fun j number -> 
                fields[i, j] <- { fields[i, j] with Value = number } ))
        { Size = size; Fields = fields; Result = None }
   
    let private isMatch i j number board =
        board.Fields[i, j].Value = number

    let private setMarked i j board =
        board.Fields[i, j] <- { board.Fields[i, j] with Marked = true }

    let private isRowFullyMarked i board =
        board.Fields |> Array2D.row i |> Seq.fold (fun state current -> state && current.Marked) true

    let private isColumnFullyMarked j board =
        board.Fields |> Array2D.col j |> Seq.fold (fun state current -> state && current.Marked) true

    let private setResult number board =
        let sumOfUnmarkedFieldValues = board.Fields |> Array2D.flatten |> Seq.sumBy (fun field -> if field.Marked then 0 else field.Value)
        board.Result <- Some(sumOfUnmarkedFieldValues * number)

    let rec private tryMarkNextField number index board =
        if index < board.Size * board.Size then
            let i = index % board.Size
            let j = index / board.Size
            if board |> isMatch i j number then
                board |> setMarked i j
                if (board |> isRowFullyMarked i) || (board |> isColumnFullyMarked j) then
                    board |> setResult number
                else 
                    board |> tryMarkNextField number (index + 1)
            else
                board |> tryMarkNextField number (index + 1)

    let tryMarkNumber number board =
        board |> tryMarkNextField number 0

module Game =

    type GameType = { Numbers: int[]; Boards: Board.BoardType[] }

    let create lines =
        let blocks = lines |> Seq.splitBy (fun line -> line = "")
        let numbers = (blocks |> Seq.head |> Seq.head) |> String.splitToInt "," |> Seq.toArray        
        let boards = 
            blocks 
            |> Seq.skip 1 
            |> Seq.map (fun lines -> Board.create lines)
            |> Seq.toArray
        { Numbers = numbers; Boards = boards }

    let play game = seq {
        for number in game.Numbers do
            for board in game.Boards |> Seq.filter (fun board -> board.Result.IsNone) do
                board |> Board.tryMarkNumber number
                if board.Result.IsSome then
                    yield board.Result.Value
    }    

let lines = File.ReadLines("input.txt")

let results = Game.create lines |> Game.play |> Seq.toArray

printfn "Part1: %A" (results |> Array.head)
printfn "Part2: %A" (results |> Array.last)
