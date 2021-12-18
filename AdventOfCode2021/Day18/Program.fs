open System
open System.IO
open System.Collections.Generic

module Day17 = 

    type Node = { Value: int; Depth: int; }

    type SnailfishNumber = List<Node>

    let readSnailfishNumbersFromFile path =

        let parseSnailfishNumber (line: string) =
            let rec readNextNode (queue: Queue<char>) (snum: SnailfishNumber) (depth: int) = 
                if queue.Count > 0 then
                    match queue.Dequeue() with
                    | '[' -> readNextNode queue snum (depth + 1)
                    | ']' -> readNextNode queue snum (depth - 1)
                    | ',' -> readNextNode queue snum depth
                    | c -> 
                        snum.Add({ Depth = depth; Value = int (string c) })
                        readNextNode queue snum depth
                else
                    snum
            readNextNode (new Queue<char>(line.ToCharArray())) (new SnailfishNumber()) 0
        
        File.ReadAllLines(path) |> Seq.map (fun line -> line |> parseSnailfishNumber) |> Seq.toList

    let rec private reduce (snum: SnailfishNumber) =

        let rec explode i (snum: SnailfishNumber) =
            if i < snum.Count then
                if snum[i].Depth > 4 then
                    let depth = snum[i].Depth
                    let leftValue = snum[i].Value
                    let rightValue = snum[i + 1].Value
                    if i > 0 then snum[i - 1] <- { snum[i - 1] with Value = snum[i - 1].Value + leftValue }
                    if i < snum.Count - 2 then snum[i + 2] <- { snum[i + 2] with Value = snum[i + 2].Value + rightValue }
                    snum.RemoveAt(i + 1)
                    snum.RemoveAt(i)
                    snum.Insert(i, { Depth = depth - 1; Value = 0 })
                    true
                else
                    snum |> explode (i + 1)
            else
                false

        let rec split i (snum: SnailfishNumber) =
            if i < snum.Count then
                if snum[i].Value > 9 then
                    let depth = snum[i].Depth + 1
                    let leftValue = snum[i].Value / 2
                    let rightValue = (snum[i].Value + 1) / 2
                    snum.RemoveAt(i)
                    snum.Insert(i, { Depth = depth; Value = rightValue})
                    snum.Insert(i, { Depth = depth; Value = leftValue})
                    true
                else
                    snum |> split (i + 1)
            else
                false

        if snum |> explode 0 then snum |> reduce
        if snum |> split 0 then snum |> reduce

    let add (snum1: SnailfishNumber) (snum2: SnailfishNumber) =
        let result = new SnailfishNumber(Seq.append snum1 snum2 |> Seq.map (fun node -> { node with Depth = node.Depth + 1 }))
        result |> reduce
        result

    let rec sum (snums: SnailfishNumber list) =
        match snums with
        | [ snum ] -> snum
        | snum1 :: snum2 :: rest -> 
            let result = add snum1 snum2
            sum (result :: rest)
        | _ -> failwith "This should not happen"

    let magnitude (snum: SnailfishNumber) =
        
        let rec reduceOne i =
            if i < snum.Count - 1 then
                let maxDepth = snum |> Seq.map (fun node -> node.Depth) |> Seq.max            
                let leftNode = snum[i]
                let rightNode = snum[i + 1]
                if (leftNode.Depth = maxDepth) then
                    snum.RemoveAt(i + 1)
                    snum.RemoveAt(i)
                    snum.Insert(i, { Value = 3 * leftNode.Value + 2 * rightNode.Value; Depth = maxDepth - 1 })
                else
                    reduceOne (i + 1)
                    
        let rec reduceAll() = 
            if snum.Count > 1 then
                reduceOne 0
                reduceAll()

        reduceAll()

        snum[0].Value

let snums = Day17.readSnailfishNumbersFromFile "input.txt"

let result1 = 
    snums
    |> Day17.sum 
    |> Day17.magnitude

printfn "%i" result1 // 4033

let result2 =
    [
        for i in 0 .. snums.Length - 1 do
            for j in 0 .. snums.Length - 1 do
                if i <> j then Day17.add snums[i] snums[j]
    ]
    |> Seq.map (fun snum -> snum |> Day17.magnitude)
    |> Seq.max

printfn "%i" result2 // 4864
