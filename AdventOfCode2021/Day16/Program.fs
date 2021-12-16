open System
open System.Collections.Generic
open System.IO

module Day16 = 

    type Packet =
    | Literal of uint64 * uint64 // Version + Value
    | Sum of uint64 * Packet list // Version + SubPackets
    | Product of uint64 * Packet list
    | Minimum of uint64 * Packet list
    | Maximum of uint64 * Packet list
    | GreaterThan of uint64 * Packet list
    | LowerThan of uint64 * Packet list
    | EqualTo of uint64 * Packet list

    // TODO: refactor this
    let readBitsFromFile path =
        let bits = new Queue<char>()
        for hex in (File.ReadAllText path).ToCharArray() do
            for bit in match hex with
                       | '0' -> [ '0'; '0'; '0'; '0' ]
                       | '1' -> [ '0'; '0'; '0'; '1' ]
                       | '2' -> [ '0'; '0'; '1'; '0' ]
                       | '3' -> [ '0'; '0'; '1'; '1' ]
                       | '4' -> [ '0'; '1'; '0'; '0' ]
                       | '5' -> [ '0'; '1'; '0'; '1' ]
                       | '6' -> [ '0'; '1'; '1'; '0' ]
                       | '7' -> [ '0'; '1'; '1'; '1' ]
                       | '8' -> [ '1'; '0'; '0'; '0' ]
                       | '9' -> [ '1'; '0'; '0'; '1' ]
                       | 'A' -> [ '1'; '0'; '1'; '0' ]
                       | 'B' -> [ '1'; '0'; '1'; '1' ]
                       | 'C' -> [ '1'; '1'; '0'; '0' ]
                       | 'D' -> [ '1'; '1'; '0'; '1' ]
                       | 'E' -> [ '1'; '1'; '1'; '0' ]
                       | 'F' -> [ '1'; '1'; '1'; '1' ]
                       | _ -> failwith $"Invalid hex character: '{hex}'"
                do bits.Enqueue(bit)
        bits
    
    let parsePackets (bits: Queue<char>) =

        let toUInt64 (bits: char[]) =
            Convert.ToUInt64(new String(bits), 2)

        let readBits count =
            [| for _ = 1 to count do yield bits.Dequeue() |]
                
        let readUInt64 count = 
            readBits count |> toUInt64
        
        let rec readPacket() =
            let version = readUInt64 3
            let typeId = readUInt64 3
            let ret = 
                match typeId with
                | 0UL -> Sum(version, readSubPackets())
                | 1UL -> Product(version, readSubPackets())
                | 2UL -> Minimum(version, readSubPackets())
                | 3UL -> Maximum(version, readSubPackets())
                | 4UL -> Literal(version, readLiteralValue())
                | 5UL -> GreaterThan(version, readSubPackets())
                | 6UL -> LowerThan(version, readSubPackets())
                | 7UL -> EqualTo(version, readSubPackets())
                | _ -> failwith $"Invalid typeId: '{typeId}'"
            ret

        and readLiteralValue() =
            let bits = [|                
                let mutable lastBlock = false
                while not lastBlock do 
                    lastBlock <- readUInt64 1 = 0UL
                    yield! readBits 4
            |]
            bits |> toUInt64

        and readSubPackets() =
            let lengthTypeId = readUInt64 1
            if lengthTypeId = 0UL then
                readSubPacketsByLength()
            else
                readSubPacketsByCount()

        and readSubPacketsByLength() = [
            let subPacketLength = readUInt64 15
            let targetCount = bits.Count - int subPacketLength
            while bits.Count <> targetCount do
                if bits.Count < targetCount then failwith "Failed to read sub-packets by length"
                yield readPacket()
        ]

        and readSubPacketsByCount() = [ 
            let subPacketCount = readUInt64 11            
            for _ = 0 to int subPacketCount - 1 do
                yield readPacket()
        ]

        let rec readToEnd (packets: Packet list) =
            if bits.Count > 7 then
                readToEnd (readPacket() :: packets)
            else
                packets
        
        readToEnd []

    let rec sumPacketVersions (packets: Packet list) =
        packets 
        |> List.sumBy (fun packet ->
            match packet with
            | Literal (version, _) -> version
            | Sum (version, packets) -> version + sumPacketVersions packets
            | Product (version, packets) -> version + sumPacketVersions packets
            | Minimum (version, packets) -> version + sumPacketVersions packets
            | Maximum (version, packets) -> version + sumPacketVersions packets
            | GreaterThan (version, packets) -> version + sumPacketVersions packets
            | LowerThan (version, packets) -> version + sumPacketVersions packets
            | EqualTo (version, packets) -> version + sumPacketVersions packets)

    let rec evaluatePackets (packets: Packet list) =

        let rec evaluateOne (packet: Packet) =
            match packet with
            | Literal (_, value) -> value
            | Sum (_, packets) -> packets |> List.map evaluateOne |> List.fold (+) 0UL
            | Product (_, packets) -> packets |> List.map evaluateOne |> List.fold (*) 1UL
            | Minimum (_, packets) -> packets |> List.map evaluateOne |> List.min
            | Maximum (_, packets) -> packets |> List.map evaluateOne |> List.max
            | GreaterThan (_, packets) -> if (packets[0] |> evaluateOne) > (packets[1] |> evaluateOne) then 1UL else 0UL
            | LowerThan (_, packets) -> if (packets[0] |> evaluateOne) < (packets[1] |> evaluateOne) then 1UL else 0UL
            | EqualTo (_, packets) -> if (packets[0] |> evaluateOne) = (packets[1] |> evaluateOne) then 1UL else 0UL

        if packets.Length <> 1 then failwith "Invalid packet list"

        packets[0] |> evaluateOne

let result1 = Day16.readBitsFromFile "input.txt" |> Day16.parsePackets |> Day16.sumPacketVersions
let result2 = Day16.readBitsFromFile "input.txt" |> Day16.parsePackets |> Day16.evaluatePackets

printfn "%i" result1 // 920
printfn "%i" result2 // 10185143721112
