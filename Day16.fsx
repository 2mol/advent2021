#load "Extensions.fsx"
open Extensions

#nowarn "25"

open System


// live reload with:
// -----------------
// echo Day16.fsx | entr -n dotnet fsi /_

printfn "---------- \u001b[31mlet's go!\u001b[0m ----------"

let input =
    System.IO.File.ReadLines "inputs/input16.txt"
    |> Seq.head
    |> Seq.toArray

let toBin c =
    match c with
    | '0' -> "0000"
    | '1' -> "0001"
    | '2' -> "0010"
    | '3' -> "0011"
    | '4' -> "0100"
    | '5' -> "0101"
    | '6' -> "0110"
    | '7' -> "0111"
    | '8' -> "1000"
    | '9' -> "1001"
    | 'A' -> "1010"
    | 'B' -> "1011"
    | 'C' -> "1100"
    | 'D' -> "1101"
    | 'E' -> "1110"
    | 'F' -> "1111"


type Packet =
    {
        Version : int
        TypeId : int
        Content : Content
    }
and Content =
    | Literal of uint64
    | Operator of Packet list

let bitsToInt str = Convert.ToInt32(str, 2)
let bitsToUInt64 str = Convert.ToUInt64(str, 2)

let rec extractLiteral ((str, acc) : string * (string list)) : string * string =
    if str[0] = '0' then
        let result =
            List.append acc [str[1..4]]
            |> String.concat ""
        let rest = str[5..]
        result, rest
    else
        extractLiteral (str[5..], List.append acc [str[1..4]])

let rec parse (limit : int) (str : string) : (Packet list) * string =
    if String.length str < 6 || Set.ofSeq str = set ['0'] then
        [], str
    else
        let version = str[0..2] |> bitsToInt
        let typeId = str[3..5] |> bitsToInt

        match typeId with
        | 4 ->
            // Literal packet
            let litBits, litRest = extractLiteral (str[6..], [])
            // printfn "wanna parse %s to int" literalBits
            let packet =
                {
                    Version = version
                    TypeId = typeId
                    Content = Literal (bitsToUInt64 litBits)
                }
            if limit = 1 then
                [packet], litRest
            else if limit > 1 then
                let packets, rest = parse (limit - 1) litRest
                packet :: packets, rest
            else
                let packets, rest = parse 0 litRest
                packet :: packets, rest
        | _ ->
            // Operator packet
            let lengthTypeId = str[6]
            let subPackets, subRest =
                match lengthTypeId with
                | '0' ->
                    let subPacketsBitLength =
                        str[7..]
                        |> Seq.take 15 |> String.Concat
                        |> bitsToInt
                    let subStr =
                        str[7+15..]
                        |> Seq.take subPacketsBitLength |> String.Concat
                    let ps, "" = parse 0 subStr
                    ps, str[7+15+subPacketsBitLength..]
                | '1' ->
                    let subPacketsCount =
                        str[7..]
                        |> Seq.take 11 |> String.Concat
                        |> bitsToInt

                    parse subPacketsCount str[7+11..]

            let packet =
                {
                    Version = version
                    TypeId = typeId
                    Content = Operator subPackets
                }
            if limit = 1 then
                [packet], subRest
            else if limit > 1 then
                let packets, rest = parse (limit - 1) subRest
                packet :: packets, rest
            else
                let packets, rest = parse 0 subRest
                packet :: packets, rest

let rec sumVersionNumbers (packet : Packet) =
    match packet.Content with
    | Literal _ -> packet.Version
    | Operator subPackets ->
        packet.Version + (List.sumBy sumVersionNumbers subPackets)

let rec value (packet : Packet) : uint64 =
    match packet.Content with
    | Literal v ->
        // printfn "got value %i" v
        v
    | Operator subPackets ->
        match packet.TypeId with
        | 0 -> List.sumBy value subPackets
        | 1 -> List.fold (fun acc p -> acc * (value p)) 1UL subPackets
        | 2 -> List.map value subPackets |> List.min
        | 3 -> List.map value subPackets |> List.max
        | 5 -> if value subPackets[0] > value subPackets[1] then 1UL else 0UL
        | 6 -> if value subPackets[0] < value subPackets[1] then 1UL else 0UL
        | 7 -> if value subPackets[0] = value subPackets[1] then 1UL else 0UL

let inputBits =
    input
    |> Array.map toBin
    |> String.concat ""

let children (packet : Packet) : Packet list =
    match packet.Content with
    | Literal _ -> []
    | Operator sub -> sub

let pretty (packet : Packet) : string =
    match packet.Content with
    | Literal v ->
        sprintf "∟ %i"
            v
    | Operator sub ->
        sprintf "op (val %i) type %i - children: %i"
            (value packet) packet.TypeId (List.length sub) //(List.map value sub)

let rec pretties' maxDepth depth packets =
    if depth > maxDepth then
        printfn "[...]"
    else
        for packet in packets do
            let indent = Array.create (depth) " "
            String.concat "" indent + pretty packet
            |> printfn "%s"
            match packet.Content with
            | Literal _ -> ()
            | Operator ps ->
                pretties' maxDepth (depth+1) ps

let pretties maxDepth p = pretties' maxDepth 0 [p]

let packets =
    parse 0 inputBits
    |> fst
    // |> printfn "day16-2: %A"

packets
|> List.sumBy sumVersionNumbers
|> printfn "day16-1: %i"

packets
|> List.head
|> value
|> printfn "day16-2: %i"
