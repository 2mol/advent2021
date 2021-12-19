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
    | Literal of int
    | Operator of Packet list

let bitsToInt str = Convert.ToInt32(str, 2)

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
            let literalBits, literalRest = extractLiteral (str[6..], [])
            let packet =
                {
                    Version = version
                    TypeId = typeId
                    Content = Literal (bitsToInt literalBits)
                }
            if limit = 1 then
                [packet], literalRest
            else if limit > 1 then
                let packets, rest = parse (limit - 1) literalRest
                packet :: packets, rest
            else
                let packets, rest = parse 0 literalRest
                packet :: packets, rest
        | _ ->
            // Operator packet
            let lengthTypeId = str[6]
            let subPacketFrom =
                if lengthTypeId = '0' then 15 else 11
                |> (+) 7
            let subPacketsInfo =
                str[7..subPacketFrom-1]
                |> bitsToInt
            let subPackets, subRest =
                if lengthTypeId = '0' then
                    let subPacketsTo = subPacketFrom+subPacketsInfo
                    let strSubPackets = str[subPacketFrom..subPacketsTo-1]
                    let rest0 = str[subPacketsTo..]
                    let packets1, rest1 = parse 0 strSubPackets // rest1 SHOULD be empty...
                    packets1, rest0
                else
                    parse subPacketsInfo str[subPacketFrom..]

            let packet =
                {
                    Version = version
                    TypeId = typeId
                    Content = Operator subPackets
                }
            let packets, rest = parse 0 subRest
            packet :: packets, rest

let rec sumVersionNumbers (packet : Packet) =
    match packet.Content with
    | Literal _ -> packet.Version
    | Operator subPackets ->
        packet.Version + (List.sumBy sumVersionNumbers subPackets)


let inputBits =
    input
    |> Array.map toBin
    |> String.concat ""

parse 0 inputBits
|> fst
|> List.sumBy sumVersionNumbers
|> printfn "%A"
