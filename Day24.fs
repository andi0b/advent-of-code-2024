module aoc24.Day24

open System
open System.Collections.Generic

type Op =
    | And
    | Xor
    | Or

    static member parse =
        function
        | "AND" -> And
        | "OR" -> Or
        | "XOR" -> Xor
        | _ -> failwith "unknown op"

let parse input =
    let emptyLine = input |> Array.findIndex ((=) "")
    let header, footer = input |> Array.splitAt emptyLine

    let startValues =
        header
        |> Array.map (fun l ->
            let parts = l |> StringEx.splitS ": "
            parts[0], parts[1] |> int)

    let connections =
        footer
        |> Array.skip 1
        |> Array.map (fun l ->
            let parts = l |> StringEx.splitS " "
            (parts[0], parts[1] |> Op.parse, parts[2], parts[4]))

    startValues, connections

let dequeueEnumerable (q: Queue<_>) =
    seq {
        let mutable hasItems, item = q.TryDequeue()

        while hasItems do
            yield item
            let h, i = q.TryDequeue()
            hasItems <- h
            item <- i
    }

let part1 input =
    let values, ops =
        parse input
        |> TupleEx.mapFst (Array.map TupleEx.toKeyValue >> Dictionary)
        |> TupleEx.mapSnd Queue


    for in1, op, in2, out in dequeueEnumerable ops do
        let in1Known, in1Value = values.TryGetValue in1
        let in2Known, in2Value = values.TryGetValue in2

        if in1Known && in2Known then
            let opf =
                match op with
                | And -> (&&&)
                | Or -> (|||)
                | Xor -> (^^^)

            values[out] <- opf in1Value in2Value
        else
            ops.Enqueue(in1, op, in2, out)

    for KeyValue(k, v) in values do
        dprintfn $"{k}: {v}"

    let bits =
        values
        |> Seq.filter (fun (KeyValue(k, _)) -> k.StartsWith("z"))
        |> Seq.sortBy (fun (KeyValue(k, _)) -> k)
        |> Seq.map (fun (KeyValue(_, v)) -> int64 v)

    printfn $"bits length: {bits |> Seq.length}"

    bits |> Seq.mapi (fun i b -> b <<< i) |> Seq.reduce (|||)


let part2 = (fun _ -> 0)

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "x00: 1"
           "x01: 1"
           "x02: 1"
           "y00: 0"
           "y01: 1"
           "y02: 0"
           ""
           "x00 AND y00 -> z00"
           "x01 XOR y01 -> z01"
           "x02 OR y02 -> z02" |]

    let example2 =
        [| "x00: 1"
           "x01: 0"
           "x02: 1"
           "x03: 1"
           "x04: 0"
           "y00: 1"
           "y01: 1"
           "y02: 1"
           "y03: 1"
           "y04: 1"
           ""
           "ntg XOR fgs -> mjb"
           "y02 OR x01 -> tnw"
           "kwq OR kpj -> z05"
           "x00 OR x03 -> fst"
           "tgd XOR rvg -> z01"
           "vdt OR tnw -> bfw"
           "bfw AND frj -> z10"
           "ffh OR nrd -> bqk"
           "y00 AND y03 -> djm"
           "y03 OR y00 -> psh"
           "bqk OR frj -> z08"
           "tnw OR fst -> frj"
           "gnj AND tgd -> z11"
           "bfw XOR mjb -> z00"
           "x03 OR x00 -> vdt"
           "gnj AND wpb -> z02"
           "x04 AND y00 -> kjc"
           "djm OR pbm -> qhw"
           "nrd AND vdt -> hwm"
           "kjc AND fst -> rvg"
           "y04 OR y02 -> fgs"
           "y01 AND x02 -> pbm"
           "ntg OR kjc -> kwq"
           "psh XOR fgs -> tgd"
           "qhw XOR tgd -> z09"
           "pbm OR djm -> kpj"
           "x03 XOR y03 -> ffh"
           "x00 XOR y04 -> ntg"
           "bfw OR bqk -> z06"
           "nrd XOR fgs -> wpb"
           "frj XOR qhw -> z04"
           "bqk OR frj -> z07"
           "y03 OR x01 -> nrd"
           "hwm AND bqk -> z03"
           "tgd XOR rvg -> z12"
           "tnw OR pbm -> gnj"

           |]

    [<Fact>]
    let ``Part 1 example 1`` () = part1 example1 =! 0b100

    [<Fact>]
    let ``Part 1 example 2`` () = part1 example2 =! 2024


    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! -1
