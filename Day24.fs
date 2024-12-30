module aoc24.Day24

open System.Collections.Generic
open System.Diagnostics

type BinaryOp =
    | And
    | Xor
    | Or

    static member parse =
        function
        | "AND" -> And
        | "OR" -> Or
        | "XOR" -> Xor
        | _ -> failwith "unknown op"

type Op = string * BinaryOp * string * string

module Op =
    let in1 ((in1, _, _, _): Op) = in1
    let op ((_, op, _, _): Op) = op
    let in2 ((_, _, in2, _): Op) = in2
    let out ((_, _, _, out): Op) = out

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
            let inops = [| parts[0]; parts[2] |] |> Array.sort
            (inops[0], parts[1] |> BinaryOp.parse, inops[1], parts[4]))

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


// FML
let part2 input =

    let _, ops = parse input

    let isIn = Op.in1 >> _.StartsWith("x")
    let isOut = Op.out >> _.StartsWith("z")
    let isFirstIn = Op.in1 >> ((=) "x00")
    let isLastOut = Op.out >> ((=) "z45")

    // outputs except z45 must be XORs, otherwise they are wrongly wired
    let invalidOutputGates =
        ops |> Array.filter (fun x -> isOut x && not <| isLastOut x && Op.op x <> Xor)

    // XOR gates are always connected to either x/y on the in-side or z on the out side, otherwise wrongly wired
    let invalidXorGates =
        ops |> Array.filter (fun x -> Op.op x = Xor && not <| isIn x && not <| isOut x)

    let insExceptFirst = ops |> Array.filter (fun x -> isIn x && not <| isFirstIn x)

    // non-first XOR in gates must be connected to another XOR gate
    let invalidXorIngates =
        insExceptFirst
        |> Array.filter (Op.op >> ((=) Xor))
        |> Array.filter (fun x ->
            ops
            |> Array.exists (fun (in1, op, in2, _) -> op = Xor && (in1 = Op.out x || in2 = Op.out x))
            |> not)

    // non-first AND in gates must be connected to an OR gate
    let invalidAndIngates =
        insExceptFirst
        |> Array.filter (Op.op >> ((=) And))
        |> Array.filter (fun x ->
            ops
            |> Array.exists (fun (in1, op, in2, _) -> op = Or && (in1 = Op.out x || in2 = Op.out x))
            |> not)

    (*
    printfn $"wrongOutputGates: %A{invalidOutputGates}"
    printfn $"invalidXorGates: %A{invalidXorGates}"
    printfn $"invalidXorIngates: %A{invalidXorIngates}"
    printfn $"invalidAndIngates: %A{invalidAndIngates}"
    *)

    [| invalidOutputGates; invalidXorGates; invalidXorIngates; invalidAndIngates |]
    |> Array.collect id
    |> Array.map Op.out
    |> Array.distinct
    |> Array.sort
    |> String.concat ","


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
           "tnw OR pbm -> gnj" |]

    [<Fact>]
    let ``Part 1 example 1`` () = part1 example1 =! 0b100

    [<Fact>]
    let ``Part 1 example 2`` () = part1 example2 =! 2024

    open System.IO

    let pipeToCode extension (writerFun: StreamWriter -> unit) =
        let tempFile = Path.GetTempFileName() + extension
        use writer = new StreamWriter(File.OpenWrite tempFile)
        writerFun writer
        writer.Close()

        use proc =
            new Process(
                StartInfo =
                    ProcessStartInfo(
                        "code",
                        Arguments = tempFile,
                        UseShellExecute = true,
                        WindowStyle = ProcessWindowStyle.Hidden
                    )
            )

        proc.Start() |> ignore
        proc.WaitForExit()

        ()


    let plot values ops =
        let (|StartsWith|_|) (str: string) (input: string) = input.StartsWith(str)

        pipeToCode ".dot" (fun tw ->

            fprintfn tw "digraph {"

            let node name shape =
                let color =
                    match name with
                    | StartsWith "x" -> "lightgreen"
                    | StartsWith "y" -> "lightblue"
                    | StartsWith "z" -> "yellow"
                    | _ -> "white"

                let shapem =
                    match (shape, name) with
                    | "", (StartsWith "x" | StartsWith "y") -> "invhouse"
                    | "", StartsWith "z" -> "house"
                    | "", _ -> "oval"
                    | _ -> shape

                fprintfn tw $"""    {name} [label="{name}" style=filled fillcolor={color} shape={shapem}]"""


            for name, _ in values do
                node name ""

            for in1, op, in2, out in ops do
                let arrow, shape, weight =
                    match op with
                    | And -> "dot", "triangle", 2
                    | Or -> "odot", "invtriangle", 1
                    | Xor -> "diamond", "diamond", 5

                fprintfn tw $"""    {{{in1}, {in2}}} -> {out} [ weight={weight} ]"""
                node out shape

            fprintfn tw "}")

    let testFile = "../../../inputs/day24.txt"
    let testFileExists = File.Exists(testFile)

    [<Fact(Skip = "requires test file", SkipUnless = nameof testFileExists)>]
    let plotInput () =
        let values, ops = File.ReadAllLines testFile |> parse
        plot values ops
