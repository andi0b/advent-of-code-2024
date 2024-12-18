module aoc24.Day17

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type R = Regex< @"(?s)Register A: (?<A>\d+).*Register B: (?<B>\d+).*Register C: (?<C>\d+).*Program: ((?<p>\d)[,]?)*" >

type ChronoComputer =
    { mutable a: int64
      mutable b: int64
      mutable c: int64
      mutable ip: int
      program: int array }

module ChronoComputer =
    let parse input =
        let m = R().TypedMatch(input)

        { a = m.A.AsInt64
          b = m.B.AsInt64
          c = m.C.AsInt64
          ip = 0
          program = m.p.Captures |> Seq.map (fun x -> int x.Value) |> Seq.toArray }

    let run cc =

        let jmp2 () = cc.ip <- cc.ip + 2

        seq {
            while cc.ip < cc.program.Length do

                let opcode = cc.program[cc.ip]
                let operand = cc.program[cc.ip + 1]

                let combo =
                    match operand with
                    | 4 -> cc.a
                    | 5 -> cc.b
                    | 6 -> cc.c
                    | x -> int64 x

                match opcode with
                | 0 (*adv*) -> cc.a <- cc.a / (1L <<< (int combo))
                | 6 (*bdv*) -> cc.b <- cc.a / (1L <<< (int combo))
                | 7 (*cdv*) -> cc.c <- cc.a / (1L <<< (int combo))
                | 1 (*bxl*) -> cc.b <- cc.b ^^^ operand
                | 2 (*bst*) -> cc.b <- (combo &&& 0b111)
                | 3 (*jnz*) -> if cc.a = 0 then jmp2 () else cc.ip <- operand
                | 4 (*bxc*) -> cc.b <- cc.b ^^^ cc.c
                | 5 (*out*) -> yield combo &&& 0b111
                | _ -> failwith "huh?"

                match opcode with
                | 3 -> ()
                | _ -> jmp2 ()
        }


let part1 input =
    let mutable cc = ChronoComputer.parse input
    cc |> ChronoComputer.run |> Seq.map string |> String.concat ","

type Match =
    | PartialMatch
    | FullMatch
    | NoMatch

let part2 input =
    let template = ChronoComputer.parse input
    let program = template.program |> Array.map int64

    // print first few values with i formatted as binary, octal, hex and decimal to stdout, to recognize output pattern
    (*
    for i=0 to 0o1000 do
        let cc = template.Clone()
        cc.seta i
        printfn $"""0b%12B{i}    0o%4o{i}    0x%4x{i}     %6i{i}: {cc.run () |> Seq.map string |> String.concat ","}"""
    *)

    let revProg = template.program |> Array.map int64 |> Array.rev

    let runAndTestWith a =
        let cc = { template with a = a }
        let revOut = cc |> ChronoComputer.run |> Seq.rev |> Seq.toArray

        //printfn $"""Trying a=0o%08o{a} with result (reverse): {revOut |> Seq.map string |> String.concat ","}"""

        if (revOut, revProg) ||> Array.compareWith Operators.compare = 0 then
            FullMatch
        elif (revOut, revProg) ||> Seq.forall2 (=) then
            PartialMatch
        else
            NoMatch

    let rec findA (start: int64) =
        //printfn $"findA start=0o%021o{start}"

        let rec loop =
            function
            | 0o10L -> None
            | i ->
                let a = (start <<< 3) + i

                match runAndTestWith a with
                | FullMatch -> Some a
                | NoMatch -> loop (i + 1L)
                | PartialMatch when a <> start ->
                    match findA a with
                    | None -> loop (i + 1L)
                    | x -> x
                | PartialMatch -> loop (i + 1L)

        loop 0L

    findA 0 |> Option.defaultValue -1

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! "4,6,3,5,6,3,5,2,1,0"


    [<Fact>]
    let ``Part 1 additional examples`` () =
        let run input =
            let c = ChronoComputer.parse input
            c |> ChronoComputer.run |> Seq.toArray |> ignore
            c

        // If register C contains 9, the program 2,6 would set register B to 1.
        test <@ (run "Register A: 0 Register B: 0 Register C: 9 Program: 2,6").b = 1 @>

        // If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
        test <@ part1 "Register A: 10 Register B: 0 Register C: 0 Program: 5,0,5,1,5,4" = "0,1,2" @>

        // If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
        test <@ part1 "Register A: 2024 Register B: 0 Register C: 0 Program: 0,1,5,4,3,0" = "4,2,5,6,7,7,7,7,3,1,0" @>
        test <@ (run "Register A: 0 Register B: 0 Register C: 9 Program: 2,6").a = 0 @>

        // If register B contains 29, the program 1,7 would set register B to 26.
        test <@ (run "Register A: 0 Register B: 29 Register C: 0 Program: 1,7").b = 26 @>

        // If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
        test <@ (run "Register A: 0 Register B: 2024 Register C: 43690 Program: 4,0").b = 44354 @>


    let example2 =
        "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! 117440

    [<Fact>]
    let ``parse example1`` () =
        let cc = ChronoComputer.parse example1
        [| cc.a; cc.b; cc.c |] =! [| 729; 0; 0 |]
        cc.program =! [| 0; 1; 5; 4; 3; 0 |]
