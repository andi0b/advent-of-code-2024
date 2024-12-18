module aoc24.Day17

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type R = Regex< @"(?s)Register A: (?<A>\d+).*Register B: (?<B>\d+).*Register C: (?<C>\d+).*Program: ((?<p>\d)[,]?)*" >


type ChronoComputer(a: int64, b: int64, c: int64, ip: int, program: int array) =

    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable ip = ip

    let jmp2 () = ip <- ip + 2

    new(input) =
        let m = R().TypedMatch(input)

        ChronoComputer(
            m.A.AsInt64,
            m.B.AsInt64,
            m.C.AsInt64,
            0,
            m.p.Captures |> Seq.map (fun x -> int x.Value) |> Seq.toArray
        )

    member this.Clone() = ChronoComputer(a, b, c, ip, program)

    member this.seta newa = a <- newa

    member this.Registers = [| a; b; c |]
    member this.Program = program

    member this.run() =
        seq {
            while ip < program.Length do

                let opcode = program[ip]
                let operand = program[ip + 1]

                let combo =
                    match operand with
                    | 4 -> a
                    | 5 -> b
                    | 6 -> c
                    | x -> int64 x

                //printfn $"op: {opcode} operand: {operand} combo: {combo}; A:{a} B:{b} C:{c}"

                match opcode with
                | 0 -> // adv
                    a <- a / (1L <<< (int combo))
                    jmp2 ()

                | 6 -> // bdv
                    b <- a / (1L <<< (int combo))
                    jmp2 ()

                | 7 -> // cdv
                    c <- a / (1L <<< (int combo))
                    jmp2 ()

                | 1 -> // bxl
                    b <- b ^^^ operand
                    jmp2 ()

                | 2 -> // bst
                    b <- (combo &&& 0b111)
                    jmp2 ()

                | 3 -> // jnz
                    if a = 0 then jmp2 () else ip <- operand

                | 4 -> // bxc
                    b <- b ^^^ c
                    jmp2 ()

                | 5 -> // out
                    //printfn $"out: {combo % 8}"
                    yield combo &&& 0b111
                    jmp2 ()

                | _ -> failwith "huh?"
        }


let part1 input =
    let mutable cc = ChronoComputer(input)
    cc.run() |> Seq.map string |> String.concat ","


// obviously too slow for real input
let part2 input =
    let template = ChronoComputer(input)
    let program = template.Program |> Array.map int64

    let mutable solution = -1L
    let mutable i = 0L


    while solution = -1 do
        let cc = template.Clone()
        cc.seta i

        if ((cc.run (), program) ||> Seq.compareWith Operators.compare) = 0 then
            solution <- i
        else
            i <- i + 1L


            let cc1 = template.Clone()
            cc1.seta i
            printfn $"""%32B{i}: {cc1.run () |> Seq.map string |> String.concat ","}"""

    solution

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
            let c = ChronoComputer(input)
            c.run () |> Seq.toArray |> ignore
            c

        // If register C contains 9, the program 2,6 would set register B to 1.
        test <@ (run "Register A: 0 Register B: 0 Register C: 9 Program: 2,6").Registers[1] = 1 @>

        // If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
        test <@ part1 "Register A: 10 Register B: 0 Register C: 0 Program: 5,0,5,1,5,4" = "0,1,2" @>

        // If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
        test <@ part1 "Register A: 2024 Register B: 0 Register C: 0 Program: 0,1,5,4,3,0" = "4,2,5,6,7,7,7,7,3,1,0" @>
        test <@ (run "Register A: 0 Register B: 0 Register C: 9 Program: 2,6").Registers[0] = 0 @>

        // If register B contains 29, the program 1,7 would set register B to 26.
        test <@ (run "Register A: 0 Register B: 29 Register C: 0 Program: 1,7").Registers[1] = 26 @>

        // If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.
        test <@ (run "Register A: 0 Register B: 2024 Register C: 43690 Program: 4,0").Registers[1] = 44354 @>


    let example2 =
        "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! 117440

    [<Fact>]
    let ``parse example1`` () =
        let c = ChronoComputer(example1)
        c.Registers =! [| 729; 0; 0 |]
        c.Program =! [| 0; 1; 5; 4; 3; 0 |]
