module aoc24.Day13

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type Point = int64 * int64

module Point =
    let inline op f ((a, b): Point) ((c, d): Point) = (f a c, f b d)
    let inline both f ((ax, ay): Point) ((bx, by): Point) = f ax bx && f ay by
    let inline one f ((ax, ay): Point) ((bx, by): Point) = f ax bx || f ay by

let inline (.+) a b = Point.op (+) a b
let inline (.-) a b = Point.op (-) a b
let inline (.*) ((x, y): Point) i = Point(x * i, y * i)

let inline (.<<) a b = Point.both (<) a b
let inline (.<) a b = Point.one (<) a b

let inline (.>>) a b = Point.both (>) a b
let inline (.>) a b = Point.one (>) a b

type R =
    Regex<"Button A: X\+(?<ax>\d+), Y\+(?<ay>\d+)\\n\
           Button B: X\+(?<bx>\d+), Y\+(?<by>\d+)\\n\
           Prize: X=(?<px>\d+), Y=(?<py>\d+)">

type Machine =
    { ButtonA: Point
      ButtonB: Point
      Prize: Point }

let parse input =
    input
    |> R().TypedMatches
    |> Seq.map (fun m ->
        { ButtonA = Point(m.ax.AsInt, m.ay.AsInt)
          ButtonB = Point(m.bx.AsInt, m.by.AsInt)
          Prize = Point(m.px.AsInt, m.py.AsInt) })
    |> Seq.toArray


let findCheapestCombination machine =
    let rec bCount i =
        let target = machine.ButtonB .* i
        if target .<< machine.Prize then bCount (i + 1L) else i

    let b = bCount 0

    let rec solve a b =
        let target = machine.ButtonA .* a .+ machine.ButtonB .* b

        if target = machine.Prize then Some(a, b)
        elif a > 100 then None
        elif target .> machine.Prize then solve a (b - 1L)
        else solve (a + 1L) b

    solve 0 (b |> min 100)


// wolfram alpha query: solve a_1*a + b_1*b = p_1, a_2*a + b_2*b = p_2 for a and b
// ->
// a = (b_2 p_1 - b_1 p_2)/(a_1 b_2 - a_2 b_1)
// b = (a_2 p_1 - a_1 p_2)/(a_2 b_1 - a_1 b_2)
// with a_2 b_1!=a_1 b_2 and b_2 !=0
let findCheapestCombinationFast machine =
    let f = TupleEx.map float
    let a1, a2 = f machine.ButtonA
    let b1, b2 = f machine.ButtonB
    let p1, p2 = f machine.Prize

    if a2 * b2 = a1 * b2 || b2 = 0 then // wolfram alpha says that's bad.
        None
    else
        let a = (b2 * p1 - b1 * p2) / (a1 * b2 - a2 * b1)
        let b = (a2 * p1 - a1 * p2) / (a2 * b1 - a1 * b2)

        let isInt = System.Double.IsInteger

        if a >= 0 && b >= 0 && isInt a && isInt b then
            Some(int64 a, int64 b)
        else
            None

let part1 input =
    parse input
    |> Array.choose findCheapestCombination
    |> Array.sumBy (fun (a, b) -> a * 3L + b)

let part2 input =
    parse input
    |> Array.map (fun m ->
        { m with
            Prize = m.Prize .+ (10000000000000L, 10000000000000L) })
    |> Array.choose findCheapestCombinationFast
    |> Array.sumBy (fun (a, b) -> a * 3L + b)

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        "Button A: X+94, Y+34\n\
         Button B: X+22, Y+67\n\
         Prize: X=8400, Y=5400\n\
         \n\
         Button A: X+26, Y+66\n\
         Button B: X+67, Y+21\n\
         Prize: X=12748, Y=12176\n\
         \n\
         Button A: X+17, Y+86\n\
         Button B: X+84, Y+37\n\
         Prize: X=7870, Y=6450\n\
         \n\
         Button A: X+69, Y+23\n\
         Button B: X+27, Y+71\n\
         Prize: X=18641, Y=10279"

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 480L

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 875318608908L

    let example1Machine =
        { ButtonA = Point(94, 34)
          ButtonB = Point(22, 67)
          Prize = Point(8400, 5400) }

    [<Fact>]
    let ``parse example`` () =
        let parsed = example |> parse
        parsed.Length =! 4

        parsed[0] =! example1Machine

    [<Fact>]
    let ``check solution`` () =
        let button1Vec = example1Machine.ButtonA .* 80
        let button2Vec = example1Machine.ButtonB .* 40

        button1Vec .+ button2Vec =! example1Machine.Prize

        example1Machine.ButtonA .* 80 .+ example1Machine.ButtonB .* 40
        =! example1Machine.Prize
