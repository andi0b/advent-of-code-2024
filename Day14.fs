module aoc24.Day14

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type Point = (struct (int64 * int64))
type Robot = { loc: Point; vec: Point }


module Point =
    let inline op f ((a, b): Point) ((c, d): Point) = Point(f a c, f b d)

let inline (.+) a b = Point.op (+) a b
let inline (.-) a b = Point.op (-) a b
let inline (.%) a b = Point.op (%) a b
let inline (..*) ((x, y): Point) i = Point(x * i, y * i)
let inline (../) ((x, y): Point) i = Point(x / i, y / i)

module Robot =
    type private R = Regex< @"p=(?<px>\d+),(?<py>\d+) v=(?<vx>-?\d+),(?<vy>-?\d+)" >

    let parse =
        R().TypedMatch
        >> (fun m ->
            { loc = Point(m.px.AsInt64, m.py.AsInt64)
              vec = Point(m.vx.AsInt64, m.vy.AsInt64) })

    let parseMany = Array.map parse

    let move (bounds: Point) times robot =
        let result = robot.loc .+ (robot.vec .+ bounds) ..* times
        result .% bounds

    let quadrantSelector bounds =
        let ops = [ (>); (<) ]

        let selectors =
            List.allPairs ops ops
            |> List.mapi (fun i (xop, yop) ->
                let struct (xBound, yBound) = bounds ../ 2L
                fun ((x, y): Point) -> xop x xBound && yop y yBound)

        fun (p: Point) -> selectors |> List.tryFindIndex (fun s -> s p)


let solve1 bounds input =
    let robots = input |> Robot.parseMany
    let finalPositions = robots |> Array.map (Robot.move bounds 100)

    let qs = Robot.quadrantSelector bounds

    finalPositions
    |> Array.groupBy qs
    |> Array.filter (fun (key, _) -> key.IsSome)
    |> Array.map (fun (_, robots) -> robots |> Array.length)
    |> Array.reduce (*)

let part1 input =
    let bounds = Point(101L, 103L)
    solve1 bounds input

let prettyPrint pos bounds =
    seq {
        yield System.Environment.NewLine
        for l = 0 to (bounds |> vsnd |> int) / 3 do
            for c = 0 to (bounds |> vfst |> int) do

                let co offset =
                    pos |> Set.contains (Point(c, 3 * l + offset |> int64))

                yield
                    match [ co 0; co 1; co 2 ] with
                    | [ false; false; false ] -> " "
                    | [ true; true; true ] -> "█"
                    | [ true; true; false ] -> "▀"
                    | [ true; false; false ] -> "▔"
                    | [ true; false; true ] -> "▒"
                    | [ false; true; false ] -> "─"
                    | [ false; false; true ] -> "▃"
                    | [ false; true; true ] -> "▆"
                    | _ -> " "

            yield System.Environment.NewLine
    }
    |> System.String.Concat
    |> printfn "%s"

open FSharp.Stats

let part2 input =
    let bounds = Point(101L, 103L)
    let robots = input |> Robot.parseMany

    let varianceSums =
        let count = bounds |> ValueTupleEx.apply (*) |> int

        Array.Parallel.init count (fun i ->
            let robotPos = robots |> Array.map (Robot.move bounds i)
            let xvar = robotPos |> varBy (fun struct (x, _) -> float x)
            let yvar = robotPos |> varBy (fun struct (_, y) -> float y)
            xvar + yvar)

    let averageVarianceSums = varianceSums |> Array.average

    let outlier =
        varianceSums |> Array.findIndex (fun x -> x < averageVarianceSums * 0.5)

    let robotPos = robots |> Array.map (Robot.move bounds outlier)
    prettyPrint (robotPos |> Set) bounds

    outlier


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "p=0,4 v=3,-3"
           "p=6,3 v=-1,-3"
           "p=10,3 v=-1,2"
           "p=2,0 v=2,-1"
           "p=0,0 v=1,3"
           "p=3,0 v=-2,-2"
           "p=7,6 v=-1,-3"
           "p=3,0 v=-1,-2"
           "p=9,3 v=2,3"
           "p=7,3 v=-1,2"
           "p=2,4 v=2,-3"
           "p=9,5 v=-3,-3" |]

    [<Fact>]
    let ``Part 1 example`` () = solve1 (Point(11L, 7L)) example1 =! 12

    [<Fact>]
    let ``parse example`` () =
        let parsed = example1 |> Robot.parseMany

        parsed[0]
        =! { loc = Point(0L, 4L)
             vec = Point(3L, -3L) }

        parsed[1]
        =! { loc = Point(6L, 3L)
             vec = Point(-1L, -3L) }

    [<Fact>]
    let ``move`` () =
        let robot = "p=2,4 v=2,-3" |> Robot.parse
        let move = Robot.move (Point(11L, 7L))

        test <@ robot |> move 1 = Point(4L, 1L) @>
        test <@ robot |> move 2 = Point(6L, 5L) @>
        test <@ robot |> move 3 = Point(8L, 2L) @>
        test <@ robot |> move 4 = Point(10L, 6L) @>
        test <@ robot |> move 5 = Point(1L, 3L) @>

        ()
