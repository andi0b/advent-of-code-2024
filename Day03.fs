module aoc24.Day03

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type MulRegex = Regex< @"mul\((?<factor1>\d+),(?<factor2>\d+)\)" >

let parseMuls input =
    MulRegex().TypedMatches(input)
    |> Seq.map (fun m -> (m.factor1.AsInt, m.factor2.AsInt))

type DoRegex = Regex< @"(?s)(?:do\(\)|^)(?<enabledOps>.*?)(?:don't\(\)|$)" >

let parseEnabledMuls input =
    DoRegex().TypedMatches(input)
    |> Seq.map _.enabledOps.Value
    |> Seq.collect parseMuls

let mulSums = 0 |> Seq.fold (fun acc (a, b) -> acc + a * b)

let part1 input = input |> parseMuls |> mulSums
let part2 input = input |> parseEnabledMuls |> mulSums
let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 161

    [<Theory>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()\n?mul(8,5))")>]
    let ``Part 2 example`` (input) = part2 input =! 48
