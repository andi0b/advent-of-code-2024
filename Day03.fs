module aoc24.Day03

open FSharp.Text.RegexProvider
open FSharp.Text.RegexExtensions

type MulRegex = Regex< @"mul\((?<factor1>\d+),(?<factor2>\d+)\)" >
type DoRegex = Regex< @"(?s)(?:do\(\)|^)(?<enabledOps>.*?)(?:don't\(\)|$)" >

let parseMuls input =
    let matches = MulRegex().TypedMatches(input)
    matches |> Seq.map (fun m -> (m.factor1.AsInt, m.factor2.AsInt))

let parseEnabledMuls input =
    let enabledParts = DoRegex().TypedMatches(input)
    enabledParts |> Seq.map _.enabledOps.Value  |> Seq.collect parseMuls

let mulSums =
    Seq.map (TupleEx.apply (*)) >> Seq.sum

let part1 input =
    parseMuls input
    |> mulSums

let part2 input =
    parseEnabledMuls input
    |> mulSums

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 161

    [<Theory>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()\n?mul(8,5))")>]
    let ``Part 2 example`` (input) = part2 input =! 48

