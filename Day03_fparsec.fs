module aoc24.Day03fparsec

open FParsec

type UserState =
    { InsideDo: bool
      IgnoreDont: bool }

    static member Default = { InsideDo = true; IgnoreDont = false }

    static member DefaultIgnoreDont =
        { UserState.Default with
            IgnoreDont = true }

let setUsInsideDo e =
    updateUserState (fun us -> { us with InsideDo = e })

let usSatisfiesInsideDo =
    userStateSatisfies (fun us -> us.InsideDo || us.IgnoreDont)

let do_ = skipString "do()" .>> setUsInsideDo true
let dont_ = skipString "don't()" .>> setUsInsideDo false
let skips = choice [ do_; dont_; skipAnyChar ]

let mul =
    let args = pint32 .>> pchar ',' .>>. pint32
    let mul = (pstring "mul(") >>. args .>> (pstring ")")
    usSatisfiesInsideDo >>. mul

let parser =
    let nextMul = skipManyTill skips (followedBy mul) >>. mul
    nextMul |> attempt |> many

let parse userState input =
    runParserOnString parser userState "" input
    |> function
        | Success(result, _, _) -> result
        | Failure(msg, _, _) -> failwith msg

let mulSums = Seq.sumBy (TupleEx.apply (*))

let part1 input =
    input |> parse UserState.DefaultIgnoreDont |> mulSums

let part2 input =
    input |> parse UserState.Default |> mulSums


open Swensen.Unquote
open Xunit

module tests =

    let example1 =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 161

    [<Theory>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")>]
    [<InlineData("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()\n?mul(8,5))")>]
    let ``Part 2 example`` (input) = part2 input =! 48


    [<Fact(Skip = "requires file")>]
    let ``Part 1 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part1 =! 156388521

    [<Fact(Skip = "requires file")>]
    let ``Part 2 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part2 =! 75920122

module ``FParsec Tests`` =

    let test parser input =
        runParserOnString parser UserState.Default "" input
        |> function
            | Success(result, _, _) -> Result.Ok result
            | Failure(msg, _, _) -> Result.Error msg

    [<Fact>]
    let ``Test mul(123,456)`` () =
        test mul "mul(123,456)" =! Result.Ok(123, 456)

    [<Fact>]
    let ``Test mul(123,456`` () =
        test mul "mul(123,456]" <>! Result.Ok(123, 456)
