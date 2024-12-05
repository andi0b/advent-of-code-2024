module aoc24.Day03noregex

open System

/// Matches a single digit char and returns it's int value
let (|Digit|_|) =
    function
    | c when Char.IsAsciiDigit c -> Some(int c - int '0')
    | _ -> None

/// matches a 1-3 digit number with a delimiter, for example '123,' or '2)'
let (|Number|_|) delim =
    function
    | Digit a :: Digit b :: Digit c :: x :: tail when x = delim -> Some(a * 100 + b * 10 + c, tail)
    | Digit a :: Digit b :: x :: tail when x = delim -> Some(a * 10 + b, tail)
    | Digit a :: x :: tail when x = delim -> Some(a, tail)
    | _ -> None

/// matches a 'mul(a,b)' and returns a, b and the remaining characters after
let (|Mul|_|) =
    function
    | 'm' :: 'u' :: 'l' :: '(' :: Number ',' (a, tail) ->
        match tail with
        | Number ')' (b, tail) -> Some(a, b, tail)
        | _ -> None
    | _ -> None

let mulSums = Seq.sumBy (TupleEx.apply (*))

let part1 input =
    let rec parse output =
        function
        | Mul(a, b, tail) -> parse ((a, b) :: output) tail
        | _ :: tail -> parse output tail
        | [] -> output

    let muls = input |> List.ofSeq |> parse []
    muls |> mulSums

/// matches 'do()' and returns the remaining characters after
let (|Do|_|) =
    function
    | 'd' :: 'o' :: '(' :: ')' :: tail -> Some tail
    | _ -> None

/// matches 'don't()' and returns the remaining characters after
let (|Dont|_|) =
    function
    | 'd' :: 'o' :: 'n' :: ''' :: 't' :: '(' :: ')' :: tail -> Some tail
    | _ -> None

type DoState =
    | Enabled
    | Disabled

let part2 input =
    let rec parse output doState =
        function
        | Do tail -> parse output Enabled tail
        | Dont tail -> parse output Disabled tail
        | Mul(a, b, tail) when doState.IsEnabled -> parse ((a, b) :: output) doState tail
        | _ :: tail -> parse output doState tail
        | [] -> output

    let muls = input |> List.ofSeq |> parse [] Enabled
    muls |> mulSums

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

    [<Fact>]
    let ``Part 1 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part1 =! 156388521


    [<Fact>]
    let ``Part 2 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part2 =! 75920122
