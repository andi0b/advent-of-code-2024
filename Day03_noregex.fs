module aoc24.Day03noregex

open System

let (|Digit|_|) =
    function
    | c when Char.IsAsciiDigit c -> Some(int c - int '0')
    | _ -> None

let (|Number|_|) delim =
    function
    | Digit a :: Digit b :: Digit c :: x :: tail when x = delim -> Some(a * 100 + b * 10 + c, tail)
    | Digit a :: Digit b :: x :: tail when x = delim -> Some(a * 10 + b, tail)
    | Digit a :: x :: tail when x = delim -> Some(a, tail)
    | _ -> None

let (|Mul|_|) =
    function
    | 'm' :: 'u' :: 'l' :: '(' :: Number ',' (a, tail) ->
        match tail with
        | Number ')' (b, tail) -> Some(a, b, tail)
        | _ -> None
    | _ -> None

let mulSums = Seq.sumBy (TupleEx.apply (*))

let part1 input =
    let rec parse input output =
        match input with
        | Mul(a, b, tail) -> parse tail ((a, b) :: output)
        | [] -> output
        | _ :: tail -> parse tail output

    let muls = parse (input |> List.ofSeq) []
    muls |> mulSums

let (|Do|_|) =
    function
    | 'd' :: 'o' :: '(' :: ')' :: tail -> Some tail
    | _ -> None

let (|Dont|_|) =
    function
    | 'd' :: 'o' :: 'n' :: ''' :: 't' :: '(' :: ')' :: tail -> Some tail
    | _ -> None

let part2 input =
    let rec parse input output enabled =
        match input with
        | Do tail -> parse tail output true
        | Dont tail -> parse tail output false
        | Mul(a, b, tail) when enabled -> parse tail ((a, b) :: output) enabled
        | [] -> output
        | _ :: tail -> parse tail output enabled

    let muls = parse (input |> List.ofSeq) [] true
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
    let ``Part 2 realworld`` () =
        System.IO.File.ReadAllText("../../../inputs/day03.txt") |> part2 =! 75920122
