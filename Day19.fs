module aoc24.Day19

open FParsec

let parseInput (input: string array) =
    let towels = input[0] |> StringEx.splitS ", "
    let arrangements = input[2..]
    (towels, arrangements)

let buildParser towels =
    let towelTokens = towels |> Array.toList |> List.map pstring

    let nextToken, nextTokenRef = createParserForwardedToRef ()

    let chainedTowelToken =
        towelTokens
        |> List.map (fun t -> pipe2 t nextToken (fun a b -> a :: b) |> attempt)

    nextTokenRef.Value <- choice [ eof >>% []; choice chainedTowelToken ]

    nextToken

let canParse parser input =
    match run parser input with
    | Success(res, _, _) ->
#if DEBUG
        printfn $"success '{input}': %A{res}"
#endif
        true
    | Failure(msg, _, _) ->
#if DEBUG
        printfn ""
        printfn $"error '{input}': %A{msg}"
#endif
        false

let part1 input =
    let towels, arrangements = parseInput input
    let parser = buildParser towels
    arrangements |> Array.filter (canParse parser) |> Array.length

let part2 = (fun _ -> 0)

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "r, wr, b, g, bwu, rb, gb, br"
           ""
           "brwrr"
           "bggr"
           "gbbr"
           "rrbgbr"
           "ubwu"
           "bwurrg"
           "brgr"
           "bbrgwb" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 6

    let example2 = null

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! -1
