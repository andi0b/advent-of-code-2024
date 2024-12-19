module aoc24.Day19

open FParsec

let parseInput (input: string array) =
    let towels = input[0] |> StringEx.splitS ", "
    let arrangements = input[2..]
    (towels, arrangements)


let part1 input =
    let towels, arrangements = parseInput input

    let parser =
        let towelTokens = towels |> Array.toList |> List.map pstring

        let nextToken, nextTokenRef = createParserForwardedToRef ()

        let chainedTowelToken =
            towelTokens
            |> List.map (fun t -> pipe2 t nextToken (fun a b -> a :: b) |> attempt)

        nextTokenRef.Value <- choice [ eof >>% []; choice chainedTowelToken ]

        nextToken

    let canParse input =
        match run parser input with
        | Success(res, _, _) ->
            dprintfn $"success '{input}': %A{res}"
            true
        | Failure(msg, _, _) ->
            dprintfn ""
            dprintfn $"error '{input}': %A{msg}"
            false

    arrangements |> Array.filter canParse |> Array.length

let part2 input =
    let towels, arrangements = parseInput input

    let countPossibilities (arrangement: string) =

        let rec loop result tails =
            dprintfn $"Previous Results: {result}, Tails: %A{tails}"

            let successes =
                tails |> Array.sumBy (fun (count, tail) -> if tail = "" then count else 0L)

            let nextTails =
                tails
                |> Array.collect (fun (count, tail) ->
                    towels
                    |> Array.choose (fun towel ->
                        if tail.StartsWith(towel) then
                            Some(count, tail.Substring(towel.Length))
                        else
                            None))

            let compactedTails =
                nextTails
                |> Array.groupBy snd
                |> Array.map (fun (key, value) -> (value |> Array.sumBy fst, key))

            if nextTails.Length > 0 then
                loop (result + successes) compactedTails
            else
                (result + successes)

        dprintfn $"\n\nTrying arrangement: {arrangement}"

        loop 0 [| 1L, arrangement |]

    arrangements |> Array.Parallel.sumBy countPossibilities


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
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
    let ``Part 1 example`` () = part1 example =! 6

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 16
