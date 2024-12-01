module aoc24.Day01

let parse =
    Array.map (fun l ->
        let parts = l |> StringEx.splitS "   "
        (int parts[0], int parts[1]))
    >> Array.unzip

let part1 input =
    let sortedPairs =
        input |> parse |> TupleEx.map Array.sort |> TupleEx.apply Array.zip

    (0, sortedPairs)
    ||> Array.fold (fun acc pairs -> acc + (pairs |> TupleEx.apply (-) |> abs))

let part2 input =
    let left, right = parse input

    let counts =
        right |> Array.groupBy id |> Array.map (TupleEx.mapSnd Array.length) |> Map

    (0, left)
    ||> Array.fold (fun acc left ->
        let rightCounts = counts |> Map.tryFind left |> Option.defaultValue 0
        acc + left * rightCounts)

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "3   4" //
           "4   3"
           "2   5"
           "1   3"
           "3   9"
           "3   3" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 11

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 31
