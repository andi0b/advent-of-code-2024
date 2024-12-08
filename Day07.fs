module aoc24.Day07

let parseLine input =
    match input |> StringEx.splitSs [| " "; ": " |] |> Seq.map int64 |> Seq.toList with
    | head :: tail -> (head, tail)
    | _ -> failwith $"parse error: {input}"

let parse = Array.map parseLine

let allCombinations times (array: 'a array) =
    let rec loop times list =
        seq {
            for i = 0 to array.Length - 1 do
                match times with
                | 1 -> yield array.[i] :: list
                | _ -> yield! loop (times - 1) (array.[i] :: list)
        }

    loop times []

let findSolutions ops (expected, numbers) =
    allCombinations (List.length numbers - 1) ops
    |> Seq.filter (fun ops ->
        let result =
            ((ops, numbers.Head), numbers.Tail)
            ||> List.fold (fun (remainingOps, accumulate) next ->
                (remainingOps.Tail, remainingOps.Head accumulate next))
            |> snd

        result = expected)

let solve ops input =
    input
    |> parse
    |> Array.Parallel.filter (fun e -> (findSolutions ops e) |> Seq.exists (fun _ -> true))
    |> Array.sumBy fst

let part1 = solve [| (+); (*) |]

let inline (.||) a b =
    let bDigitCount = (log10 (float b) |> int) + 1
    (pown 10L bDigitCount) * a + b

let part2 = solve [| (+); (*); (.||) |]

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "190: 10 19"
           "3267: 81 40 27"
           "83: 17 5"
           "156: 15 6"
           "7290: 6 8 6 15"
           "161011: 16 10 13"
           "192: 17 8 14"
           "21037: 9 7 18 13"
           "292: 11 6 16 20" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 3749

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 11387

    let findSolutionCount ops equation =
        findSolutions ops equation |> Seq.length

    [<Theory>]
    [<InlineData("190: 10 19", 1)>]
    [<InlineData("3267: 81 40 27", 2)>]
    [<InlineData("292: 11 6 16 20", 1)>]
    [<InlineData("21037: 9 7 18 13", 0)>]
    let ``Part 1 solution count`` line count =
        line |> parseLine |> findSolutionCount [| (+); (*) |] =! count

    [<Theory>]
    [<InlineData("156: 15 6", 1)>]
    [<InlineData("7290: 6 8 6 15", 1)>]
    [<InlineData("192: 17 8 14", 1)>]
    [<InlineData("21037: 9 7 18 13", 0)>]
    let ``Part 2 solution count`` line count =
        line |> parseLine |> findSolutionCount [| (+); (*); (.||) |] =! count
