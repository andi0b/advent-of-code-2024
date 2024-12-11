module aoc24.Day11

let parse = StringEx.splitC ' ' >> Array.map int64

let solve blinks input =
    let initial = input |> parse |> Array.toList

    (initial, seq { 1..blinks })
    ||> Seq.fold (fun list _ ->
        ([], list)
        ||> List.fold (fun results next ->
            let digits = 1 + (next |> float |> log10 |> int)

            match next with
            | 0L -> 1L :: results
            | n when System.Int64.IsEvenInteger(digits) ->
                let factor = int digits / 2 |> pown 10L
                let left = n / factor
                let right = n - left * factor
                left :: right :: results
            | n -> (n * 2024L) :: results))
    |> List.length

let solveFast blinks input =
    // Dictionary of (remaining blinks * input number) -> solution
    let memorizedSolutions =
        System.Collections.Generic.Dictionary<struct (int * int64), int64>()

    let rec deep remainingBlinks num =
        let key = struct (remainingBlinks, num)

        match memorizedSolutions.TryGetValue(key) with
        | true, solution -> solution
        | false, _ ->
            let digits = 1 + (num |> float |> log10 |> int)

            let deepn =
                match remainingBlinks with
                | 1 -> (fun _ -> 1L)
                | _ -> deep (remainingBlinks - 1)

            let solution =
                match num with
                | 0L -> deepn 1L
                | n when System.Int64.IsEvenInteger(digits) ->
                    let factor = int digits / 2 |> pown 10L
                    let left = n / factor
                    let right = n - left * factor
                    (deepn left) + (deepn right)
                | n -> deepn (n * 2024L)

            memorizedSolutions[key] <- solution
            solution

    input |> parse |> Array.sumBy (deep blinks)

let part1 = solve 25
let part2 = solveFast 75
let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example = "125 17"

    [<Fact>]
    let ``Part 1 solve`` () = solve 25 example =! 55312

    [<Fact>]
    let ``Part 1 solveFast`` () = solveFast 25 example =! 55312
