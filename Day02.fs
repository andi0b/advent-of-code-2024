module aoc24.Day02

module Report =
    let parse = StringEx.splitC ' ' >> Array.map int >> List.ofArray

    let isSafe report =
        let changes = report |> List.pairwise |> List.map (TupleEx.apply (-))

        let test lower upper =
            changes |> List.forall (fun c -> c >= lower && c <= upper)

        test 1 3 || test -3 -1

    let isSafeWithDamper report =
        let permutations =
            [ 0 .. (report |> List.length) - 1 ]
            |> List.map (fun i -> report |> List.removeAt i)

        permutations |> List.exists isSafe

let solve chooser =
    Array.map Report.parse >> Seq.filter chooser >> Seq.length

let part1 = solve Report.isSafe

let part2 = solve Report.isSafeWithDamper

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "7 6 4 2 1" //
           "1 2 7 8 9"
           "9 7 6 2 1"
           "1 3 2 4 5"
           "8 6 4 4 1"
           "1 3 6 7 9" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 2

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 4
