module aoc24.Day09

open Microsoft.FSharp.Core

let parse (input: string) =
    input |> Seq.map (fun c -> c - '0' |> int) |> Seq.toArray

type Memory = int voption

let unfoldDiskMap (map: int array) =
    seq {
        for i = 0 to map.Length - 1 do
            let cur = map.[i]

            for j = 1 to cur do
                match i % 2 with
                | 0 -> Memory.Some(i / 2)
                | _ -> Memory.None
    }


let part1 input =
    let unfolded =
        input
        |> parse
        |> unfoldDiskMap
        |> Seq.map (ValueOption.map int64)
        |> Seq.indexed
        |> Array.ofSeq

    use reverseEnumerator =
        unfolded
        |> Seq.rev
        |> Seq.filter (snd >> ValueOption.isSome)
        |> Seq.map (fun (a, b) -> a, b.Value)
        |> _.GetEnumerator()

    reverseEnumerator.MoveNext() |> ignore

    unfolded
    |> Seq.sumBy (fun (i, v) ->
        let ri, revFileId = reverseEnumerator.Current

        if ri >= i then
            match v with
            | ValueSome fileId -> int64 i * fileId
            | ValueNone ->
                reverseEnumerator.MoveNext() |> ignore
                int64 i * revFileId
        else
            0)


let part2 = (fun _ -> 0)

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let examples = [ "12345"; "2333133121414131402" ]

    let part1Data = [ 60; 1928 ] |> List.zip examples

    [<Theory; MemberData(nameof part1Data)>]
    let ``Part 1 examples`` input expected = part1 input =! expected

    [<Fact>]
    let ``Part 2 example`` () = part2 examples[1] =! 2858

    let parseUnfoldData =
        [ "0..111....22222"; "00...111...2...333.44.5555.6666.777.888899" ]
        |> List.zip examples

    [<Theory; MemberData(nameof parseUnfoldData)>]
    let ``Test parse and unfold`` input expected =
        let unfolded = input |> parse |> unfoldDiskMap

        let formatted =
            unfolded
            |> Seq.map (function
                | ValueSome fileId -> fileId.ToString()[0]
                | ValueNone -> '.')
            |> Array.ofSeq
            |> System.String

        formatted =! expected
