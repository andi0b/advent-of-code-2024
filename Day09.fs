module aoc24.Day09

open Microsoft.FSharp.Core

type Memory = int voption

let unfoldDiskMap input =
    let map = input |> Seq.map (fun c -> c - '0' |> byte) |> Seq.toArray

    seq {
        for i = 0 to map.Length - 1 do
            for j = 1uy to map.[i] do
                match i % 2 with
                | 0 -> Memory.Some(i / 2)
                | _ -> Memory.None
    }

module SeqEx =
    let valueIndexed = Seq.mapi (fun i v -> struct (i, v))

let part1 input =

    let unfolded = input |> unfoldDiskMap |> SeqEx.valueIndexed |> Array.ofSeq

    use reverseEnumerator =
        unfolded
        |> Seq.rev
        |> Seq.choose (function
            | i, Memory.ValueSome v -> Some(i, v)
            | _ -> None)
        |> _.GetEnumerator()

    reverseEnumerator.MoveNext() |> ignore

    unfolded
    |> Seq.sumBy (fun struct (i, fileId) ->
        let revi, revFileId = reverseEnumerator.Current

        match fileId with
        | _ when i > revi -> 0
        | Memory.ValueSome fileId -> i * fileId
        | Memory.ValueNone ->
            reverseEnumerator.MoveNext() |> ignore
            i * revFileId
        |> int64)


let part2 = (fun _ -> 0)

let run = runReadAllText part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let examples =
        [ "12345" //
          "2333133121414131402" ]

    let part1Expected =
        [ 60 //
          1928 ]
        |> List.zip examples

    [<Theory; MemberData(nameof part1Expected)>]
    let ``Part 1 examples`` input expected = part1 input =! expected

    [<Fact>]
    let ``Part 2 example`` () = part2 examples[1] =! 2858

    let parseUnfoldData =
        [ "0..111....22222" //
          "00...111...2...333.44.5555.6666.777.888899" ]
        |> List.zip examples

    [<Theory; MemberData(nameof parseUnfoldData)>]
    let ``Test parse and unfold`` input expected =
        input
        |> unfoldDiskMap
        |> Seq.map (function
            | ValueSome fileId -> fileId.ToString()[0]
            | ValueNone -> '.')
        |> System.String.Concat
        =! expected
