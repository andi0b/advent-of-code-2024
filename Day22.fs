module aoc24.Day22

open System
open System.Runtime.InteropServices

let inline nextSecretNumberFast x0 =
    let inline mixPrune a b = a ^^^ b &&& 0xFFFFFFL
    let x1 = (x0 <<< 6) |> mixPrune x0
    let x2 = (x1 >>> 5) |> mixPrune x1
    (x2 <<< 11) |> mixPrune x2

let part1 input =
    let secretNumberAt pos x =
        let mutable x' = x

        for i = 1 to pos do
            x' <- nextSecretNumberFast x'

        x'

    input |> Array.Parallel.sumBy (int64 >> secretNumberAt 2000)

let part2 input =

    let generate2000SecretNumbers start =
        let array = GC.AllocateUninitializedArray(2000)
        array[0] <- start

        for i in [ 1..1999 ] do
            array[i] <- nextSecretNumberFast array[i - 1]

        array

    let buyerPrices =
        input
        |> Array.Parallel.map (fun x -> generate2000SecretNumbers (int64 x) |> Array.map (fun y -> y % 10L |> int8))

    let buyerDifferences =
        buyerPrices
        |> Array.Parallel.map (fun x -> x |> Array.pairwise |> Array.map (fun (a, b) -> b - a))

    let allPatterns =
        [| let span = [ 0y .. 9y ]

           for a in span do
               for b in span do
                   for c in span do
                       for d in span do
                           for e in span do
                               let array = [| b - a; c - b; d - c; e - d |]
                               let span = Span<int8>(array)
                               let x = MemoryMarshal.Cast<int8, uint>(span)
                               x[0]

           |]

    allPatterns
    |> Array.Parallel.map (fun pattern ->
        buyerDifferences
        |> Array.mapi (fun i buyer ->
            let mutable minIndex = ValueNone

            for offset in [ 0..3 ] do
                let span =
                    match minIndex with
                    | ValueSome min -> buyer.AsSpan(offset, min - offset)
                    | _ -> buyer.AsSpan(offset)

                let cast = MemoryMarshal.Cast<int8, uint>(span)
                let foundIndex = MemoryExtensions.IndexOf(cast, pattern)

                if foundIndex <> -1 then
                    let updatedIndex = foundIndex * 4 + offset + 4

                    if minIndex.IsNone then
                        minIndex <- ValueSome updatedIndex
                    else
                        minIndex <- ValueSome(min minIndex.Value updatedIndex)

            match minIndex with
            | ValueSome idx when idx <= 1999 -> buyerPrices[i][idx] |> int
            | _ -> 0)

        |> Array.sum)
    |> Array.max



let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 = [| "1"; "10"; "100"; "2024" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 37327623L


    let example2 = [| "1"; "2"; "3"; "2024" |]

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! 23
