module aoc24.Day25

let parseSchematic (input: string array) =
    let isKey = input |> Seq.head |> Seq.head |> (=) '.'

    input
    |> Seq.transpose
    |> Seq.map (Seq.filter (fun c -> c = '#') >> Seq.length >> (+) -1)
    |> Seq.toList
    |> (fun x -> isKey, x)

let parse (input: string array) =
    input |> Array.chunkBySize 8 |> Array.map parseSchematic

let part1 input =
    input
    |> parse
    |> Array.partition fst
    ||> Array.allPairs
    |> Array.map (TupleEx.map snd)
    |> Array.filter (TupleEx.apply <| List.forall2 (fun a b -> a + b <= 5))
    |> Array.length

let part2 _ = "merry christmas"

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "#####"
           ".####"
           ".####"
           ".####"
           ".#.#."
           ".#..."
           "....."
           ""
           "#####"
           "##.##"
           ".#.##"
           "...##"
           "...#."
           "...#."
           "....."
           ""
           "....."
           "#...."
           "#...."
           "#...#"
           "#.#.#"
           "#.###"
           "#####"
           ""
           "....."
           "....."
           "#.#.."
           "###.."
           "###.#"
           "###.#"
           "#####"
           ""
           "....."
           "....."
           "....."
           "#...."
           "#.#.."
           "#.#.#"
           "#####" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 3

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! "merry christmas"

    [<Fact>]
    let ``parse example`` () =
        parse example
        =! [| false, [ 0; 5; 3; 4; 3 ]
              false, [ 1; 2; 0; 5; 3 ]
              true, [ 5; 0; 2; 1; 3 ]
              true, [ 4; 3; 4; 0; 2 ]
              true, [ 3; 0; 2; 0; 1 ] |]
