module aoc24.Day25

type Schematics =
    | Key of int list
    | Lock of int list

let parseSchematic (input: string array) =
    input
    |> Seq.transpose
    |> Seq.map (Seq.filter ((=) '#') >> Seq.length >> (+) -1)
    |> Seq.toList
    |> (match input[0][0] with
        | '.' -> Key
        | _ -> Lock)

let parse (input: string array) =
    input |> Array.chunkBySize 8 |> Array.map parseSchematic

let part1 input =
    parse input
    |> Array.partition _.IsKey
    ||> Array.allPairs
    |> Array.map (TupleEx.map (fun (Key a | Lock a) -> a))
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
        =! [| Lock [ 0; 5; 3; 4; 3 ]
              Lock [ 1; 2; 0; 5; 3 ]
              Key [ 5; 0; 2; 1; 3 ]
              Key [ 4; 3; 4; 0; 2 ]
              Key [ 3; 0; 2; 0; 1 ] |]
