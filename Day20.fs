module aoc24.Day20

open FSharp.HashCollections

type Point = (struct (int * int))

module Point =
    let inline op f ((a, b): Point) ((c, d): Point) = Point(f a c, f b d)
    let inline manhattan ((x1, y1): Point) ((x2, y2): Point) = abs (x1 - x2) + abs (y1 - y2)

let inline (.+) a b = Point.op (+) a b
let inline (.-) a b = Point.op (-) a b

module Grid =
    let inline at (struct (x, y)) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x]
        else
            '#'

    let asSeq (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    Point(x, y), grid[y][x]
        }

    let findStart grid =
        grid |> asSeq |> Seq.find (fun (_, value) -> value = 'S') |> fst

    let directions = [| Point(1, 0); Point(-1, 0); Point(0, 1); Point(0, -1) |]

    let adjacent p =
        directions |> Array.map (fun d -> p .+ d)

    let adjacentOnTrack p grid =
        adjacent p
        |> Array.choose (fun p ->
            match grid |> at p with
            | '#' -> None
            | _ -> Some p)

    let race grid =
        let start = grid |> findStart

        (start, Some start)
        |> Seq.unfold (function
            | prev, Some this ->
                let next = grid |> adjacentOnTrack this |> Array.tryFind (fun n -> n <> prev)
                Some(this, (this, next))
            | _ -> None)

let findShortcuts2 grid =
    let map =
        grid
        |> Grid.race
        |> Seq.indexed
        |> Seq.map (TupleEx.swap >> TupleEx.toKeyValue)
        |> HashMap.ofSeq

    let walls =
        grid |> Grid.asSeq |> Seq.filter (snd >> (=) '#') |> Seq.map fst |> Seq.toArray

    let horizontal = Point(1, 0)
    let vertical = Point(0, 1)

    let shortcutCandidates =
        walls
        |> Array.collect (fun wall ->
            [| (wall .- horizontal, wall .+ horizontal)
               (wall .- vertical, wall .+ vertical) |])

    shortcutCandidates
    |> Array.choose (fun ps ->
        ps
        |> TupleEx.map (fun p -> map |> HashMap.tryFind p)
        |> function
            | ValueSome a, ValueSome b -> Some(ps, (a - b |> abs) - 2)
            | _ -> None)

let findShortcuts20 minSavings grid =

    let rec findShortcuts shortcuts =
        function
        | [] -> shortcuts
        | p when p |> List.length < minSavings + 1 -> shortcuts
        | start :: remainingPath ->

            let possibleEnds = remainingPath |> List.indexed |> List.skip minSavings

            let nextShortcuts =
                possibleEnds |> List.filter (fun (offset, pEnd) ->
                    let distance = Point.manhattan start pEnd
                    distance <= 20 && (offset + 2 - distance) > minSavings  )

            findShortcuts (nextShortcuts @ shortcuts) remainingPath

    grid |> Grid.race |> List.ofSeq |> findShortcuts []

let part1 input =
    let shortcuts = findShortcuts2 input
    shortcuts |> Array.filter (fun (_, length) -> length >= 100) |> Array.length

let part2 input =
    let shortcuts = findShortcuts20 100 input
    shortcuts |> List.length


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "###############"
           "#...#...#.....#"
           "#.#.#.#.#.###.#"
           "#S#...#.#.#...#"
           "#######.#.#.###"
           "#######.#.#...#"
           "#######.#.###.#"
           "###..E#...#...#"
           "###.#######.###"
           "#...###...#...#"
           "#.#####.#.###.#"
           "#.#...#.#.#...#"
           "#.#.#.#.#.#.###"
           "#...#...#...###"
           "###############" |]

    [<Fact>]
    let ``Part 1 example`` () =
        let shortcuts = findShortcuts2 example

        shortcuts |> Array.map snd |> Array.countBy id |> Array.sortBy fst
        =! [| (2, 14) //There are 14 cheats that save 2 picoseconds.
              (4, 14) //There are 14 cheats that save 4 picoseconds.
              (6, 2) //There are 2 cheats that save 6 picoseconds.
              (8, 4) //There are 4 cheats that save 8 picoseconds.
              (10, 2) //There are 2 cheats that save 10 picoseconds.
              (12, 3) //There are 3 cheats that save 12 picoseconds.
              (20, 1) //There is one cheat that saves 20 picoseconds.
              (36, 1) //There is one cheat that saves 36 picoseconds.
              (38, 1) //There is one cheat that saves 38 picoseconds.
              (40, 1) //There is one cheat that saves 40 picoseconds.
              (64, 1) |] //There is one cheat that saves 64 picoseconds.


    [<Fact>]
    let ``Part 2 example`` () =
        // There are 32 cheats that save 50 picoseconds.
        // There are 31 cheats that save 52 picoseconds.
        // There are 29 cheats that save 54 picoseconds.
        // There are 39 cheats that save 56 picoseconds.
        // There are 25 cheats that save 58 picoseconds.
        // There are 23 cheats that save 60 picoseconds.
        // There are 20 cheats that save 62 picoseconds.
        // There are 19 cheats that save 64 picoseconds.
        // There are 12 cheats that save 66 picoseconds.
        // There are 14 cheats that save 68 picoseconds.
        // There are 12 cheats that save 70 picoseconds.
        // There are 22 cheats that save 72 picoseconds.
        // There are 4 cheats that save 74 picoseconds.
        //There are 3 cheats that save 76 picoseconds.
        let cheatCount = 32 + 31 + 29 + 39 + 25 + 23 + 20 + 19 + 12 + 14 + 12 + 22 + 4 + 3

        findShortcuts20 50 example |> List.length =! cheatCount
