module aoc24.Day06

open System.Collections.Generic

type Point = (struct (int * int))

[<Struct>]
type Cursor = { position: Point; direction: Point }

module Point =
    let inline op f ((a, b): Point) ((c, d): Point) = struct (f a c, f b d)

let inline (.+) a b = Point.op (+) a b
let inline (.-) a b = Point.op (-) a b

module Grid =
    let inline at struct (x, y) (grid: string array) = grid[y][x]

    let inline tryAt (struct (x, y)) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x]
        else
            ' '

    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    yield struct (x, y)
        }

    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq


module Cursor =
    let directions =
        [ struct (0, -1) // N
          struct (1, 0) // E
          struct (0, 1) // S
          struct (-1, 0) ] // W

    let turnRight cursor =
        let index = directions |> List.findIndex (fun i -> i = cursor.direction)

        { cursor with
            direction = directions |> List.item ((index + 1) % 4) }

    let walk cursor =
        { cursor with
            position = cursor.position .+ cursor.direction }

    let walkBack cursor =
        { cursor with
            position = cursor.position .- cursor.direction }

let getStartCursor input =
    let start = input |> Grid.findChars '^' |> Seq.exactlyOne

    { position = start
      direction = Cursor.directions.Head }

let walkPath cursor input =
    cursor
    |> List.unfold (fun state ->
        match input |> Grid.tryAt state.position with
        | ('^' | '.') -> Some(state, state |> Cursor.walk)
        | '#' ->
            let backtracked = state |> Cursor.walkBack
            Some(backtracked, backtracked |> Cursor.turnRight)
        | ' ' -> None
        | c -> failwith $"unexpected input: {c}")

let part1 input =
    let cursor = getStartCursor input
    let path = walkPath cursor input

    path |> List.distinctBy _.position |> List.length


type SimulationResult =
    | Loop
    | Exit

let part2 input =
    let simulate cursor addedObstruction =
        // Store only visited turns with the direction the guard is facing in. This is enough to detect a loop.
        let visitedTurns = HashSet<Cursor>()

        let rec loop state =
            let atPosition =
                if state.position = addedObstruction then
                    '#'
                else
                    input |> Grid.tryAt state.position

            if atPosition <> '#' || visitedTurns.Add state then
                match atPosition with
                | ('^' | '.') -> loop (state |> Cursor.walk)
                | '#' -> loop (state |> Cursor.walkBack |> Cursor.turnRight)
                | ' ' -> Exit
                | c -> failwith $"unexpected input: {c}"
            else
                Loop

        loop cursor

    let startCursorAddedObstructionPairs =
        walkPath (getStartCursor input) input
        |> Seq.distinctBy _.position
        |> Seq.pairwise
        |> Seq.map (fun (first, second) -> (first, second.position))
        |> Seq.toArray

    let simulationResults =
        startCursorAddedObstructionPairs
        |> Array.Parallel.map (fun (cursor, obstruction) -> simulate cursor obstruction)

    simulationResults |> Array.filter _.IsLoop |> Array.length


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "....#....."
           ".........#"
           ".........."
           "..#......."
           ".......#.."
           ".........."
           ".#..^....."
           "........#."
           "#........."
           "......#..." |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 41

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 6
