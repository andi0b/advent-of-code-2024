module aoc24.Day06

open System.Collections.Generic

type Point = (struct (int * int))

[<Struct>]
type Cursor = { position: Point; direction: Point }

module Point =
    let op f ((a, b): Point) ((c, d): Point) = struct (f a c, f b d)

let (.+) = Point.op (+)
let (.-) = Point.op (-)

module Grid =
    let at struct (x, y) (grid: string array) = grid[y][x]

    let tryAt (struct (x, y)) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            ValueSome(grid[y][x])
        else
            ValueNone

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

    let turn offset cursor =
        let index = directions |> List.findIndex (fun i -> i = cursor.direction)

        { cursor with
            direction = directions |> List.item ((index + offset) % 4) }

    let turnRight = turn 1

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

let part1 input =
    let cursor = getStartCursor input

    let path =
        cursor
        |> List.unfold (fun state ->
            match input |> Grid.tryAt state.position with
            | ValueSome('^' | '.') -> Some(state, state |> Cursor.walk)
            | ValueSome '#' ->
                let backtracked = state |> Cursor.walkBack
                Some(backtracked, backtracked |> Cursor.turnRight)
            | ValueNone -> None
            | ValueSome c -> failwith $"unexpected input: {c}")

    path |> List.distinctBy _.position |> List.length


type SimulationResult =
    | Loop
    | Exit

let part2 input =
    let cursor = getStartCursor input

    let obstructionPossibilities =
        input
        |> Grid.allPos
        |> Seq.filter (fun pos ->
            pos <> cursor.position
            && (input |> Grid.tryAt pos |> ValueOption.defaultValue '#') <> '#')
        |> Seq.toList

    let simulate addedObstruction =
        let visited = HashSet<Cursor>()

        let rec loop state =
            if visited.Add state then
                let atPosition =
                    if state.position = addedObstruction then
                        ValueSome '#'
                    else
                        input |> Grid.tryAt state.position

                match atPosition with
                | ValueSome('^' | '.') -> loop (state |> Cursor.walk)
                | ValueSome '#' -> loop (state |> Cursor.walkBack |> Cursor.turnRight)
                | ValueNone -> Exit
                | ValueSome c -> failwith $"unexpected input: {c}"
            else
                Loop

        loop cursor

    let simulationResults =
        obstructionPossibilities |> List.toArray |> Array.Parallel.map simulate

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

    let example2 = null

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 6
