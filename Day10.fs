module aoc24.Day10

type Point = (struct (int * int))

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
                    yield Point(x, y)
        }

    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq


let solve tailMapper input =
    let trailHeads = input |> Grid.findChars '0'
    let directions = [ Point(0, -1); Point(-1, 0); Point(0, 1); Point(1, 0) ]

    let scoreTrail head =
        let rec findTrailTails curHeight pos =
            match curHeight with
            | '9' -> [pos]
            | _ ->
                let nextHeight = curHeight + char 1

                directions
                |> List.map ((.+) pos)
                |> List.filter (fun p -> (input |> Grid.tryAt p) = nextHeight)
                |> function
                    | [] -> []
                    | l -> l |> List.collect (findTrailTails nextHeight)

        findTrailTails '0' head |> tailMapper |> Seq.length

    trailHeads |> List.sumBy scoreTrail


let part1 = solve Seq.distinct
let part2 = solve id

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "89010123"
           "78121874"
           "87430965"
           "96549874"
           "45678903"
           "32019012"
           "01329801"
           "10456732" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 36

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 81
