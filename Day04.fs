module aoc24.Day04

module Point =
    let op f (a, b) (c, d) = (f a c, f b d)
    let add = op (+)

    let allDirections =
        let directions = [ -1; 0; 1 ]
        List.allPairs directions directions |> List.except [ (0, 0) ]

module Grid =
    let at (x, y) (grid: string array) = grid[y][x]

    let tryAt (x, y) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            Some(grid[y][x])
        else
            None

    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    yield (x, y)
        }

    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq

let part1 input =

    let startPoints = input |> Grid.findChars 'X'

    let rec findString start direction search =
        let next = Point.add start direction

        match search with
        | chr :: tail when input |> Grid.tryAt next |> Option.contains chr -> findString next direction tail
        | [] -> Some next
        | _ -> None

    let findFromStart start search =
        Point.allDirections
        |> List.choose (fun dir -> findString start dir (search |> List.ofSeq))

    let allEnds = startPoints |> List.collect (fun p -> findFromStart p "MAS")

    allEnds.Length

let part2 input =

    let startPoints = input |> Grid.findChars 'A'

    let checkDiagonal start =
        let diagonals = [ [ (1, 1); (-1, -1) ]; [ (1, -1); (-1, 1) ] ]

        let matchingDiagonals =
            diagonals
            |> List.choose (fun diagonal ->
                let diagonalChars =
                    diagonal |> List.choose (fun p -> input |> Grid.tryAt (Point.add start p))

                match diagonalChars with
                | [ 'M'; 'S' ]
                | [ 'S'; 'M' ] -> Some diagonalChars
                | _ -> None)

        matchingDiagonals.Length = 2

    startPoints |> List.filter checkDiagonal |> List.length


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "MMMSXXMASM"
           "MSAMXMSMSA"
           "AMXSXMAAMM"
           "MSAMASMSMX"
           "XMASAMXAMM"
           "XXAMMXXAMA"
           "SMSMSASXSS"
           "SAXAMASAAA"
           "MAMMMXMMMM"
           "MXMXAXMASX" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 18

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 9
