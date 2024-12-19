module aoc24.Day12

type Point = (struct (int * int))

module Point =
    let inline add ((a, b): Point) ((c, d): Point) = struct (a + c, b + d)

module Grid =
    let inline at (struct (x, y)) (grid: string array) =
        if y >= 0 && y < grid.Length && x >= 0 && x < grid[y].Length then
            grid[y][x]
        else
            ' '

    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    Point(x, y)
        }

    let findChars chr grid =
        grid |> allPos |> Seq.filter (fun p -> grid |> at p = chr) |> List.ofSeq


let directions = [ Point(0, -1); Point(-1, 0); Point(0, 1); Point(1, 0) ]

let findRegions input =

    let points = input |> Grid.allPos |> Set

    let rec findNeighbours results point =
        let plant = input |> Grid.at point

        let neighbours =
            directions
            |> Seq.map (Point.add point)
            |> Seq.filter (fun x -> input |> Grid.at x = plant && results |> Set.contains x |> not)
            |> Set

        match neighbours |> Set.isEmpty with
        | true -> results
        | false -> (results |> Set.union neighbours, neighbours) ||> Set.fold findNeighbours

    let rec findRegionsInner results points =
        match points |> Set.isEmpty with
        | true -> results
        | false ->
            let nextPoint = points |> Set.minElement
            let region = findNeighbours (Set.singleton nextPoint) nextPoint
            findRegionsInner (region :: results) (Set.difference points region)

    findRegionsInner [] points

let part1 input =
    input
    |> findRegions
    |> List.sumBy (fun r ->
        let area = r |> Set.count

        let borderPieces =
            r
            |> Seq.sumBy (fun p ->
                let plant = input |> Grid.at p

                directions
                |> Seq.map (Point.add p)
                |> Seq.filter (fun x -> input |> Grid.at x <> plant)
                |> Seq.length)

        area * borderPieces)


let part2 = (fun _ -> 0)

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
        [| "RRRRIICCFF"
           "RRRRIICCCF"
           "VVRRRCCFFF"
           "VVRCCCJFFF"
           "VVVVCJJCFE"
           "VVIVCCJJEE"
           "VVIIICJJEE"
           "MIIIIIJJEE"
           "MIIISIJEEE"
           "MMMISSJEEE" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 1930

    [<Fact(Skip = "Part 2 not implemented yet")>]
    let ``Part 2 example`` () = part2 example =! 1206
