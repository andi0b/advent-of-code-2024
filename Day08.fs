module aoc24.Day08

type Point = (struct (int * int))

module Point =
    let inline op f ((a, b): Point) ((c, d): Point) = struct (f a c, f b d)

let inline (.+) a b = Point.op (+) a b
let inline (.-) a b = Point.op (-) a b

type Grid(input: string array) =
    let bx, by = (input.[0].Length, input.Length)

    member x.findAntennas =
        seq {
            for y = 0 to input.Length - 1 do
                for x = 0 to input.[y].Length - 1 do
                    match input.[y].[x] with
                    | '.' -> ()
                    | c -> yield (c, Point(x, y))
        }

    member x.groupedAntennas =
        x.findAntennas
        |> List.ofSeq
        |> List.groupBy fst
        |> List.map (snd >> List.map snd)

    member x.inBounds((x, y): Point) = x >= 0 && y >= 0 && x < bx && y < by

let allPairs1 list =
    Seq.allPairs list list |> Seq.filter (fun (a, b) -> a <> b) |> List.ofSeq

let part1 input =
    let grid = input |> Grid

    let firstGenAntiNodes (a, b) =
        let vec = b .- a
        [ b .+ vec; a .- vec ]

    let antiNodes =
        grid.groupedAntennas
        |> List.collect (allPairs1 >> List.map firstGenAntiNodes)
        |> List.collect id

    antiNodes |> List.distinct |> List.filter grid.inBounds |> List.length

let part2 input =
    let grid = input |> Grid

    let antiNodesInBounds (a, b) =
        let vec = b .- a

        let find op =
            List.unfold (function
                | p when grid.inBounds p -> Some(p, op p vec)
                | _ -> None)

        find (.+) b @ find (.-) a

    let antiNodes =
        grid.groupedAntennas
        |> List.collect (allPairs1 >> List.map antiNodesInBounds)
        |> List.collect id

    antiNodes |> List.distinct |> List.length

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "............"
           "........0..."
           ".....0......"
           ".......0...."
           "....0......."
           "......A....."
           "............"
           "............"
           "........A..."
           ".........A.."
           "............"
           "............" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 14

    [<Fact>]
    let ``Part 2 example`` () = part2 example1 =! 34
