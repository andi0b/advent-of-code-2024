module aoc24.Day18

open FSharp.HashCollections

type Point = (struct (int * int))

module Point =
    let inline op f ((a, b): Point) ((c, d): Point) = Point(f a c, f b d)

    let inline (.+) a b = op (+) a b

    let directions = [| Point(1, 0); Point(-1, 0); Point(0, 1); Point(0, -1) |]

    let adjacent p =
        directions |> Array.map (fun d -> p .+ d)

let solve gridSize byteCount input =
    let corrupted =
        input
        |> Array.take byteCount
        |> Array.map (fun l -> l |> StringEx.splitC ',' |> Array.map int |> (fun r -> Point(r[0], r[1])))
        |> HashSet.ofSeq

    let rec floodFill generation visited current =
        let next =
            current
            |> Seq.toArray
            |> Array.collect Point.adjacent
            |> Array.filter (fun struct (x, y) -> x >= 0 && y >= 0 && x <= vfst gridSize && y <= vsnd gridSize)
            |> HashSet.ofSeq
            |> (fun this -> HashSet.difference this visited)

        if next |> HashSet.count = 0 then
            None
        elif next |> HashSet.contains gridSize then
            Some generation
        else
            floodFill (generation + 1) (HashSet.union visited current) next

    floodFill 1 corrupted (HashSet.ofSeq [ Point(0, 0) ])

let solve2 gridSize byteCount input =
    [| (input |> Array.length) .. -1 .. byteCount |]
    |> Array.Parallel.tryFind (fun i -> (solve gridSize i input).IsSome)
    |> (fun x -> input[x.Value])

let part1 = solve (Point(70, 70)) 1024 >> Option.get
let part2 = solve2 (Point(70, 70)) 1024
let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        "5,4\n4,2\n4,5\n3,0\n2,1\n6,3\n2,4\n1,5\n0,6\n3,3\n2,6\n5,1\n1,2\n5,5\n2,5\n6,5\n1,4\n0,4\n6,4\n1,1\n6,1\n1,0\n0,5\n1,6\n2,0"
        |> StringEx.splitC '\n'

    [<Fact>]
    let ``Part 1 example`` () =
        example1 |> solve (Point(6, 6)) 12 =! Some 22


    [<Fact>]
    let ``Part 2 example`` () =
        example1 |> solve2 (Point(6, 6)) 12 =! "6,1"
