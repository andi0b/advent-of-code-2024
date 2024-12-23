module aoc24.Day21

module Grid =
    let at (x, y) (grid: string array) =
        if y >= 0 && x >= 0 && y < grid.Length && x < grid[y].Length then
            grid[y][x]
        else
            ' '

    let allPos (grid: string array) =
        seq {
            for y = 0 to grid.Length - 1 do
                for x = 0 to grid[y].Length - 1 do
                    yield (x, y)
        }

    let findChar chr grid =
        grid |> allPos |> Seq.find (fun p -> grid |> at p = chr)

    let findPath start end' grid =
        let inline (.-) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
        let startP = findChar start grid
        let endP = findChar end' grid
        let vecX, vecY = endP .- startP

        let leftRight =
            if vecX > 0 then
                Seq.replicate vecX '>'
            else
                Seq.replicate (-vecX) '<'

        let upDown =
            if vecY > 0 then
                Seq.replicate vecY 'v'
            else
                Seq.replicate (-vecY) '^'

        let canGoXFirst = grid |> at (fst startP + vecX, snd startP) <> ' '
        let canGoYFirst = grid |> at (fst startP, snd startP + vecY) <> ' '

        // try to go left first if possible, otherwise up/down then right
        // sometimes we need to chose a specific route, do that too
        [| if (vecX < 0 && canGoXFirst) || not canGoYFirst then
               yield! leftRight
               yield! upDown
           else
               yield! upDown
               yield! leftRight

           yield 'A' |]
        |> System.String

    let createPathMap grid =
        let allChars =
            grid |> allPos |> Seq.map (fun p -> grid |> at p) |> Seq.filter ((<>) ' ')

        Seq.allPairs allChars allChars
        |> Seq.map (fun (a, b) -> struct (a, b), if (a = b) then "A" else grid |> findPath a b)
        |> Map.ofSeq

module Keypad =
    let numericKeypad =
        [| "789" //
           "456"
           "123"
           " 0A" |]
        |> Grid.createPathMap

    let directionalKeypad =
        [| " ^A" //
           "<v>" |]
        |> Grid.createPathMap

    let expand keypad (input: char seq) =
        Seq.append (Seq.singleton 'A') input
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> keypad |> Map.find struct (a, b))
        |> System.String.Concat

    let buildExpanderChain numKeypads =
        (expand numericKeypad, [ 1..numKeypads ])
        ||> List.fold (fun acc _ -> acc >> (expand directionalKeypad))

    let calculateCost keypad nextGenCalculator (input: string) =
        Seq.append (Seq.singleton 'A') input
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> keypad |> Map.find struct (a, b) |> nextGenCalculator)
        |> Seq.sum

    let buildCostChain numKeypads =
        (String.length >> int64, [ 1..numKeypads ])
        ||> List.fold (fun inner _ -> calculateCost directionalKeypad inner |> memorize)
        |> calculateCost numericKeypad

let complexity (input: string) length = length * (input.TrimEnd('A') |> int64)

let part1 input =
    let expander = Keypad.buildExpanderChain 2
    input |> Array.Parallel.sumBy (fun i -> complexity i (expander i).Length)

let part2 input =
    let costChain = Keypad.buildCostChain 25
    input |> Array.sumBy (fun i -> complexity i (costChain i))

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example = [| "029A"; "980A"; "179A"; "456A"; "379A" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 126384

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 154115708116294L

    [<Fact>]
    let ``pathMapExamples`` () =

        printfn "map sizes:"
        printfn $"directional: {Keypad.directionalKeypad.Count}"
        printfn $"numeric: {Keypad.numericKeypad.Count}"

        [ struct ('A', '<'); struct ('<', '>'); struct ('<', '^'); struct ('>', '^') ]
        |> List.iter (fun k -> printfn $"{vfst k} -> {vsnd k}: {Keypad.directionalKeypad |> Map.find k}")


        [ struct ('A', '1'); struct ('0', '7'); struct ('1', 'A'); struct ('1', '9') ]
        |> List.iter (fun k -> printfn $"{vfst k} -> {vsnd k}: {Keypad.numericKeypad |> Map.find k}")

        printfn ""
        printfn $"example robot 1:"

        [ ('A', '0'); ('0', '2'); ('2', '9'); ('9', 'A') ]
        |> List.iter (fun (a, b) -> printfn $"{a} -> {b}: {Keypad.numericKeypad |> Map.find struct (a, b)}")

        printfn ""
        printfn $"example robot 2:"

        "A<A^A>^^AvvvA"
        |> Seq.pairwise
        |> Seq.iter (fun (a, b) -> printfn $"{a} -> {b}: {Keypad.directionalKeypad |> Map.find struct (a, b)}")
