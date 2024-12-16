module aoc24.Day15

type Point = (struct (float * float))
let inline pOp f ((a, b): Point) ((c, d): Point) = Point(f a c, f b d)
let inline (.+) a b = pOp (+) a b
let inline (.-) a b = pOp (-) a b
let inline (.*) a b = pOp (*) a b
let inline (..*) ((x, y): Point) i = Point(x * i, y * i)
let inline pfloor ((x, y): Point) = Point(floor x, y)

type Grid =
    { robot: Point
      boxes: Point Set
      walls: Point Set }

module Grid =
    let create (input: string array) =
        let find c =
            seq {
                for y = 0 to input.Length - 1 do
                    let l = input[y]

                    for x = 0 to l.Length - 1 do
                        if l[x] = c then
                            yield Point(x, y)
            }

        { robot = find '@' |> Seq.exactlyOne
          boxes = find 'O' |> Set
          walls = find '#' |> Set }

    let boxScore { boxes = boxes } =
        boxes |> Seq.sumBy (fun struct (x, y) -> x + y * 100.0)

    let boxScoreScaled { boxes = boxes } =
        boxes |> Seq.sumBy (fun struct (x, y) -> 2.0 * x + y * 100.0)

    let directionVector =
        function
        | '>' -> Point(1, 0)
        | '<' -> Point(-1, 0)
        | '^' -> Point(0, -1)
        | 'v' -> Point(0, 1)
        | c -> failwith $"unknown direction: {c}"

    let move direction grid =
        let vec = directionVector direction
        let nextPos = grid.robot .+ vec

        let (|Wall|Box|Empty|) p =
            if grid.walls |> Set.contains p then Wall
            elif grid.boxes |> Set.contains p then Box
            else Empty

        let rec findFreeSpace p =
            let np = p .+ vec

            match np with
            | Wall -> ValueNone
            | Box -> findFreeSpace np
            | Empty -> ValueSome np

        match nextPos with
        | Wall -> grid
        | Empty -> { grid with robot = nextPos }
        | Box ->
            match findFreeSpace nextPos with
            | ValueNone -> // box can't be pushed, do nothing
                grid
            | ValueSome fs -> // push box and move
                { grid with
                    robot = nextPos
                    boxes = grid.boxes |> Set.remove nextPos |> Set.add fs }

    let scaleFactor = Point(0.5, 1)
    let onex = Point(0.5, 0)

    let moveScaled direction grid =
        let vec = directionVector direction .* scaleFactor
        let nextPos = grid.robot .+ vec

        let (|Wall|Box|Empty|) p =
            if grid.walls |> Set.contains (pfloor p) then
                Wall
            elif grid.boxes |> Set.contains p then
                Box p
            elif grid.boxes |> Set.contains (p .- onex) then
                Box(p .- onex)
            else
                Empty

        let rec findMovableBoxes results boxes =
            let adjacent =
                boxes
                |> List.collect (fun b ->
                    match direction with
                    | '>' -> [ b .+ vec .+ vec ] // 2 steps to the right, to skip self
                    | '<' -> [ b .+ vec ] // 1 step to the left
                    | _ -> // above/below of self and 1 step to the right
                        [ b .+ vec //
                          b .+ vec .+ onex ])

            let hasAdjWalls, adjBoxes =
                ((false, []), adjacent)
                ||> List.fold (fun (walls, boxes) next ->
                    match next with
                    | Wall -> (true, boxes)
                    | Box b -> (walls, b :: boxes)
                    | Empty -> (walls, boxes))

            if hasAdjWalls then
                ValueNone
            else
                match adjBoxes with
                | [] -> ValueSome(boxes @ results)
                | _ -> findMovableBoxes (boxes @ results) (adjBoxes |> List.distinct)

        match nextPos with
        | Wall -> grid
        | Empty -> { grid with robot = nextPos }
        | Box b ->
            match findMovableBoxes [] [ b ] with
            | ValueNone -> // b can't be pushed, do nothing
                grid
            | ValueSome boxes -> // push all connected boxes and move
                let movedBoxes = boxes |> List.map (fun box -> box .+ vec) |> Set

                { grid with
                    robot = nextPos
                    boxes = Set.difference grid.boxes (Set boxes) |> Set.union movedBoxes }

let parse lines =
    let emptyLineIndex = lines |> Array.findIndex (fun l -> l = "")
    let box, instructions = lines |> Array.splitAt emptyLineIndex

    Grid.create box, instructions |> Array.collect _.ToCharArray()

let solve mover scorer input =
    parse input ||> Array.fold (fun g i -> mover i g) |> scorer

let part1 = solve Grid.move Grid.boxScore
let part2 = solve Grid.moveScaled Grid.boxScoreScaled
let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let smallerExample =
        [| "########"
           "#..O.O.#"
           "##@.O..#"
           "#...O..#"
           "#.#.O..#"
           "#...O..#"
           "#......#"
           "########"
           ""
           "<^^>>>vv<v>>v<<" |]

    let example =
        [| "##########"
           "#..O..O.O#"
           "#......O.#"
           "#.OO..O.O#"
           "#..O@..O.#"
           "#O#..O...#"
           "#O..O..O.#"
           "#.OO.O.OO#"
           "#....O...#"
           "##########"
           ""
           "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
           "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
           "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
           "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
           "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
           "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
           ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
           "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
           "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
           "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 10092

    [<Fact>]
    let ``Part 1 smaller example`` () = part1 smallerExample =! 2028

    [<Fact>]
    let ``parse example`` () =
        let grid, instructions = parse example

        grid.robot =! Point(4, 4)

        grid.walls |> Seq.contains (Point(0, 0)) =! true
        grid.walls |> Seq.contains (Point(9, 9)) =! true
        grid.boxes |> Seq.contains (Point(5, 8)) =! true

        instructions |> Array.take 4 =! [| '<'; 'v'; 'v'; '>' |]
        instructions |> Array.rev |> Array.take 4 =! [| '^'; '<'; '<'; '^' |]


    let example2 = null

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 9021

    let anotherSmallerExample =
        [| "#######"
           "#...#.#"
           "#.....#"
           "#..OO@#"
           "#..O..#"
           "#.....#"
           "#######"
           ""
           "<vv<<^^<<^^" |]

    [<Fact>]
    let ``Part 2 another smaller example`` () = part2 anotherSmallerExample =! 618
