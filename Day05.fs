module aoc24.Day05

module Rule =
    type T = Rule of int * int
    let create value = Rule value
    let value (Rule(a, b)) = (a, b)
    let fst (Rule(a, _)) = a
    let snd (Rule(_, b)) = b

module Pages =
    type T = Pages of int list
    let create value = Pages value
    let value (Pages p) = p
    let middle (Pages p) = p[p.Length / 2]

let parse input =
    let parseRules =
        Seq.map (StringEx.splitC '|' >> (fun p -> (int p.[0], int p.[1]) |> Rule.create))
        >> Seq.toList

    let parsePages =
        Seq.map (StringEx.splitC ',' >> Seq.map int >> List.ofSeq >> Pages.create)
        >> Seq.toList

    let splitAt = input |> Array.findIndex ((=) "")
    (parseRules input.[0 .. splitAt - 1], parsePages input.[splitAt + 1 ..])

g
let part1 input =
    let rules, pages = parse input

    let ruleMap = rules |> List.groupBy Rule.snd |> Map

    let findBrokenRules pages =
        let rec loop pNums brokenRules =
            match pNums with
            | cur :: tail ->
                let matchingRules = ruleMap |> Map.tryFind cur |> Option.defaultValue []

                let brokenMatchingRules =
                    matchingRules
                    |> List.filter (fun rule -> tail |> List.contains (rule |> Rule.fst))

                loop tail (brokenMatchingRules @ brokenRules)

            | [] -> brokenRules

        loop (pages |> Pages.value) []

    pages
    |> List.filter (fun p -> (findBrokenRules p |> List.length) = 0)
    |> List.sumBy Pages.middle


let part2 = (fun _ -> 0)

let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "47|53"
           "97|13"
           "97|61"
           "97|47"
           "75|29"
           "61|13"
           "75|53"
           "29|13"
           "97|29"
           "53|29"
           "61|53"
           "97|53"
           "61|29"
           "47|13"
           "75|47"
           "97|75"
           "47|61"
           "75|61"
           "47|29"
           "75|13"
           "53|13"
           ""
           "75,47,61,53,29"
           "97,61,53,29,13"
           "75,29,13"
           "75,97,47,61,53"
           "61,13,29"
           "97,13,75,29,47" |]

    [<Fact>]
    let ``Parse example`` () =
        let rules, pages = parse example1
        Seq.head rules =! Rule.create (47, 53)
        Seq.last rules =! Rule.create (53, 13)
        Seq.head pages =! Pages.create [ 75; 47; 61; 53; 29 ]
        Seq.last pages =! Pages.create [ 97; 13; 75; 29; 47 ]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 143

    let example2 = null

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! 123
