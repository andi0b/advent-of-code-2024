module aoc24.Day05

module Pages =
    let middle (p: int list) = p[p.Length / 2]

let parse input =
    let parseRules =
        Seq.map (StringEx.splitC '|' >> (fun p -> (int p.[0], int p.[1]))) >> Seq.toList

    let parsePages =
        Seq.map (StringEx.splitC ',' >> Seq.map int >> Seq.toList) >> Seq.toList

    let splitAt = input |> Array.findIndex ((=) "")
    (parseRules input.[0 .. splitAt - 1], parsePages input.[splitAt + 1 ..])

let part1 input =

    let rules, pages = parse input

    let ruleMap = rules |> List.groupBy snd |> Map

    let findBrokenRules pages =
        let rec loop pNums brokenRules =
            match pNums with
            | cur :: tail ->
                let matchingRules = ruleMap |> Map.tryFind cur |> Option.defaultValue []

                let brokenMatchingRules =
                    matchingRules |> List.filter (fun rule -> tail |> List.contains (rule |> fst))

                loop tail (brokenMatchingRules @ brokenRules)

            | [] -> brokenRules

        loop pages []

    pages
    |> List.filter (fun p -> (findBrokenRules p |> List.length) = 0)
    |> List.sumBy Pages.middle


let part2 input =
    let rules, pagesList = parse input

    let rulesSet = rules |> Set

    let comparer a b =
        if rulesSet |> Set.contains (a, b) then -1
        elif rulesSet |> Set.contains (b, a) then 1
        else 0

    let sortedPagesList = pagesList |> List.map (List.sortWith comparer)

    let correctedPagesList =
        List.zip pagesList sortedPagesList
        |> List.choose (function
            | original, sorted when original <> sorted -> Some sorted
            | _ -> None)

    correctedPagesList |> List.sumBy Pages.middle


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example =
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
        let rules, pages = parse example
        Seq.head rules =! (47, 53)
        Seq.last rules =! (53, 13)
        Seq.head pages =! [ 75; 47; 61; 53; 29 ]
        Seq.last pages =! [ 97; 13; 75; 29; 47 ]

    [<Fact>]
    let ``Part 1 example`` () = part1 example =! 143

    [<Fact>]
    let ``Part 2 example`` () = part2 example =! 123
