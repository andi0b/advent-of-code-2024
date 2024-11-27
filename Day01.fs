module aoc24.Day01

// This is still 2023/Day 1

open System

module CalibrationValue =

    let fromDigits (line: string) =
        let chars = line.ToCharArray()
        let firstDigit = chars |> Array.find Char.IsDigit
        let lastDigit = chars |> Array.findBack Char.IsDigit
        $"{firstDigit}{lastDigit}" |> int

    let words =
        [ ("one", 1)
          ("two", 2)
          ("three", 3)
          ("four", 4)
          ("five", 5)
          ("six", 6)
          ("seven", 7)
          ("eight", 8)
          ("nine", 9)
          ("1", 1)
          ("2", 2)
          ("3", 3)
          ("4", 4)
          ("5", 5)
          ("6", 6)
          ("7", 7)
          ("8", 8)
          ("9", 9) ]

    type SearchMode =
        | First
        | Last

    let findValue searchMode line =

        let indexOfFunc, minOrMaxBy =
            match searchMode with
            | First -> (StringEx.indexOf, List.minBy)
            | Last -> (StringEx.lastIndexOf, List.maxBy)

        let findValueWithIndex (searchTerm, value) =
            line
            |> indexOfFunc searchTerm
            |> Option.map (fun index -> {| index = index; value = value |})

        (words |> List.choose findValueWithIndex |> minOrMaxBy _.index).value

    let fromWordsAndDigits (line: string) =
        let first = line |> findValue First
        let last = line |> findValue Last
        first * 10 + last

let part1 = Seq.map CalibrationValue.fromDigits >> Seq.sum
let part2 = Seq.map CalibrationValue.fromWordsAndDigits >> Seq.sum
let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 = null

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! -1

    let example2 = null

    [<Fact>]
    let ``Part 2 example`` () = part2 example2 =! -1

