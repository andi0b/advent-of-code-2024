open System
open System.IO
open System.Net.Http
open System.Reflection
open System.Threading.Tasks
open FSharp.Text.RegexProvider
open aoc24

let year = "2024"
let inputDirectory = "inputs"
let inputPath day =
    Path.Combine(inputDirectory, $"day%02u{day}.txt")

let downloadAllDays maxDay =

    let dotEnv path =
        if File.Exists path then
            let vars =
                File.ReadAllLines(path)
                |> Array.map (StringEx.splitC '=')
                |> Array.choose (function
                    | [| key; value |] -> Some(key, value)
                    | _ -> None)

            for key, value in vars do
                Environment.SetEnvironmentVariable(key, value)

    dotEnv ".env"
    Directory.CreateDirectory(inputDirectory) |> ignore

    let httpClient =
        let handler = new HttpClientHandler(UseCookies = false)
        let client = new HttpClient(handler)
        let sessionKey = Environment.GetEnvironmentVariable("AOC_SESSION")
        client.DefaultRequestHeaders.Add("Cookie", $"session={sessionKey}")
        client

    let downloadInput day =
        printf $"Downloading input for Day {day}... "

        task {
            let! response = httpClient.GetAsync($"https://adventofcode.com/{year}/day/{day}/input")
            response.EnsureSuccessStatusCode |> ignore
            use stream = response.Content.ReadAsStream()
            use fileStream = File.OpenWrite(inputPath day)
            stream.CopyTo(fileStream)
            printfn "done"
        }

    task {
        if Environment.GetEnvironmentVariable("AOC_SESSION") |> String.IsNullOrWhiteSpace then
            printf "Skipping input download, because AOC_SESSION is unset"
        else
            for day = 1 to maxDay do
                if not (File.Exists(inputPath day)) then
                    do! downloadInput day
    }

type DayRegex = Regex< @"Day(?<num>\d+)" >

let days =

    let dayModules =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter (fun t ->
            t.IsClass
            && t.IsAbstract
            && t.IsSealed
            && t.IsPublic
            && (t.DeclaringType = null)) // Ensure it's a top-level module
        |> Array.choose (fun t ->
            DayRegex().TryTypedMatch(t.Name)
            |> Option.map (fun m -> (m.num.Value |> int, t)))
        |> Map

    let createDay i =
        dayModules
        |> Map.tryFind i
        |> Option.map (fun t ->

            let propertyMethodInfo =
                t.GetMethod("get_run", BindingFlags.Public ||| BindingFlags.Static)

            let functionMethodInfo =
                t.GetMethod("run", BindingFlags.Public ||| BindingFlags.Static)

            if (propertyMethodInfo = null && functionMethodInfo = null) then
                failwith $"Type {t.Name} doesn't have a method 'run'"

            if (propertyMethodInfo <> null) then
                (fun () ->
                    let run = propertyMethodInfo.Invoke(null, [||]) :?> (string -> string)
                    run (inputPath i))
            else
                (fun () -> functionMethodInfo.Invoke(null, [| inputPath i |]) :?> string))

        |> Option.defaultValue (fun () -> "skipped")

    let maxDay = dayModules |> Map.keys |> Seq.max
    [ 1..maxDay ] |> List.map createDay

let runAll () =

    let tasks =
        days
        |> List.indexed
        |> List.rev
        |> List.map (fun (i, r) ->
            task {
                let! result = Task.Run r
                return (i, result)
            })

    task {
        for task in tasks do
            let! i, result = task
            printfn $"Day {i + 1} {result}"
    }
    |> Task.WaitAll

open TryParser

[<EntryPoint>]
let Main args =

    (downloadAllDays days.Length).Wait()

    match args with
    | [| Int day |] when day > 0 && day <= 25 ->

        match days |> List.tryItem (day - 1) with
        | Some implementation ->
            printfn $"Running day {day}:"
            printfn $"{implementation ()}"
            0

        | None ->
            printfn $"Could not find an implementation for day {day}"
            1

    | [| "latest" |] when days.Length > 0 ->
        printfn $"Running latest (day {days.Length}):"
        printf $"{(days |> List.last) ()}"
        0

    | [||] ->
        printfn $"Running all days (1 - {days.Length})"
        runAll ()
        0

    | _ ->
        printf "Expect either a number, 'latest', no parameters"
        1
