[<AutoOpen>]
module aoc24.Prelude

open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let tryRegex pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None

let (|Regex|_|) pattern input = tryRegex pattern input

let tryRegexG pattern input =
    let m = Regex.Matches(input, pattern)

    if m.Count > 0 then
        Some([ for x in m -> x.Value ])
    else
        None

let (|RegexG|_|) pattern input = tryRegexG pattern input

let tryPrefix (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let (|Prefix|_|) (p: string) (s: string) = tryPrefix p s

module TryParser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith (tryParseFunc: string -> bool * _) =
        tryParseFunc
        >> function
            | true, v -> Some v
            | false, _ -> None

    let parseDate = tryParseWith System.DateTime.TryParse
    let parseInt = tryParseWith System.Int32.TryParse
    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith System.Double.TryParse
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|) = parseDate
    let (|Int|_|) = parseInt
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble

let formatRun part1 part2 =

    let runWithSw runnable =
        let sw = Stopwatch.StartNew()
        let result = runnable ()
        sw.Stop()
        $"{result} ({sw.ElapsedMilliseconds}ms)"

    $"Part 1: {runWithSw part1}  Part 2: {runWithSw part2}"

let runReadAllLines part1 part2 fileName =
    let lines = File.ReadAllLines(fileName)
    formatRun (fun () -> lines |> part1) (fun () -> lines |> part2)

let runReadAllText part1 part2 fileName =
    let lines = File.ReadAllText(fileName)
    formatRun (fun () -> lines |> part1) (fun () -> lines |> part2)

let skipPart _ = "skipped"

module StringEx =
    let private asOption =
        function
        | x when x < 0 -> None
        | x -> Some x

    let indexOf (search: string) (source: string) = source.IndexOf(search) |> asOption
    let lastIndexOf (search: string) (source: string) = source.LastIndexOf(search) |> asOption

    /// split by string separator
    let splitS (separator: string) (source: string) = source.Split(separator)

    /// split by string array separator
    let splitSs (separator: string array) (source: string) =
        source.Split(separator, System.StringSplitOptions.None)

    /// split by character separator
    let splitC (separator: char) (source: string) = source.Split(separator)

    /// split by character array separator
    let splitCs (separator: char array) (source: string) = source.Split(separator)

    let replace (search: string) (replace: string) (source: string) = source.Replace(search, replace)

    let length (source: string) = source.Length

module SeqEx =
    let valueIndexed (seq: 'a seq) =
        seq |> Seq.mapi (fun i v -> struct (i, v))

    let fromEnumerator (enumerator: System.Collections.Generic.IEnumerator<'a>) =
        seq {
            while enumerator.MoveNext() do
                enumerator.Current
        }

module TupleEx =
    let inline map f (a, b) = (f a, f b)
    let inline map3 f (a, b, c) = (f a, f b, f c)
    let inline mapFst f (a, b) = (f a, b)
    let inline mapSnd f (a, b) = (a, f b)

    let inline swap (a, b) = (b, a)

    let toList (a, b) = [ a; b ]
    let toList3 (a, b, c) = [ a; b; c ]

    let inline apply f (a, b) = f a b
    let inline apply3 f (a, b, c) = f a b c

    let inline replicate x = (x, x)
    let inline replicate3 x = (x, x, x)

    let fromKeyValue (KeyValue(k, v)) = (k, v)

    let toKeyValue (k, v) =
        System.Collections.Generic.KeyValuePair(k, v)

    let fromList (l: 'a list) =
        match l with
        | [ a; b ] -> (a, b)
        | _ -> failwith "TupleEx.fromList: list must have exactly 2 elements"

    let fromList3 (l: 'a list) =
        match l with
        | [ a; b; c ] -> (a, b, c)
        | _ -> failwith "TupleEx.fromList3: list must have exactly 3 elements"

let inline vfst struct (a, _) = a
let inline vsnd struct (_, b) = b

module ValueTupleEx =
    let inline map f struct (a, b) = struct (f a, f b)
    let inline map3 f struct (a, b, c) = struct (f a, f b, f c)
    let inline mapFst f struct (a, b) = struct (f a, b)
    let inline mapSnd f struct (a, b) = struct (a, f b)

    let inline swap struct (a, b) = struct (b, a)

    let toList struct (a, b) = [ a; b ]
    let toList3 struct (a, b, c) = [ a; b; c ]

    let inline apply f struct (a, b) = f a b
    let inline apply3 f struct (a, b, c) = f a b c

    let inline replicate x = struct (x, x)
    let inline replicate3 x = struct (x, x, x)

    let fromKeyValue (KeyValue(k, v)) = struct (k, v)

    let toKeyValue struct (k, v) =
        System.Collections.Generic.KeyValuePair(k, v)

    let fromList (l: 'a list) =
        match l with
        | [ a; b ] -> struct (a, b)
        | _ -> failwith "TupleEx.fromList: list must have exactly 2 elements"

    let fromList3 (l: 'a list) =
        match l with
        | [ a; b; c ] -> struct (a, b, c)
        | _ -> failwith "TupleEx.fromList3: list must have exactly 3 elements"

[<AutoOpen>]
type DebugPrint =
    /// Like printf but only prints conditionally in DEBUG builds
    [<Conditional("DEBUG")>]
    static member dprintf format = printf format

    /// Like printfn but only prints conditionally in DEBUG builds
    [<Conditional("DEBUG")>]
    static member dprintfn format = printfn format