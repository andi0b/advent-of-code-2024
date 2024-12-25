module aoc24.Day23

open System.Collections.Generic
open FSharp.HashCollections

type Helper(input) =
    let parsed =
        input
        |> Array.map (fun l -> l |> StringEx.splitC '-' |> (fun parts -> (parts[0], parts[1])))

    let _connections =
        parsed
        |> Array.collect (fun x -> [| x; TupleEx.swap x |])
        |> Array.distinct
        |> Array.groupBy fst
        |> Array.map (fun (k, v) -> KeyValuePair(k, v |> Array.map snd |> Set.ofSeq))
        |> HashMap.ofSeq

    let _nodes = _connections |> HashMap.keys |> Set.ofSeq

    member x.nodes = _nodes
    member x.connections = _connections

    member x.findConnection n =
        _connections |> HashMap.tryFind n |> ValueOption.defaultValue Set.empty


let part1 input =
    let helper = Helper input

    let threeConnected =
        helper.nodes
        |> Set.ofSeq
        |> Seq.collect (fun n ->
            helper.findConnection n
            |> TupleEx.replicate
            ||> Seq.allPairs
            |> Seq.filter (fun (a, b) ->
                if a = b then
                    false
                else
                    helper.findConnection a |> Set.contains b)
            |> Seq.map (fun (a, b) ->
                seq {
                    a
                    b
                    n
                }
                |> Set.ofSeq))
        |> Set.ofSeq

    threeConnected
    |> Set.toSeq
    |> Seq.filter (fun triples -> triples |> Set.toSeq |> Seq.exists (fun n -> n.StartsWith "t"))
    |> Seq.length

let part2 input =
    let helper = Helper input

    let tryAddConnectedNodes nodeSet =
        let candidates = nodeSet |> Seq.map helper.findConnection |> Set.unionMany

        let connectedCandidates =
            candidates
            |> Set.filter (fun n -> Set.isSubset nodeSet <| helper.findConnection n)

        //dprintfn "set length: %A, candidates: %A, connected: %A" nodeSet.Count candidates.Count connectedCandidates.Count

        connectedCandidates |> Seq.map (fun n -> nodeSet |> Set.add n)


    let rec findBiggestChunk nodeSet =
        let expanded = nodeSet |> Seq.collect tryAddConnectedNodes |> Set

        dprintfn "next gen"

        if expanded |> Set.isEmpty then
            nodeSet
        else
            findBiggestChunk expanded

    let result =
        helper.nodes |> Seq.map (Seq.singleton >> Set) |> Set |> findBiggestChunk

    result |> Seq.head |> String.concat ","


let run = runReadAllLines part1 part2

module tests =
    open Swensen.Unquote
    open Xunit

    let example1 =
        [| "kh-tc"
           "qp-kh"
           "de-cg"
           "ka-co"
           "yn-aq"
           "qp-ub"
           "cg-tb"
           "vc-aq"
           "tb-ka"
           "wh-tc"
           "yn-cg"
           "kh-ub"
           "ta-co"
           "de-co"
           "tc-td"
           "tb-wq"
           "wh-td"
           "ta-ka"
           "td-qp"
           "aq-cg"
           "wq-ub"
           "ub-vc"
           "de-ta"
           "wq-aq"
           "wq-vc"
           "wh-yn"
           "ka-de"
           "kh-ta"
           "co-tc"
           "wh-qp"
           "tb-vc"
           "td-yn" |]

    [<Fact>]
    let ``Part 1 example`` () = part1 example1 =! 7

    [<Fact>]
    let ``Part 2 example`` () = part2 example1 =! "co,de,ka,ta"
