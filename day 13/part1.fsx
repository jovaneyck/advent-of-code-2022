#r "nuget: Unquote"
#r "nuget: FParsec"

open Swensen.Unquote
open FParsec

let input =
    $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> System.IO.File.ReadAllText
    |> fun t -> t.Split($"{System.Environment.NewLine}{System.Environment.NewLine}")


let example =
    """[1,1,3,1,1]
[1,1,5,1,1]
    
[[1],[2,3,4]]
[[1],4]
    
[9]
[[8,7,6]]
    
[[4,4],4,4]
[[4,4],4,4,4]
    
[7,7,7,7]
[7,7,7]
    
[]
[3]
    
[[[]]]
[[]]
    
[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"""
        .Split("\n    \n")

type Packet =
    | Int of int
    | List of Packet list

let packetParser, packetParserRef = createParserForwardedToRef ()

let intParser = pint32 |>> Int

let listParser =
    (pchar '[') >>. (sepBy packetParser (pchar ','))
    .>> (pchar ']')
    |>> List

packetParserRef.Value <- (listParser <|> intParser) //TODO: eof

let parsePacket packet =
    let result = run packetParser packet

    match result with
    | Success (res, _, _) -> res
    | _ -> failwithf "%A" result

let parse (packets: string []) =
    packets
    |> Array.map (fun ps ->
        let [ left; right ] =
            ps.Split("\n")
            |> Array.map parsePacket
            |> Array.toList

        (left, right))
    |> Array.toList

let rec sorted left right =
    match left, right with
    | Int l, Int b when l < b -> true |> Some
    | Int l, Int b when l > b -> false |> Some
    | Int l, Int b when l = b -> None
    | List [], List [] -> None
    | List [], List _ -> true |> Some
    | List _, List [] -> false |> Some
    | List (l :: ls), List (r :: rs) ->
        match sorted l r with
        | Some true -> Some true
        | Some false -> Some false
        | None -> sorted (List ls) (List rs)
    | List l, Int r -> sorted (List l) (List [ Int r ])
    | Int l, List r -> sorted (List [ Int l ]) (List r)

let solve input =
    parse input
    |> List.map (fun t -> t ||> sorted)
    |> List.indexed
    |> List.map (fun (i, f) -> (i + 1, f))
    |> List.filter (fun (_, t) ->
        match t with
        | Some true -> true
        | _ -> false)
    |> List.sumBy fst

solve input

let run () =
    printf "Testing..."
    test <@ solve example = 13 @>
    printfn "...done!"

run ()
