#r "nuget: Unquote"
#r "nuget: FParsec"

open FParsec
open Swensen.Unquote

let input =
    $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> System.IO.File.ReadAllText
    |> fun t -> t.Split($"{System.Environment.NewLine}")
    |> Array.filter (fun s -> s <> "")
    |> Array.toList

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
        .Split()
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> s <> "")
    |> Array.toList

type Packet =
    | Int of int
    | List of Packet list

let packetParser, packetParserRef = createParserForwardedToRef ()

let intParser = pint32 |>> Int

let listParser =
    (pchar '[') >>. (sepBy packetParser (pchar ','))
    .>> (pchar ']')
    |>> List

packetParserRef.Value <- (listParser <|> intParser)
let parser = packetParser .>> eof

let parse packet = run parser packet

let parsePacket packet =
    let result = parse packet

    match result with
    | Success (res, _, _) -> res
    | _ -> failwithf "%A" result

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

let divTwo = List [ List [ Int 2 ] ]
let divSix = List [ List [ Int 6 ] ]
let dividers = [ divTwo; divSix ]

let ssorted left right =
    match sorted left right with
    | Some true -> -1
    | Some false -> 1
    | None -> failwithf "Could not compare %A with %A" left right

let solve input =
    let parsed = input |> List.map parsePacket
    let full = List.append dividers parsed
    let s = full |> List.sortWith ssorted
    let idx2 = 1 + (s |> List.findIndex ((=) divTwo))
    let idx6 = 1 + (s |> List.findIndex ((=) divSix))
    let decoderKey = idx2 * idx6
    decoderKey

test
    <@ let (ParserResult.Success (r, _, _)) = parse "[[1],[2,3,4]]"

       r = List [ List [ Int 1 ]
                  List [ Int 2; Int 3; Int 4 ] ] @>

test
    <@ let (ParserResult.Failure (e, _, _)) = parse "[[1],[2,3,4]]   "

       e.Contains("Expecting: end of input") @>

test <@ solve example = 140 @>

solve input
