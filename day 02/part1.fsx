#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """A Y
    B X
    C Z"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofArray

type Shape =
    | Rock
    | Paper
    | Scissors

type Round = { Opponent: Shape; Me: Shape }

let parseLine (line: string) =
    let [| opp; my |] = line.Split(" ")

    let opponentShape =
        match opp with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors

    let myShape =
        match my with
        | "X" -> Rock
        | "Y" -> Paper
        | "Z" -> Scissors

    { Opponent = opponentShape
      Me = myShape }

let score (round: Round) =
    let shapeScore =
        match round.Me with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let winScore =
        match (round.Me, round.Opponent) with
        | Rock, Scissors
        | Paper, Rock
        | Scissors, Paper -> 6
        | me, opp when me = opp -> 3
        | _ -> 0

    shapeScore + winScore

let rounds = input |> List.map parseLine
rounds |> List.map score |> List.sum

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
