#r "nuget: Unquote"
open Swensen.Unquote

let input =
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

type Outcome =
    | Draw
    | Win
    | Lose

type Round =
    { Opponent: Shape
      DesiredOutcome: Outcome }

let parseLine (line: string) =
    let [| opp; my |] = line.Split(" ")

    let opponentShape =
        match opp with
        | "A" -> Rock
        | "B" -> Paper
        | "C" -> Scissors

    let outcome =
        match my with
        | "X" -> Lose
        | "Y" -> Draw
        | "Z" -> Win

    { Opponent = opponentShape
      DesiredOutcome = outcome }

let myMove (round: Round) =
    match round.DesiredOutcome, round.Opponent with
    | Draw, o -> o
    | Win, Rock -> Paper
    | Win, Paper -> Scissors
    | Win, Scissors -> Rock
    | Lose, Rock -> Scissors
    | Lose, Paper -> Rock
    | Lose, Scissors -> Paper

let score (round: Round) =
    let ourMove = myMove round

    let shapeScore =
        match ourMove with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let winScore =
        match (ourMove, round.Opponent) with
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
