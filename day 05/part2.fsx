#r "nuget: Unquote"

open Swensen.Unquote
open System.Text.RegularExpressions

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"""
        .Split("\n")
    |> Seq.skip 1
    |> List.ofSeq

type Stack = char list
type Stacks = Stack list

type Instruction =
    { NumberStacks: int
      Source: int
      Destination: int }

let parseInstruction instruction =
    let regex = Regex("move (\d+) from (\d) to (\d)", RegexOptions.Compiled)
    let m = regex.Match(instruction)
    let numberStacks = m.Groups.[1].Value |> int
    let src = m.Groups.[2].Value |> int
    let dest = m.Groups.[3].Value |> int

    { NumberStacks = numberStacks
      Source = src
      Destination = dest }

let parse lines =
    let initial =
        lines
        |> List.takeWhile ((<>) "")
        |> List.rev
        |> List.skip 1
        |> List.rev

    let parseRow row =
        row
        |> Seq.chunkBySize 4
        |> Seq.map (function
            | [| ' '; ' '; ' '; ' ' |] -> None
            | [| ' '; ' '; ' ' |] -> None
            | [| '['; c; ']' |] -> Some c
            | [| '['; c; ']'; ' ' |] -> Some c)
        |> List.ofSeq

    let rows = initial |> List.map parseRow

    let stacks =
        [ for i in 0 .. (rows.[0] |> Seq.length) - 1 ->
              rows
              |> List.map (fun row -> row.[i])
              |> List.choose id ]

    let instructions =
        lines
        |> List.skipWhile ((<>) "")
        |> List.skip 1
        |> List.map parseInstruction

    (stacks, instructions)

let applyInstruction (stacks: Stacks) (i: Instruction) =
    let (cratesToMove, restSource) =
        stacks[i.Source - 1]
        |> List.splitAt i.NumberStacks

    let newDest = cratesToMove @ stacks[i.Destination - 1]

    stacks
    |> List.mapi (fun idx element ->
        if idx = (i.Source - 1) then
            restSource
        elif idx = (i.Destination - 1) then
            newDest
        else
            element)

let solve input =
    let (stacks, instructions) = parse input
    let endStacks = instructions |> List.fold applyInstruction stacks

    let solution =
        endStacks
        |> List.map List.head
        |> List.map string
        |> String.concat ""

    solution

#time
solve input

let run () =
    printf "Testing..."

    test
        <@ parseInstruction "move 13 from 1 to 3" = { NumberStacks = 13
                                                      Source = 1
                                                      Destination = 3 } @>

    test <@ solve example = "CMZ" @>

    printfn "...done!"

run ()
