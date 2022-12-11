#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let smallExample =
    """noop
        addx 3
        addx -5"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let example =
    """addx 15
    addx -11
    addx 6
    addx -3
    addx 5
    addx -1
    addx -8
    addx 13
    addx 4
    noop
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx 5
    addx -1
    addx -35
    addx 1
    addx 24
    addx -19
    addx 1
    addx 16
    addx -11
    noop
    noop
    addx 21
    addx -15
    noop
    noop
    addx -3
    addx 9
    addx 1
    addx -3
    addx 8
    addx 1
    addx 5
    noop
    noop
    noop
    noop
    noop
    addx -36
    noop
    addx 1
    addx 7
    noop
    noop
    noop
    addx 2
    addx 6
    noop
    noop
    noop
    noop
    noop
    addx 1
    noop
    noop
    addx 7
    addx 1
    noop
    addx -13
    addx 13
    addx 7
    noop
    addx 1
    addx -33
    noop
    noop
    noop
    addx 2
    noop
    noop
    noop
    addx 8
    noop
    addx -1
    addx 2
    addx 1
    noop
    addx 17
    addx -9
    addx 1
    addx 1
    addx -3
    addx 11
    noop
    noop
    addx 1
    noop
    addx 1
    noop
    noop
    addx -13
    addx -19
    addx 1
    addx 3
    addx 26
    addx -30
    addx 12
    addx -1
    addx 3
    addx 1
    noop
    noop
    noop
    addx -9
    addx 18
    addx 1
    addx 2
    noop
    noop
    addx 9
    noop
    noop
    noop
    addx -1
    addx 2
    addx -37
    addx 1
    addx 3
    noop
    addx 15
    addx -21
    addx 22
    addx -6
    addx 1
    noop
    addx 2
    addx 1
    noop
    addx -10
    noop
    noop
    addx 20
    addx 1
    addx 2
    addx 2
    addx -6
    addx -11
    noop
    noop
    noop"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Instruction =
    | NOOP
    | ADDX of int

let parse (line: string) =
    let split = line.Split(' ')

    match split[0] with
    | "noop" -> NOOP
    | "addx" -> ADDX(int split[1])

let cyclesForInstruction prev instruction =
    let x = prev |> Seq.last

    match instruction with
    | NOOP -> [ x ]
    | ADDX v -> [ x; x + v ]


let instructions = input |> List.map parse

let cycles =
    instructions
    |> Seq.scan cyclesForInstruction [ 1 ]
    |> Seq.concat

let result =
    [ 20; 60; 100; 140; 180; 220 ]
    |> List.map (fun c -> c, cycles |> Seq.item (c - 1))
    |> List.map (fun t -> t ||> (*))
    |> List.sum

let run () =
    printf "Testing..."
    test <@ parse "addx 20" = ADDX 20 @>
    test <@ parse "addx -11" = ADDX -11 @>
    test <@ parse "noop" = NOOP @>
    printfn "...done!"

run ()
