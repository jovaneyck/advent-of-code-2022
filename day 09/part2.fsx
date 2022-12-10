#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """R 4
    U 4
    L 3
    D 1
    R 4
    D 1
    L 5
    R 2"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Location = int * int

type Rope = { Knots: Location list }

type Motion =
    | U
    | D
    | L
    | R

type Instruction = { Motion: Motion; Distance: int }

let parseInstruction (line: string) =
    let [| i; rest |] = line.Split(' ')
    let distance = int rest

    let motion =
        match i with
        | "U" -> U
        | "D" -> D
        | "L" -> L
        | "R" -> R

    { Motion = motion; Distance = distance }

type State =
    { Rope: Rope
      VisitedTailLocations: Set<Location> }

let initial =
    { Rope = { Knots = (0, 0) |> List.replicate 10 }
      VisitedTailLocations = Set.empty }

let moveHead motion state =
    let ((x, y) :: tails) = state.Rope.Knots

    let newHead =
        match motion with
        | U -> (x, y + 1)
        | D -> (x, y - 1)
        | R -> (x + 1, y)
        | L -> (x - 1, y)

    { state with Rope = { state.Rope with Knots = newHead :: tails } }

let newKnotLocation (hx, hy) (tx, ty) =
    let distance x y = abs (x - y)

    match distance hx tx, distance hy ty with
    | 0, 2 when hy = ty + 2 -> (tx, ty + 1)
    | 0, 2 when hy = ty - 2 -> (tx, ty - 1)
    | 2, 0 when hx = tx + 2 -> (tx + 1, ty)
    | 2, 0 when hx = tx - 2 -> (tx - 1, ty)
    | 1, 1 -> (tx, ty)
    | n, m when n >= 1 && m >= 1 ->
        let ntx = if hx > tx then tx + 1 else tx - 1
        let nty = if hy > ty then ty + 1 else ty - 1
        (ntx, nty)
    | _ -> (tx, ty)

let moveTail state =
    //YOLO MODE ENGAGED
    let (h :: t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: t7 :: t8 :: t9 :: []) =
        state.Rope.Knots

    let nT1 = newKnotLocation h t1
    let nT2 = newKnotLocation nT1 t2
    let nT3 = newKnotLocation nT2 t3
    let nT4 = newKnotLocation nT3 t4
    let nT5 = newKnotLocation nT4 t5
    let nT6 = newKnotLocation nT5 t6
    let nT7 = newKnotLocation nT6 t7
    let nT8 = newKnotLocation nT7 t8
    let nT9 = newKnotLocation nT8 t9

    { state with
        VisitedTailLocations = state.VisitedTailLocations |> Set.add nT9
        Rope =
            { Knots =
                [ h
                  nT1
                  nT2
                  nT3
                  nT4
                  nT5
                  nT6
                  nT7
                  nT8
                  nT9 ] } }

let moveOne motion state = state |> moveHead motion |> moveTail

let rec applyRec state instruction =
    match instruction.Distance with
    | 0 -> state
    | d ->
        let step = state |> moveOne instruction.Motion
        applyRec step { instruction with Distance = d - 1 }

let apply state instruction =
    //printfn "head @ %A; tail @ %A" state.Rope.Head state.Rope.Tail
    applyRec state instruction

let solve input =
    let instructions = input |> List.map parseInstruction
    let endState = instructions |> List.fold apply initial
    endState.VisitedTailLocations |> Set.count

solve example

let run () =
    printf "Testing..."
    test <@ (0, 0) = newKnotLocation (0, 0) (0, 0) @>
    test <@ (0, 1) = newKnotLocation (0, 2) (0, 0) @>
    test <@ (0, -1) = newKnotLocation (0, -2) (0, 0) @>
    test <@ (-1, 0) = newKnotLocation (-2, 0) (0, 0) @>
    test <@ (1, 0) = newKnotLocation (2, 0) (0, 0) @>
    test <@ (3, 4) = newKnotLocation (2, 3) (3, 4) @>
    test <@ (1, 2) = newKnotLocation (2, 3) (1, 2) @>
    //diagonal jump
    test <@ (2, 2) = newKnotLocation (2, 3) (1, 1) @>
    test <@ solve example = 1 @>
    test <@ solve input = 2427 @>
    printfn "...done!"

run ()
