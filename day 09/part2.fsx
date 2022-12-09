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

let initialRope = { Knots = (0, 0) |> List.replicate 10 }

let initial =
    { Rope = initialRope
      VisitedTailLocations = Set.empty }

let moveHead motion state =
    let (x, y) = state.Rope.Knots[0]

    let newHead =
        match motion with
        | U -> (x, y + 1)
        | D -> (x, y - 1)
        | R -> (x + 1, y)
        | L -> (x - 1, y)

    let newKnots = newHead :: (state.Rope.Knots |> List.skip 1)
    { state with Rope = { state.Rope with Knots = newKnots } }

let newKnotLocation (hx, hy) (tx, ty) =
    let distance x y = abs (x - y)

    //on top
    if distance hx tx = 0 && distance hy ty = 0 then
        (tx, ty)
    //directly up
    elif distance hx tx = 0 && hy = ty + 1 then
        (tx, ty)
    elif distance hx tx = 0 && hy = ty + 2 then
        (tx, ty + 1)
    //2 down
    elif hx = tx && hy = ty - 1 then
        (tx, ty)
    elif hx = tx && hy = ty - 2 then
        (tx, ty - 1)
    //2 right
    elif hy = ty && hx = tx + 1 then
        (tx, ty)
    elif hy = ty && hx = tx + 2 then
        (tx + 1, ty)
    //2 left
    elif hy = ty && hx = tx - 1 then
        (tx, ty)
    elif hy = ty && hx = tx - 2 then
        (tx - 1, ty)
    //diagonally touching
    elif distance hx tx = 1 && distance hy ty = 1 then
        (tx, ty)
    else
        let ntx = if hx > tx then tx + 1 else tx - 1
        let nty = if hy > ty then ty + 1 else ty - 1
        (ntx, nty)

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

let instructions = input |> List.map parseInstruction
let endState = instructions |> List.fold apply initial
endState.VisitedTailLocations |> Set.count

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

    printfn "...done!"

run ()
