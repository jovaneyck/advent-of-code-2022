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
type Rope = { Head: Location; Tail: Location }

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

let initialRope = { Head = (0, 0); Tail = (0, 0) }

let initial =
    { Rope = initialRope
      VisitedTailLocations = Set.empty }

let moveHead motion state =
    let (x, y) = state.Rope.Head

    match motion with
    | U -> { state with Rope = { state.Rope with Head = (x, y + 1) } }
    | D -> { state with Rope = { state.Rope with Head = (x, y - 1) } }
    | R -> { state with Rope = { state.Rope with Head = (x + 1, y) } }
    | L -> { state with Rope = { state.Rope with Head = (x - 1, y) } }

let newTailLocation (hx, hy) (tx, ty) =
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
    let newTail = newTailLocation state.Rope.Head state.Rope.Tail

    { state with
        Rope = { state.Rope with Tail = newTail }
        VisitedTailLocations = state.VisitedTailLocations |> Set.add newTail }

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
    test <@ (0, 0) = newTailLocation (0, 0) (0, 0) @>
    test <@ (0, 1) = newTailLocation (0, 2) (0, 0) @>
    test <@ (0, -1) = newTailLocation (0, -2) (0, 0) @>
    test <@ (-1, 0) = newTailLocation (-2, 0) (0, 0) @>
    test <@ (1, 0) = newTailLocation (2, 0) (0, 0) @>
    test <@ (3, 4) = newTailLocation (2, 3) (3, 4) @>
    test <@ (1, 2) = newTailLocation (2, 3) (1, 2) @>
    //diagonal jump
    test <@ (2, 2) = newTailLocation (2, 3) (1, 1) @>

    printfn "...done!"

run ()
