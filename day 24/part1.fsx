#r "nuget: Unquote"
open Swensen.Unquote

type Direction =
    | U
    | D
    | L
    | R

type Location = int * int
type Blizzard = Direction * Location

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Dimensions = { Width: int; Height: int }

type Parsed =
    { Blizzards: Blizzard list
      Dimensions: Dimensions }

let parse grid =
    let h = (grid |> List.length) - 2
    let w = (grid[0] |> Seq.length) - 2

    let blizzards =
        [ for r, row in grid |> List.skip 1 |> List.indexed do
              for c, cell in row |> Seq.skip 1 |> Seq.indexed do
                  if cell = '>' then yield R, (r, c)
                  elif cell = '<' then yield L, (r, c)
                  elif cell = 'v' then yield D, (r, c)
                  elif cell = '^' then yield U, (r, c) ]

    { Blizzards = blizzards
      Dimensions = { Width = w; Height = h } }

let parsed = parse input
let start = (-1, 0)
let finish = (parsed.Dimensions.Height, parsed.Dimensions.Width - 1)

let validNextMoves (r, c) world =
    let allMoves =
        [ (-1, 0)
          (1, 0)
          (0, 1)
          (0, -1)
          (0, 0) ]
        |> List.map (fun (dr, dc) -> (r + dr, c + dc))

    let onGrid =
        allMoves
        |> List.filter (fun (r, c) ->
            (r, c) = start
            || (r, c) = finish
            || (r >= 0
                && r <= (world.Dimensions.Height - 1)
                && c >= 0
                && c <= (world.Dimensions.Width - 1)))

    let noBlizzards =
        onGrid
        |> List.filter (fun pos ->
            world.Blizzards
            |> List.exists (fun (_, p) -> pos = p)
            |> not)

    noBlizzards

let moveBlizzard dimensions blizzard : Blizzard =
    let (d, (r, c)) = blizzard

    match d with
    | U ->
        U,
        ((if r = 0 then
              dimensions.Height - 1
          else
              r - 1),
         c)
    | D ->
        D,
        ((if r = dimensions.Height - 1 then
              0
          else
              r + 1),
         c)
    | L ->
        L,
        (r,
         (if c = 0 then
              dimensions.Width - 1
          else
              c - 1))
    | R ->
        R,
        (r,
         (if c = dimensions.Width - 1 then
              0
          else
              c + 1))

let moveBlizzards state =
    { state with
        Blizzards =
            state.Blizzards
            |> List.map (moveBlizzard state.Dimensions) }

let rec bfs length state positions finish =
    if positions |> Set.contains finish then
        length, state
    else
        let nextBlizzards = moveBlizzards state

        let next =
            positions
            |> Seq.collect (fun position -> validNextMoves position nextBlizzards)
            |> set

        bfs (length + 1) nextBlizzards next finish

let length1, blizzards1 = bfs 0 parsed (Set.singleton start) finish
let length2, blizzards2 = bfs length1 blizzards1 (Set.singleton finish) start
let solution, _ = bfs length2 blizzards2 (Set.singleton start) finish

solution
