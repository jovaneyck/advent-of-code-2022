#r "nuget: Unquote"
open Swensen.Unquote

///(row,column)
type Location = int * int
type Elves = Set<Location>

type Direction =
    | N
    | E
    | S
    | W

let directions = [ N; S; W; E ]
let directionForRound n = directions[n % 4]

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let small_example =
    """.....
..##.
..#..
.....
..##.
....."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let example =
    """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#.."""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse grid : Elves =
    [ for (rowNumber, row) in grid |> Seq.indexed do
          for (colNumber, cell) in row |> Seq.indexed do
              if cell = '#' then
                  yield (rowNumber, colNumber) ]
    |> Set.ofList

let neighbouringLocations ((x, y): Location) =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))

let neighbourLocationsTo ((x, y): Location) (dir: Direction) : Location list =
    match dir with
    | N -> [ (-1, -1); (-1, 0); (-1, 1) ]
    | S -> [ (1, -1); (1, 0); (1, 1) ]
    | E -> [ (-1, 1); (0, 1); (1, 1) ]
    | W -> [ (-1, -1); (0, -1); (1, -1) ]
    |> List.map (fun (dx, dy) -> (x + dx, y + dy))

let move ((x, y): Location) (d: Direction) : Location =
    match d with
    | N -> (x - 1, y)
    | S -> (x + 1, y)
    | E -> (x, y + 1)
    | W -> (x, y - 1)

let doRound roundNumber (elves: Elves) : Elves =
    let proposedMoves =
        [ for e in elves ->
              if neighbouringLocations e
                 |> List.filter (fun n -> elves |> Set.contains n)
                 |> List.isEmpty then
                  e, e
              else
                  let priorities =
                      [ roundNumber .. (roundNumber + 3) ]
                      |> List.map directionForRound

                  let next =
                      priorities
                      |> Seq.tryFind (fun dir ->
                          neighbourLocationsTo e dir
                          |> List.filter (fun n -> (elves |> Set.contains n))
                          |> Seq.isEmpty)
                      |> Option.map (fun dir -> move e dir)
                      |> Option.defaultValue e

                  e, next ]

    let start = fst
    let destination = snd
    let groupedMoves = proposedMoves |> List.groupBy destination

    let movedElves =
        groupedMoves
        |> List.collect (fun (dest, proposals) ->
            if proposals |> List.length = 1 then
                [ dest ]
            else
                proposals |> List.map start)
        |> Set.ofList

    movedElves

let rec fix iteration elves =
    printfn "%d" iteration
    let nElves = doRound (iteration - 1) elves

    if nElves = elves then
        iteration, elves
    else
        fix (iteration + 1) nElves

type Range = { Min: int; Max: int }
type BoundingBox = { Row: Range; Column: Range }

let boundingBox (points: Location seq) : BoundingBox =
    let rows = points |> Seq.map fst
    let cols = points |> Seq.map snd
    let rMin = rows |> Seq.min
    let rMax = rows |> Seq.max
    let cMin = cols |> Seq.min
    let cMax = cols |> Seq.max

    { Row = { Min = rMin; Max = rMax }
      Column = { Min = cMin; Max = cMax } }

let printGrid (e: Elves) : unit =
    let box = boundingBox e

    [ for r in box.Row.Min .. box.Row.Max do
          [ for c in box.Column.Min .. box.Column.Max ->
                if e |> Set.contains (r, c) then
                    printf "#"
                else
                    printf "." ]

          printfn "" ]
    |> ignore

let elves = parse input

let result, _ = fix 1 elves

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
