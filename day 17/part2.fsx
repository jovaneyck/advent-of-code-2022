#r "nuget: Unquote"
open Swensen.Unquote

let sw = System.Diagnostics.Stopwatch.StartNew()

let printReset msg =
    printfn $"{msg} ({sw.ElapsedMilliseconds} ms)"
    sw.Restart()

///A coordinate in our chamber.
///       ymax
///        |
///        |
/// (x,) x0,y0 __ __ xmax
type Coord = int * int

///A pattern is a set of coordinates relative to the shape's origin (0,0)
type Pattern = Set<Coord>
let pattern = set
let contains c p = p |> Set.contains c

///A rock keeps track of its location and its pattern
type Rock = { Location: Coord; Pattern: Pattern }

let absoluteCoords (rock: Rock) =
    let ox, oy = rock.Location

    rock.Pattern
    |> Set.map (fun (x, y) -> (x + ox, y + oy))

let print (pattern: Pattern) =
    let xmin = pattern |> Seq.map fst |> Seq.min
    let xmax = pattern |> Seq.map fst |> Seq.max
    let ymin = pattern |> Seq.map snd |> Seq.min
    let ymax = pattern |> Seq.map snd |> Seq.max

    [ for y in ymax .. -1 .. ymin do
          [ for x in xmin..xmax ->
                if pattern |> contains (x, y) then
                    printf "#"
                else
                    printf "." ]

          printfn "" ]
    |> ignore

    printfn ""

let patterns: Pattern list =
    [ pattern [ (0, 0)
                (1, 0)
                (2, 0)
                (3, 0) ]
      pattern [ (1, 0)
                (1, 1)
                (0, 1)
                (2, 1)
                (1, 2) ]
      pattern [ (0, 0)
                (1, 0)
                (2, 0)
                (2, 1)
                (2, 2) ]
      pattern [ (0, 0)
                (0, 1)
                (0, 2)
                (0, 3) ]
      pattern [ (0, 0)
                (0, 1)
                (1, 0)
                (1, 1) ] ]

patterns |> Seq.iter print

///The camber contains all stable rocks
type Chamber = { Width: int; Blocked: Set<Coord> }

let emptyChamber width : Chamber =
    let floor = [ for x in 0 .. (width - 1) -> (x, -1) ]
    { Width = width; Blocked = set floor }

let top chamber =
    1 + (chamber.Blocked |> Seq.map snd |> Seq.max)

let add (rock: Rock) (chamber: Chamber) =
    let newBlocked = Set.union chamber.Blocked (rock |> absoluteCoords)

    { chamber with Blocked = newBlocked }

let printChamber (chamber: Chamber) = print chamber.Blocked

///A jet pushes left or right
type Jet =
    | Left
    | Right

let parse =
    function
    | '>' -> Right
    | '<' -> Left
    | u -> failwithf "Unknown jet: %c" u

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""
let example = """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

let jets = input |> Seq.map parse |> Seq.toArray
let maxJet = jets |> Seq.length
let jetAt idx = jets[idx % maxJet]

let maxPattern = patterns |> Seq.length
let patternAt rockNb = patterns[(rockNb - 1) % maxPattern]

let apply (jet: Jet) (rock: Rock) : Rock =
    let (x, y) = rock.Location

    match jet with
    | Left -> { rock with Location = (x - 1, y) }
    | Right -> { rock with Location = (x + 1, y) }

let collides (chamber: Chamber) (rock: Rock) : bool =
    let coords = rock |> absoluteCoords

    if coords |> Seq.exists (fun (x, y) -> x < 0) then
        true
    elif coords
         |> Seq.exists (fun (x, y) -> x > (chamber.Width - 1)) then
        true
    elif Set.intersect chamber.Blocked coords
         |> Seq.isEmpty
         |> not then
        true
    else
        false

let push chamber jet rock =
    let pushed = rock |> apply jet

    if pushed |> collides chamber then
        rock
    else
        pushed

let lower height (rock: Rock) =
    let (x, y) = rock.Location
    { rock with Location = (x, y - height) }

let drop (chamber: Chamber) (rock: Rock) : Rock =
    let dropped = rock |> lower 1

    if dropped |> collides chamber then
        rock
    else
        dropped

let rec falldown jetIndex chamber rock =
    let j = jetAt jetIndex

    let pushed = rock |> push chamber j
    let dropped = pushed |> drop chamber

    if dropped = pushed then
        (jetIndex + 1, pushed)
    else
        falldown (jetIndex + 1) chamber dropped

let mutable x = 0

let fall (jetIndex: int, chamber: Chamber) (rockNumber: int) : (int * Chamber) =
    x <- x + 1

    printfn "Rock %d" x
    let pattern = patternAt rockNumber

    let rock: Rock =
        { Location = (2, 3 + (chamber |> top))
          Pattern = pattern }

    let js, dropped = falldown jetIndex chamber rock
    let res = js, chamber |> add dropped
    res

let (_, c) = Seq.fold fall (0, emptyChamber 7) [ 1..2022 ]
c |> printChamber
let result = c |> top

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
