#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """498,4 -> 498,6 -> 496,6
    503,4 -> 502,4 -> 502,9 -> 494,9"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Thing =
    | Rock
    | SandAtRest

type Cave = Map<int * int, Thing>

let parseCoordinate (coord: string) =
    let coords = coord.Split(",") |> Seq.map int
    (coords |> Seq.item 0, coords |> Seq.item 1)

let generatePointsOnLine ((x1, y1), (x2, y2)) =
    let [ xmin; xmax ] = [ x1; x2 ] |> List.sort
    let [ ymin; ymax ] = [ y1; y2 ] |> List.sort

    [ for x in xmin..xmax do
          for y in ymin..ymax -> (x, y) ]

let pathCoords (line: string) =
    let corners =
        line.Split(" -> ")
        |> Seq.map parseCoordinate
        |> Seq.toList

    corners
    |> List.pairwise
    |> List.collect generatePointsOnLine
    |> List.distinct

let update location thing cave = cave |> Map.add location thing
let thingAt location cave = cave |> Map.tryFind location

let isfree location cave =
    match cave |> thingAt location with
    | None -> true
    | Some _ -> false

let rec dropSand (startx, starty) (cave: Cave) : Cave =
    //printfn "CAVE %A" (cave |> Map.toList)

    let onY =
        cave
        |> Map.filter (fun (x, y) _ -> x = startx && y > starty)
        |> Map.toList

    //printfn "onY %A" onY
    if onY |> Seq.isEmpty then
        cave
    else
        let (thingx, thingy), _ =
            onY
            |> List.sortBy (fun ((_, y), _) -> y)
            |> List.head

        //printfn "(thing loc) %A" (thingx, thingy)
        let (x, y) = (thingx, thingy - 1)
        let bottomLeft = (x - 1, y + 1)
        let bottomRight = (x + 1, y + 1)

        if cave |> isfree bottomLeft then
            dropSand bottomLeft cave
        elif cave |> isfree bottomRight then
            dropSand bottomRight cave
        else
            cave |> update (x, y) SandAtRest

let origin = (500, 0)

let cave =
    input
    |> List.collect pathCoords
    |> List.map (fun c -> c, Rock)
    |> Map.ofSeq

let rec fixpoint iteration cave : (int * Cave) =
    //printfn "%d" iteration
    let next = cave |> dropSand origin

    if cave = next then
        (iteration - 1, next)
    else
        fixpoint (iteration + 1) next

let finish = fixpoint 1 cave

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
