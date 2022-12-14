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

let isfree floor location cave =
    match (snd location = floor), cave |> thingAt location with
    | true, _ -> false
    | _, None -> true
    | _, Some _ -> false

let rec dropSand (startx, starty) floor (cave: Cave) : Cave =
    //printfn "CAVE %A" (cave |> Map.toList)

    let onY =
        cave
        |> Map.filter (fun (x, y) _ -> x = startx && y > starty)
        |> Map.toList
        |> List.append [ (startx, floor), Rock ]

    //printfn "onY %A" onY

    let (thingx, thingy), _ =
        onY
        |> List.sortBy (fun ((_, y), _) -> y)
        |> List.head

    //printfn "(thing loc) %A" (thingx, thingy)
    let (x, y) = (thingx, thingy - 1)
    let bottomLeft = (x - 1, y + 1)
    let bottomRight = (x + 1, y + 1)

    if cave |> isfree floor bottomLeft then
        dropSand bottomLeft floor cave
    elif cave |> isfree floor bottomRight then
        dropSand bottomRight floor cave
    else
        cave |> update (x, y) SandAtRest

let origin = (500, 0)

let cave =
    input
    |> List.collect pathCoords
    |> List.map (fun c -> c, Rock)
    |> Map.ofSeq

let floor = 2 + (cave |> Map.keys |> Seq.maxBy snd |> snd)

let rec untilfull iteration floor cave : (int * Cave) =
    printfn "%d" iteration
    let next = cave |> dropSand origin floor

    if cave |> thingAt origin |> Option.isSome then
        (iteration - 1, next)
    else
        untilfull (iteration + 1) floor next

#time
//Yeaaaa....
//Real: 00:09:30.705, CPU: 00:09:29.656, GC gen0: 192550, gen1: 58, gen2: 5
//27324
let finish = untilfull 1 floor cave
fst finish

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
