#r "nuget: Unquote"
#r "nuget: FSharpx.Collections"

open Swensen.Unquote
open FSharpx.Collections

///Dijkstra Priority Queue. Combines a (priority) Heap with a visited Set so we can update priorities of a node without having to iterate over the entire heap
module DPQ =
    type State<'t> when 't: comparison =
        { Heap: Heap<int * 't>
          Visited: Set<'t>
          Distances: Map<'t, int> }

    let private heapOf s = Heap.ofSeq false s

    let ofSeq s =
        { Heap = heapOf s
          Visited = Set.empty
          Distances = Map.empty }

    let rec tryUncons pq =
        pq.Heap
        |> Heap.tryUncons
        |> Option.bind (fun ((d, h), t) ->
            if pq.Visited |> Set.contains h then
                tryUncons { pq with Heap = t }
            else
                ((d, h),
                 { pq with
                     Visited = pq.Visited |> Set.add h
                     Distances = pq.Distances |> Map.add h d
                     Heap = t })
                |> Some)

    let updateDistances updates pq =
        { pq with Heap = pq.Heap |> Heap.merge (heapOf updates) }

    let isEmpty pq = pq |> tryUncons |> Option.isNone

    let visited x pq = pq.Visited |> Set.contains x

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """Sabqponm
    abcryxxl
    accszExk
    acctuvwj
    abdefghi"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let coordsIn grid =
    [ for y in [ 0 .. (grid |> Array2D.length1) - 1 ] do
          for x in [ 0 .. (grid |> Array2D.length2) - 1 ] -> (x, y) ]

let findLocationsOf character grid =
    grid
    |> Array2D.mapi (fun y x el -> (x, y, el))
    |> Seq.cast<int * int * char>
    |> Seq.choose (fun (x, y, el) ->
        if el = character then
            Some(x, y)
        else
            None)

type Location = int * int
type Grid = char [,]
let at (grid: Grid) ((x, y): Location) = grid[y, x]
let xlength (grid: Grid) = Array2D.length2 grid
let ylength (grid: Grid) = Array2D.length1 grid

let neighbourLocations (x, y) (grid: Grid) =
    let locs =
        seq {
            if x <= (grid |> xlength) - 2 then
                yield (x + 1, y)

            if x >= 1 then yield (x - 1, y)

            if y <= (grid |> ylength) - 2 then
                yield (x, y + 1)

            if y >= 1 then yield (x, y - 1)
        }

    locs |> Seq.toList

let canClimb (currentHeight: char) (height: char) = (1 + int currentHeight) >= (int height)

///Immutable version of Dijkstra's shortest path algorithm
///https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode
let rec dijkstra (pq: DPQ.State<Location>) grid endings =
    match pq |> DPQ.tryUncons with
    | None -> 0
    | Some ((dist, coord), pqrest) ->
        if endings |> Set.contains coord then
            dist
        else
            let neighbs = neighbourLocations coord grid

            let unvisitedNeighbourDistances =
                neighbs
                |> List.filter (fun n -> canClimb (at grid n) (at grid coord))
                |> List.filter (fun n -> pqrest |> DPQ.visited n |> not)
                |> List.map (fun n -> (dist + 1, n))

            let next =
                pqrest
                |> DPQ.updateDistances unvisitedNeighbourDistances

            dijkstra next grid endings

let solve input =
    let rawgrid: Grid = input |> array2D
    let start = rawgrid |> findLocationsOf 'S' |> Seq.last
    let ending = rawgrid |> findLocationsOf 'E' |> Seq.last

    let grid: Grid =
        rawgrid
        |> Array2D.mapi (fun y x el ->
            match (x, y) with
            | p when p = start -> 'a'
            | p when p = ending -> 'z'
            | _ -> el)

    let dpq: DPQ.State<Location> =
        grid
        |> coordsIn
        |> List.map (fun c ->
            if c = ending then
                (0, c)
            else
                (System.Int32.MaxValue, c))
        |> DPQ.ofSeq

    let aLocations = grid |> findLocationsOf 'a' |> Set.ofSeq
    let result = dijkstra dpq grid aLocations
    result



let runT () =
    printf "Testing..."

    test
        <@ let (Some (h, _)) = [ (1, 'a') ] |> DPQ.ofSeq |> DPQ.tryUncons
           h = (1, 'a') @>

    test
        <@ let (Some (_, t)) = [ (1, 'a') ] |> DPQ.ofSeq |> DPQ.tryUncons
           t |> DPQ.isEmpty @>

    test
        <@ let (Some (h, _)) =
            [ (3, 'b'); (2, 'a') ]
            |> DPQ.ofSeq
            |> DPQ.updateDistances [ (1, 'b') ]
            |> DPQ.tryUncons

           h = (1, 'b') @>

    test
        <@ let (Some (h, _)) =
            [ (3, 'b'); (2, 'a') ]
            |> DPQ.ofSeq
            |> DPQ.updateDistances [ (1, 'b') ]
            |> DPQ.tryUncons
            |> Option.bind (fun (_, t) -> t |> DPQ.tryUncons)

           h = (2, 'a') @>

    test
        <@ let (Some (_, t)) =
            [ (3, 'b'); (2, 'a') ]
            |> DPQ.ofSeq
            |> DPQ.updateDistances [ (1, 'b') ]
            |> DPQ.tryUncons
            |> Option.bind (fun (_, t) -> t |> DPQ.tryUncons)

           t |> DPQ.isEmpty @>

    test <@ canClimb 'b' 'c' @>
    test <@ canClimb 'b' 'b' @>
    test <@ canClimb 'b' 'a' @>
    test <@ canClimb 'a' 'c' |> not @>

    test <@ solve example = 29 @>
    printfn "...done!"

runT ()

#time //Real: 00:00:00.044, CPU: 00:00:00.031, GC gen0: 9, gen1: 0, gen2: 0
solve input
