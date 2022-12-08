#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """ 30373
        25512
        65332
        33549
        35390"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse lines =
    array2D lines |> Array2D.map (string >> int)

let grid = parse input
let dimension = grid |> Array2D.length1

let innerCoordinates =
    [ for r in 1 .. (dimension - 2) do
          for c in 1 .. (dimension - 2) -> (r, c) ]

let neighbours (grid: int [,]) (r, c) =
    let left = grid[r, 0 .. c - 1]
    let right = grid[r, c + 1 .. (dimension - 1)]
    let above = grid[0 .. r - 1, c]
    let below = grid[r + 1 .. (dimension - 1), c]
    let neighbourSets = [ left; right; above; below ]
    neighbourSets

let treeAt (grid: int [,]) (r, c) = grid[r, c]

let isVisible tree neighbours =
    neighbours
    |> Seq.exists (fun ns -> ns |> Seq.forall (fun n -> n < tree))

let nbVisibleInnerTrees =
    innerCoordinates
    |> List.map (fun coordinate -> coordinate, treeAt grid coordinate, neighbours grid coordinate)
    |> List.filter (fun (coordinate, tree, neighbours) -> isVisible tree neighbours)
    |> List.length

let nbTreesOnEdge = 4 * dimension - 4

nbVisibleInnerTrees + nbTreesOnEdge

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
