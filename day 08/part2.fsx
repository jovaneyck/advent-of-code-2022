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

let allCoordinates =
    [ for r in 0 .. (dimension - 1) do
          for c in 0 .. (dimension - 1) -> (r, c) ]

let neighbours (grid: int [,]) (r, c) =
    let left = grid[r, 0 .. c - 1] |> Array.rev
    let right = grid[r, c + 1 .. (dimension - 1)]
    let above = grid[0 .. r - 1, c] |> Array.rev
    let below = grid[r + 1 .. (dimension - 1), c]
    let neighbourSets = [ left; right; above; below ]
    neighbourSets

let treeAt (grid: int [,]) (r, c) = grid[r, c]

let rec grabTrees tree trees =
    match trees with
    | [] -> []
    | t :: ts when t < tree -> t :: (grabTrees tree ts)
    | t :: _ when t = tree -> [ t ]
    | t :: _ -> [ t ]

let scenicScore neighbours tree =
    neighbours
    |> List.map (fun ns -> ns |> List.ofSeq |> grabTrees tree |> Seq.length)
    |> List.reduce (*)

let bestestTree =
    allCoordinates
    |> List.map (fun coordinate -> coordinate, treeAt grid coordinate, neighbours grid coordinate)
    |> List.map (fun (coordinate, tree, neighbours) -> (coordinate, tree, scenicScore neighbours tree))
    |> List.maxBy (fun (_, _, score) -> score)
