#r "nuget: Unquote"
open Swensen.Unquote

//https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-collection-types#table-of-functions
//part 2: https://en.wikipedia.org/wiki/Flood_fill
//Let's literally simulate the water to figure out which pixels are on the outside, then perform part 1 on those

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """2,2,2
    1,2,2
    3,2,2
    2,1,2
    2,3,2
    2,2,1
    2,2,3
    2,2,4
    2,2,6
    1,2,5
    3,2,5
    2,1,5
    2,3,5"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (line: string) =
    let [| x; y; z |] = line.Split(",")
    (int x, int y, int z)

let buildDroplet coords =
    let maxX =
        coords
        |> List.map (fun (x, _, _) -> x)
        |> List.max

    let maxY =
        coords
        |> List.map (fun (_, y, _) -> y)
        |> List.max

    let maxZ =
        coords
        |> List.map (fun (_, _, z) -> z)
        |> List.max

    let droplet =
        Array3D.init (maxX + 1) (maxY + 1) (maxZ + 1) (fun x y z ->

            if coords |> Seq.contains (x, y, z) then
                true
            else
                false)

    droplet

let neighbours (x, y, z) =
    [ (x + 1, y, z)
      (x - 1, y, z)
      (x, y + 1, z)
      (x, y - 1, z)
      (x, y, z + 1)
      (x, y, z - 1) ]

let exists droplet (x, y, z) =
    if x < 0 || y < 0 || z < 0 then
        false
    elif x > ((droplet |> Array3D.length1) - 1)
         || y > ((droplet |> Array3D.length2) - 1)
         || z > ((droplet |> Array3D.length3) - 1) then
        false
    else
        droplet[x, y, z]

let rec floodfill droplet queue visited =
    match queue with
    | [] -> visited
    | c :: cs ->
        if visited |> Set.contains c then
            floodfill droplet cs visited
        else
            //printfn "Checking out %A" c

            let reachableNeighbours =
                neighbours c
                //Bounding box
                |> List.filter (fun (x, y, z) -> x >= -1 && y >= -1 && z >= -1)
                |> List.filter (fun (x, y, z) ->
                    x <= (droplet |> Array3D.length1) + 1
                    && y <= (droplet |> Array3D.length2) + 1
                    && z <= (droplet |> Array3D.length3) + 1)
                |> List.filter (fun n -> exists droplet n |> not)

            floodfill droplet (List.append cs reachableNeighbours) (visited |> Set.add c)

let countOutsideSides outside coord =
    let ns = coord |> neighbours
    Set.difference (set ns) outside |> Set.count

let parsedCoords = example |> List.map parse
let droplet = buildDroplet parsedCoords
let outside = floodfill droplet [ (0, 0, 0) ] Set.empty

parsedCoords
|> List.map (countOutsideSides outside)
|> List.sum

let touchingDroplet =
    outside
    |> Seq.filter (fun c ->
        neighbours c
        |> Seq.exists (fun n -> exists droplet n))

let surface =
    touchingDroplet
    |> Seq.sumBy (fun c ->
        6
        - (neighbours c
           |> Seq.filter (exists droplet)
           |> Seq.length))

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
