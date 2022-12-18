#r "nuget: Unquote"
open Swensen.Unquote

//https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/fsharp-collection-types#table-of-functions

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

let coords = input |> List.map parse
let droplet = buildDroplet coords

let surface =
    coords
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
