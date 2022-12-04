#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """2-4,6-8
    2-3,4-5
    5-7,7-9
    2-8,3-7
    6-6,4-6
    2-6,4-8"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let parse (line: string) =
    let [| f; s |] = line.Split(",")
    let [| b1; e1 |] = f.Split("-")
    let [| b2; e2 |] = s.Split("-")
    ((int b1, int e1), (int b2, int e2))

let subsumes ((b1, e1), (b2, e2)) =
    (b1 <= b2 && b2 <= e1 && b1 <= e2 && e2 <= e1)
    || (b2 <= b1 && b1 <= e2 && b2 <= e1 && e1 <= e2)

input
|> List.map parse
|> List.filter subsumes
|> List.length

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
