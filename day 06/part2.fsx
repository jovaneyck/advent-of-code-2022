#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example = "mjqjpqmgbljsphdztnvjfqwrcgsmlb" |> List.ofSeq

input
|> List.windowed 14
|> List.indexed
|> List.map (fun (i, window) -> (i + 14, set window))
|> List.find (fun (_, distinctLetters) -> distinctLetters |> Set.count = 14)
|> fst

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
