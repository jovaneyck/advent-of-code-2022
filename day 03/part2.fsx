#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """vJrwpWtwJgWrhcsFMMfFFhFp
    jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
    PmmdzqPrVvPwwTWBwg
    wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
    ttgJtRGJQctTZtZT
    CrZsJsPPZsGzwwsLwLmpwMDw"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let findBadge [ first; second; third ] =
    [ for item in first do
          if second |> Seq.contains item
             && third |> Seq.contains item then
              yield item ]
    |> Seq.head

let priority (item: char) =
    if System.Char.IsLower item then
        int item - 96
    else
        int item - 38

let solve lines =
    let groups = lines |> List.chunkBySize 3

    groups
    |> List.map (findBadge >> priority)
    |> List.sum

solve input

let run () =
    printf "Testing..."
    test <@ priority 'a' = 1 @>
    test <@ priority 'z' = 26 @>
    test <@ priority 'A' = 27 @>
    test <@ priority 'Z' = 52 @>
    test <@ solve example = 70 @>
    printfn "...done!"

run ()
