#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """1000
    2000
    3000
    
    4000
    
    5000
    6000
    
    7000
    8000
    9000
    
    10000"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofArray

let rec splitPerElf lines =
    if lines |> Seq.isEmpty then
        []
    else
        let caloriesForElf = lines |> List.takeWhile (fun r -> r <> "")

        let rest =
            lines
            |> List.skipWhile (fun r -> r <> "")
            |> List.skip 1

        caloriesForElf :: (splitPerElf rest)

let solve lines =
    let elfCalories = splitPerElf (List.append lines [ "" ])
    let parseCalories (cals: string list) = cals |> List.map int
    let parsed = elfCalories |> List.map parseCalories
    let sums = parsed |> List.map List.sum

    sums
    |> List.sortDescending
    |> List.take 3
    |> List.sum

solve input

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
