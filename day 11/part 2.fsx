#r "nuget: Unquote"
open Swensen.Unquote

type Monkey =
    { Id: int
      NumberInspections: int64
      Items: int64 list
      Operation: int64 -> int64
      ThrowTo: int64 -> int }

let example =
    [ { Id = 0
        NumberInspections = 0
        Items = [ 79; 98 ]
        Operation = (*) 19L
        ThrowTo = fun n -> if n % 23L = 0 then 2 else 3 }
      { Id = 1
        NumberInspections = 0
        Items = [ 54; 65; 75; 74 ]
        Operation = (+) 6L
        ThrowTo = fun n -> if n % 19L = 0 then 2 else 0 }
      { Id = 2
        NumberInspections = 0
        Items = [ 79; 60; 97 ]
        Operation = fun old -> old * old
        ThrowTo = fun n -> if n % 13L = 0 then 1 else 3 }
      { Id = 3
        NumberInspections = 0
        Items = [ 74 ]
        Operation = (+) 3L
        ThrowTo = fun n -> if n % 17L = 0 then 0 else 1 } ]

let input =
    [ { Id = 0
        NumberInspections = 0
        Items = [ 65; 78 ]
        Operation = (*) 3L
        ThrowTo = fun n -> if n % 5L = 0 then 2 else 3 }
      { Id = 1
        NumberInspections = 0
        Items = [ 54; 78; 86; 79; 73; 64; 85; 88 ]
        Operation = (+) 8L
        ThrowTo = fun n -> if n % 11L = 0 then 4 else 7 }
      { Id = 2
        NumberInspections = 0
        Items = [ 69; 97; 77; 88; 87 ]
        Operation = (+) 2L
        ThrowTo = fun n -> if n % 2L = 0 then 5 else 3 }
      { Id = 3
        NumberInspections = 0
        Items = [ 99 ]
        Operation = (+) 4L
        ThrowTo = fun n -> if n % 13L = 0 then 1 else 5 }
      { Id = 4
        NumberInspections = 0
        Items = [ 60; 57; 52 ]
        Operation = (*) 19L
        ThrowTo = fun n -> if n % 7L = 0 then 7 else 6 }
      { Id = 5
        NumberInspections = 0
        Items = [ 91; 82; 85; 73; 84; 53 ]
        Operation = (+) 5L
        ThrowTo = fun n -> if n % 3L = 0 then 4 else 1 }
      { Id = 6
        NumberInspections = 0
        Items = [ 88; 74; 68; 56 ]
        Operation = fun old -> old * old
        ThrowTo = fun n -> if n % 17L = 0 then 0 else 2 }
      { Id = 7
        NumberInspections = 0
        Items = [ 54; 82; 72; 71; 53; 99; 67 ]
        Operation = (+) 1L
        ThrowTo = fun n -> if n % 19L = 0 then 6 else 0 } ]

let throw item target monkeys =
    let monkey = monkeys |> List.item target

    monkeys
    |> List.updateAt target { monkey with Items = (List.append monkey.Items [ item ]) }

let doItem monkey monkeys item =
    //Had to look up a hint, thanks reddit!
    //https://aoc.just2good.co.uk/2022/11
    //let safeMod = [ 17L; 13L; 19L; 23L ] |> List.reduce (*)
    let safeMod =
        [ 5L; 11L; 2L; 13L; 7L; 3L; 17L; 19L ]
        |> List.reduce (*)

    let worryLevel = (monkey.Operation item) % safeMod
    let target = monkey.ThrowTo worryLevel
    throw worryLevel target monkeys

let playTurn monkeys id =
    let monkey = monkeys |> List.item id

    let cleared =
        monkeys
        |> List.updateAt
            id
            { monkey with
                Items = []
                NumberInspections =
                    monkey.NumberInspections
                    + (int64 monkey.Items.Length) }

    monkey.Items |> List.fold (doItem monkey) cleared

let playRound monkeys =
    let players = [ 0 .. (monkeys |> List.length) - 1 ]
    players |> List.fold playTurn monkeys


let rec playRounds nb monkeys =
    if nb = 0 then
        monkeys
    else
        let next = playRound monkeys
        playRounds (nb - 1) next

let solve input =
    input
    |> playRounds 10_000
    |> List.map (fun m -> m.NumberInspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

solve input

let run () =
    printf "Testing..."
    test <@ solve example = 2713310158L @>
    printfn "...done!"

run ()
