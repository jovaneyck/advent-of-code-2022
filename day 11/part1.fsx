#r "nuget: Unquote"
open Swensen.Unquote

type Monkey =
    { Id: int
      NumberInspections: int
      Items: int list
      Operation: int -> int
      ThrowTo: int -> int }

let example =
    [ { Id = 0
        NumberInspections = 0
        Items = [ 79; 98 ]
        Operation = (*) 19
        ThrowTo = fun n -> if n % 23 = 0 then 2 else 3 }
      { Id = 1
        NumberInspections = 0
        Items = [ 54; 65; 75; 74 ]
        Operation = (+) 6
        ThrowTo = fun n -> if n % 19 = 0 then 2 else 0 }
      { Id = 2
        NumberInspections = 0
        Items = [ 79; 60; 97 ]
        Operation = fun old -> old * old
        ThrowTo = fun n -> if n % 13 = 0 then 1 else 3 }
      { Id = 3
        NumberInspections = 0
        Items = [ 74 ]
        Operation = (+) 3
        ThrowTo = fun n -> if n % 17 = 0 then 0 else 1 } ]

let input =
    [ { Id = 0
        NumberInspections = 0
        Items = [ 65; 78 ]
        Operation = (*) 3
        ThrowTo = fun n -> if n % 5 = 0 then 2 else 3 }
      { Id = 1
        NumberInspections = 0
        Items = [ 54; 78; 86; 79; 73; 64; 85; 88 ]
        Operation = (+) 8
        ThrowTo = fun n -> if n % 11 = 0 then 4 else 7 }
      { Id = 2
        NumberInspections = 0
        Items = [ 69; 97; 77; 88; 87 ]
        Operation = (+) 2
        ThrowTo = fun n -> if n % 2 = 0 then 5 else 3 }
      { Id = 3
        NumberInspections = 0
        Items = [ 99 ]
        Operation = (+) 4
        ThrowTo = fun n -> if n % 13 = 0 then 1 else 5 }
      { Id = 4
        NumberInspections = 0
        Items = [ 60; 57; 52 ]
        Operation = (*) 19
        ThrowTo = fun n -> if n % 7 = 0 then 7 else 6 }
      { Id = 5
        NumberInspections = 0
        Items = [ 91; 82; 85; 73; 84; 53 ]
        Operation = (+) 5
        ThrowTo = fun n -> if n % 3 = 0 then 4 else 1 }
      { Id = 6
        NumberInspections = 0
        Items = [ 88; 74; 68; 56 ]
        Operation = fun old -> old * old
        ThrowTo = fun n -> if n % 17 = 0 then 0 else 2 }
      { Id = 7
        NumberInspections = 0
        Items = [ 54; 82; 72; 71; 53; 99; 67 ]
        Operation = (+) 1
        ThrowTo = fun n -> if n % 19 = 0 then 6 else 0 } ]

let throw item target monkeys =
    let monkey = monkeys |> Seq.item target

    monkeys
    |> Seq.updateAt target { monkey with Items = (List.append monkey.Items [ item ]) }

let doItem monkey monkeys item =
    let worryLevel = monkey.Operation item
    let boredLevel = worryLevel / 3
    let target = monkey.ThrowTo boredLevel
    throw boredLevel target monkeys

let playTurn monkeys id =
    let monkey = monkeys |> Seq.item id

    let cleared =
        monkeys
        |> Seq.updateAt
            id
            { monkey with
                Items = []
                NumberInspections = monkey.NumberInspections + monkey.Items.Length }

    monkey.Items |> List.fold (doItem monkey) cleared

let playRound monkeys =
    let players = [ 0 .. (monkeys |> Seq.length) - 1 ]
    players |> List.fold playTurn monkeys


let rec playRounds nb monkeys =
    if nb = 0 then
        monkeys
    else
        let next = playRound monkeys
        playRounds (nb - 1) next

let solve input =
    input
    |> playRounds 20
    |> Seq.map (fun m -> m.NumberInspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

solve input

let run () =
    printf "Testing..."
    test <@ solve example = 10605 @>
    printfn "...done!"

run ()
