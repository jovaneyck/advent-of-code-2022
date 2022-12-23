#r "nuget: Unquote"
open Swensen.Unquote

let input = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\input.txt"""
let example = System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\example.txt"""

type Command =
    | Forward of int
    | Left
    | Right

let parseSequence s =
    let rec parseSeq acc s =
        match s with
        | [] -> acc |> List.rev
        | h :: t ->
            let digits = s |> List.takeWhile System.Char.IsAsciiDigit

            match digits with
            | [] ->
                let command =
                    match h with
                    | 'L' -> Left
                    | 'R' -> Right

                parseSeq (command :: acc) t
            | _ ->
                let rest = s |> List.skip (digits |> List.length)
                let rawnumber = digits |> List.map string |> String.concat ""
                let forward = int rawnumber |> Forward
                parseSeq (forward :: acc) rest

    parseSeq [] s

type Tile =
    | Open
    | Wall

//x = row index, y = col index
type World = (Tile option) [,]

type Direction =
    | Rightward
    | Downward
    | Leftward
    | Upward

type State =
    { Location: (int * int)
      Facing: Direction }

let parseWorld (world: string) : World =
    let rows = world.Split($"{System.Environment.NewLine}")
    let width = rows |> Seq.map Seq.length |> Seq.max

    [ for row in rows ->
          [ for colIdx in [ 0 .. (width - 1) ] ->
                let col =
                    row
                    |> Seq.tryItem colIdx
                    |> Option.defaultValue ' '

                match col with
                | ' ' -> None
                | '.' -> Some Open
                | '#' -> Some Wall ] ]
    |> array2D

let print (world: World) =
    [ for row in
          [ 0 .. ((world |> Array2D.length1) - 1) ]
          |> List.map (fun idx -> world[idx, *]) ->

          [ for c in row ->
                let el =
                    match c with
                    | None -> ' '
                    | Some Wall -> '#'
                    | Some Open -> '.'

                printf "%c" el ]
          |> ignore

          printfn "" ]
    |> ignore

let [| rawworld; rawsequence |] =
    example.Split($"{System.Environment.NewLine}{System.Environment.NewLine}")

let sequence = parseSequence (rawsequence |> List.ofSeq)
let world = parseWorld rawworld

let initialState (world: World) : State =
    let col =
        world[0, *]
        |> Array.indexed
        |> Array.pick (fun (col, tile) ->
            match tile with
            | Some Open -> Some col
            | _ -> None)

    { Location = (0, col)
      Facing = Rightward }

let turnLeft =
    function
    | Upward -> Leftward
    | Leftward -> Downward
    | Downward -> Rightward
    | Rightward -> Upward

let turnRight =
    function
    | Upward -> Rightward
    | Rightward -> Downward
    | Downward -> Leftward
    | Leftward -> Upward

let rec moveForward world f state = failwith "I HIT A MENTAL WALL"

let apply world state command =
    match command with
    | Left -> { state with Facing = turnLeft state.Facing }
    | Right -> { state with Facing = turnRight state.Facing }
    | Forward f -> moveForward world f state

sequence
|> List.fold (apply world) (initialState world)

print world

let run () =
    printf "Testing..."

    test
        <@ parseSequence (rawsequence |> List.ofSeq) = [ Forward 10
                                                         Right
                                                         Forward 5
                                                         Left
                                                         Forward 5
                                                         Right
                                                         Forward 10
                                                         Left
                                                         Forward 4
                                                         Right
                                                         Forward 5
                                                         Left
                                                         Forward 5 ] @>

    printfn "...done!"

run ()
