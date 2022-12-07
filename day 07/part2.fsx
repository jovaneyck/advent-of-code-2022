#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

type Command =
    | CD of string
    | LS

type FileInfo = int64 * string


type Output =
    | Dir of string
    | File of FileInfo

type Line =
    | Command of Command
    | Output of Output

let example =
    """$ cd /
    $ ls
    dir a
    14848514 b.txt
    8504156 c.dat
    dir d
    $ cd a
    $ ls
    dir e
    29116 f
    2557 g
    62596 h.lst
    $ cd e
    $ ls
    584 i
    $ cd ..
    $ cd ..
    $ cd d
    $ ls
    4060174 j
    8033020 d.log
    5626152 d.ext
    7214296 k"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

let (|IsCommand|_|) (line: string) =
    match line |> List.ofSeq with
    | '$' :: ' ' :: 'c' :: 'd' :: ' ' :: dir -> Some(CD(dir |> Seq.map string |> String.concat ""))
    | '$' :: ' ' :: 'l' :: 's' :: [] -> Some(LS)
    | _ -> None

let (|IsOutput|_|) (line: string) =
    match line |> List.ofSeq with
    | 'd' :: 'i' :: 'r' :: ' ' :: dir -> Some(Dir(dir |> Seq.map string |> String.concat ""))
    | _ ->
        let [| size; filename |] = line.Split(" ")
        Some(File(int64 size, filename))

let parse (line: string) : Line =
    match line with
    | IsCommand c -> Command c
    | IsOutput o -> Output o

type State =
    { CurrentPath: string list
      Tree: Map<string list, Output list> }

let initial = { CurrentPath = []; Tree = Map.empty }

let insert tree path node =
    let atPath = tree |> Map.tryFind path

    let updated =
        match atPath with
        | None -> [ node ]
        | Some things -> node :: things

    tree |> Map.add path updated

let processLine state line =
    match line with
    | Command (CD "..") -> { state with CurrentPath = state.CurrentPath |> List.skip 1 }
    | Command (CD dir) -> { state with CurrentPath = dir :: state.CurrentPath }
    | Output o -> { state with Tree = insert state.Tree state.CurrentPath o }
    | _ -> state

let dict = new System.Collections.Generic.Dictionary<string list, int64>()

let rec calculateSize tree path =
    let exists, size = dict.TryGetValue path

    match exists with
    | true -> size
    | false ->
        let atPath = tree |> Map.find path
        let s = atPath |> List.sumBy (sizeOf tree path)
        dict.TryAdd(path, s) |> ignore
        s

and sizeOf tree path node =
    match node with
    | File (s, _) -> s
    | Dir (name) ->
        let path = name :: path

        let exists, size = dict.TryGetValue path

        match exists with
        | true -> size
        | false ->
            let s = calculateSize tree path
            dict.TryAdd(path, s) |> ignore
            s

let solve input =
    let lines = input |> List.map parse

    let tree = (lines |> List.fold processLine initial).Tree
    let paths = tree |> Map.keys |> Seq.toList

    dict.Clear()

    let sizes =
        paths
        |> List.map (fun dir -> dir, dir |> calculateSize tree)

    sizes
    |> List.filter (fun (_, s) -> s <= 100000L)
    |> Seq.sumBy snd

#time
//solve input

let run () =
    printf "Testing..."
    test <@ parse "$ cd efg" = Command(CD "efg") @>
    test <@ parse "$ ls" = Command LS @>
    test <@ parse "dir e" = Output(Dir "e") @>
    test <@ parse "14848514 b.txt" = Output(File(14848514L, "b.txt")) @>
    test <@ solve example = 95437L @>
    printfn "...done!"

run ()
