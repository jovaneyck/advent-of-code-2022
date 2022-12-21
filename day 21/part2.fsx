#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"""{__SOURCE_DIRECTORY__}\input.txt"""
    |> List.ofSeq

let example =
    """root: pppw + sjmn
    dbpl: 5
    cczh: sllz + lgvd
    zczc: 2
    ptdq: humn - dvpt
    dvpt: 3
    lfqf: 4
    humn: 5
    ljgn: 2
    sjmn: drzm * dbpl
    sllz: 4
    pppw: cczh / lfqf
    lgvd: ljgn * ptdq
    drzm: hmdt - zczc
    hmdt: 32"""
        .Split("\n")
    |> Array.map (fun s -> s.Trim())
    |> List.ofSeq

type Monkey = string

type Operation =
    | Add
    | Subtract
    | Multiply
    | Divide

type Job =
    | Number of int64
    | Calculation of (Monkey * Operation * Monkey)

let (|ANumber|_|) (job: string) =
    match System.Int64.TryParse(job) with
    | true, n -> Number n |> Some
    | false, _ -> None

let (|ACalculation|_|) (job: string) =
    let [| l; op; r |] = job.Split(" ")

    let operation =
        match op with
        | "+" -> Add
        | "-" -> Subtract
        | "*" -> Multiply
        | "/" -> Divide

    Calculation(l, operation, r) |> Some

let parse (line: string) =
    let [| monkey; job |] = line.Split(": ")

    let j =
        match job with
        | ANumber n -> n
        | ACalculation c -> c

    (monkey, j)

let apply l op r =
    match op with
    | Add -> l + r
    | Subtract -> l - r
    | Multiply -> l * r
    | Divide -> l / r

let rec perform jobs (Calculation (l, op, r)) =
    let left = calculate l jobs
    let right = calculate r jobs
    apply left op right

and calculate monkey jobs =
    let j = jobs |> Map.find monkey

    match j with
    | Number n -> n
    | Calculation c -> perform jobs (Calculation c)

let simulate jobs shout =
    let origRoot = jobs |> Map.find "root"
    let (Calculation (l, _, r)) = origRoot

    let patched =
        jobs
        |> Map.add "humn" (Number shout)
        |> Map.remove "root"

    let leftN = calculate l patched
    let rightN = calculate r patched
    //printfn "%A" shout
    printfn "%A" leftN
    printfn "%A" rightN
    leftN = rightN

#time

let jobs = input |> List.map parse |> Map.ofList

//Approach: noticed that there was a sequence, numbers were monotonic decreasing when shout increased
// bit fiddling/binary search on a reasonable input seed.
let shouts = Seq.initInfinite (fun i -> 3715799488_000L + (int64 i))

shouts
|> Seq.find (fun shout -> simulate jobs shout)

let run () =
    printf "Testing..."
    test <@ parse "dbpl: 5" = ("dbpl", Number 5L) @>
    test <@ parse "root: pppw + sjmn" = ("root", Calculation("pppw", Add, "sjmn")) @>
    test <@ parse "ptdq: humn - dvpt" = ("ptdq", Calculation("humn", Subtract, "dvpt")) @>
    printfn "...done!"

run ()
