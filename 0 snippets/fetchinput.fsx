#r "nuget: FSharp.Data"

open FSharp.Data
open System.IO

let session =
    "<your session ID goes here, you can find your session ID by using your web browser dev tools when logged into adventofcode.com, look at the request headers>cookie>session=<session id>"

let day = 1
let year = 2022

let input =
    Http.RequestString($"https://adventofcode.com/{year}/day/{day}/input", cookies = [ "session", session ])

File.WriteAllText($@"{__SOURCE_DIRECTORY__}\input{day}", input)
