let input =
    System.IO.File.ReadAllText $"""{__SOURCE_DIRECTORY__}\multiline_file.txt"""

let example =
    """first line

second line"""

(*works as expected*)
example.Split("\n\n")

(*this does not work as expected on a windows machine*)
input.Split("\n\n")

(*this works as expected on a windows machine*)
input.Split("\r\n\r\n")

(*this works as expected on all machines*)
input.Split($"{System.Environment.NewLine}{System.Environment.NewLine}")
