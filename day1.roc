app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{Task}, "input/day-1-input.txt" as input : Str]
    provides [main] to pf

lines : Str -> List Str
lines = \str -> str |> Str.split "\n"

main : Task {} *
main = 
    {} <- "Part 1: \(part1)" |> Stdout.line |> Task.await
    {} <- "Part 2: \(part2)" |> Stdout.line |> Task.await
    Task.ok {}

part1 : Str
part1 =
    calibrationValues
    |> List.sum
    |> Num.toStr

calibrationValues = parse input

parse = \str -> 
    str 
        |> lines
        |> List.map parseLine
        |> List.keepOks \x -> x

parseLine = \line ->
    line 
        |> Str.graphemes
        |> List.keepOks Str.toU32
        |> \lst ->
            when (List.first lst, List.last lst) is
                (Ok dec, Ok num) -> dec * 10 + num |> Ok
                _ -> Err "No numbers"

part2 : Str
part2 =
    calibrationValues2
        |> List.sum
        |> Num.toStr

calibrationValues2 = parse2 input

parse2 = \str -> 
    str 
        |> lines
        |> List.map parseLine2
        |> List.keepOks \x -> x

parseLine2 = \line ->
    line 
        |> replaceDigits
        |> Str.graphemes
        |> List.keepOks Str.toU32
        |> \lst ->
            when (List.first lst, List.last lst) is
                (Ok dec, Ok num) -> dec * 10 + num |> Ok
                _ -> Err "No numbers"
       
replaceDigits = \line ->
    digits |> List.walk line \state, dig ->
        newDig = dig |> toInt
        state |> Str.replaceEach dig newDig        

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

toInt = \str ->
    when str is
    "one" -> "one1one"
    "two" -> "two2two"
    "three" -> "three3three"
    "four" -> "four4four"
    "five" -> "five5five"
    "six" -> "six6six"
    "seven" -> "seven7seven"
    "eight" -> "eight8eight"
    "nine" -> "nine9nine"
    _ -> ""


expect
    expected = [12, 38, 15, 77]
    actual = parse example
    actual == expected

expect 
    parseLine "treb7uchet" == Ok 77

expect
    expected = [29, 83, 13, 24, 42, 14, 76]
    actual = parse2 example2    
    actual == expected

example : Str
example =
    """
    1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet
    """

example2 = 
    """
    two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen
    """