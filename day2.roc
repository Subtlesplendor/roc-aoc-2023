app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{Task}, "input/day-2-input.txt" as input : Str]
    provides [main] to pf

lines : Str -> List Str
lines = \str -> str |> Str.split "\n"

main : Task {} *
main = 
    {} <- "Part 1: \(part1)" |> Stdout.line |> Task.await
    {} <- "Part 2: \(part2)" |> Stdout.line |> Task.await
    Task.ok {}

part1 = solve input

solve = \str ->
    str 
        |> parse
        |> List.keepIf \(_, b) -> b
        |> List.map \(val, _) -> val
        |> List.sum
        |> Num.toStr

parse = \str ->
    str
        |> lines
        |> List.map parseLine

parseLine = \line ->
    line 
        |> Str.replaceFirst "Game " ""
        |> Str.split ":"
        |> \lst ->
            when lst is 
                [num, drawsStr] ->
                    gameId = num |> Str.toU32 |> Result.withDefault 0
                    possible = 
                        drawsStr 
                            |> Str.split ";" 
                            |> List.map parseDraw
                            |> isPossibleGame
                            |> List.isEmpty
                    (gameId, possible)  
                _ -> (0, Bool.false)
                          

isPossibleGame = \drawList ->
    drawList |> List.keepErrs isPossibleDraw

isPossibleDraw = \draw ->
    isPossibleWithConstraint draw constraintPart1

isPossibleWithConstraint = \draw, constraint ->
    when constraint draw is
        Possible -> Ok draw
        Impossible -> Err "Impossible game"

constraintPart1 = \draw ->
    when draw is 
        {red, green, blue} if red <= 12 && green <= 13 && blue <= 14 -> Possible
        _ -> Impossible
    
    

parseDraw = \str ->
    str 
        |> Str.split ","
        |> List.map Str.trim
        |> List.map (\s -> Str.split s " ")
        |> List.map \lst ->
            when lst is
            [num, color] -> 
                number = num |> Str.toU32 |> Result.withDefault 0
                (number, color)
            _ -> (0, "red")
        |> List.walk emptyDraw \state, (num, color) ->
                when color is
                    "red" -> {state & red: state.red + num}
                    "green" -> {state & green: state.green + num}
                    "blue" -> {state & blue: state.blue + num}
                    _ -> state

emptyDraw = {red: 0, green: 0, blue: 0}

part2 = solve2 input

solve2 = \str ->
    str 
        |> parse2
        |> List.map power
        |> List.sum
        |> Num.toStr

power = \{red, green, blue} -> red * green * blue        

parse2 = \str ->
    str
        |> lines
        |> List.map parseLine2       

parseLine2 = \line ->
    line 
        |> Str.replaceFirst "Game " ""
        |> Str.split ":"
        |> \lst ->
            when lst is 
                [_, drawsStr] ->
                    drawsStr 
                        |> Str.split ";" 
                        |> List.map parseDraw
                        |> minimumContent
                _ -> emptyDraw

minimumContent = \drawList ->
    drawList |> List.walk emptyDraw \state, draw ->
        {
            red: Num.max state.red draw.red,
            green: Num.max state.green draw.green,
            blue: Num.max state.blue draw.blue
        }

expect
    expected = {green : 8, blue: 6, red: 20}
    parsed = parseDraw exampleDraw
    parsed == expected

expect
    expected = Err "Impossible game"
    possible = parseDraw exampleDraw |> isPossibleDraw
    possible == expected    

expect
    expected = [(1, Bool.true), (2, Bool.true), (3, Bool.false), (4, Bool.false), (5, Bool.true)]
    parsed = parse example
    parsed == expected

expect
    expected = "8"
    solved = solve example 
    solved == expected

example = 
    """
    Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    """

exampleDraw = "8 green, 6 blue, 20 red"


expect 
    expectedMinimum = [
        {red: 4, green: 2, blue: 6},
        {red: 1, green: 3, blue: 4},
        {red: 20, green: 13, blue: 6},
        {red: 14, green: 3, blue: 15},
        {red: 6, green: 3, blue: 2},
    ]
    actualMinimum = parse2 example
    expectedMinimum == actualMinimum

expect
    expectedSolve = "2286"
    actualSolve = solve2 example
    expectedSolve == actualSolve