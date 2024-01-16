// Advent of Code 2023 - Day 21 - Step Counter.
// F#.

open AoC2023

[<EntryPoint>]
let main argv =

    // Create a new garden.    
    let garden =
        "Day21Input.txt"
        |> Garden.create

    // Get start coord.    
    let start =
        garden
        |> Garden.getStartCoord
        |> fun sc -> [sc,0]    
    
    // Get visited plots.
    let noVisitedPlots =
        garden
        |> Garden.walk 64 start
        |> Garden.getNoVisitedPlots

    garden |> Garden.display

    printfn "Part 1 answer: %d" noVisitedPlots
    0