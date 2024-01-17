// Advent of Code 2023 - Day 21 - Step Counter.
// F#.

open AoC2023

[<EntryPoint>]
let main argv =

    let inputFile = "Day21Input.txt"

    // Get regular garden.
    let garden =
        inputFile
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

    // Part 1 - not too bad.
    printfn "Part 1 answer: %d" noVisitedPlots

    // Part 2 - nightmare. Had to look up Reddit hints for this one, settled on
    // quadratic formula approach in the end.
    // Note: this takes a few mins to run, using a 2D array is not ideal.

    // Get big garden.
    let getBigGarden n =
        inputFile
        |> Garden.createBig n

    // Get new start coord.    
    let getBigGardenStart n =
        getBigGarden n
        |> Garden.getStartCoord
        |> fun sc -> [sc,0]    

    // Get a, b, c and x for quadratic.
    let targetSteps = 26_501_365
    let size = garden |> Garden.getNoRows
    let edge = size/2

    let start' = getBigGardenStart 5
    let pointsToInterpolate =
        [for n in [0..2] -> edge + n*size]
        |> List.map (fun p ->
            getBigGarden 7 
            |> Garden.walk p start'
            |> Garden.getNoVisitedPlots)

    // Work out quadratic.
    let a = int64 (pointsToInterpolate.[2] - 2*pointsToInterpolate.[1] + pointsToInterpolate.[0])/2L
    let b = int64 pointsToInterpolate.[1] - int64 pointsToInterpolate.[0] - a
    let c = int64 pointsToInterpolate.[0]
    let x = int64 ((targetSteps-edge)/size)
    let noVisitedPlots' = (a*x*x) + (b*x) + c

    printfn "Part 2 answer: %d" noVisitedPlots'    
    0