namespace AoC2023

open System.IO

/// Garden for the elves.
type Garden =
    {
        Plots: char [,]        
    }

/// Function to act on Garden type.
module Garden =

    /// Create a return new garden type.
    let create inputFile =
        let lines = inputFile |> File.ReadLines |> Array.ofSeq                
        let plots = Array2D.create (lines.Length) (lines.[0].Length) ' '
        for row in [0..lines.Length-1] do
            for col in [0..lines.[0].Length-1] do
                plots.[row,col] <- lines.[row].[col]
        {
            Plots = plots                
        }

    /// Get no rows in garden.
    let getNoRows garden =
        garden.Plots
        |> Array2D.length1

    /// Get no cols in garden.
    let getNoCols garden =
        garden.Plots
        |> Array2D.length2
        
    /// Get contents of a specific plot.
    let getPlot (row,col) garden =
        garden.Plots.[row,col]

    /// Set contents of a specific plot.
    let setPlot (row,col) c garden =
        garden.Plots.[row,col] <- c

    /// Get start.
    let getStartCoord garden =
        let noRows, noCols = garden |> getNoRows, garden |> getNoCols
        seq {for row in [0..noRows-1] do
                for col in [0..noCols-1] ->
                    if garden |> getPlot (row,col) = 'S' then Some (row,col) else None}
        |> Seq.skipWhile (fun c -> c |> Option.isNone)
        |> Seq.head
        |> function
           | Some c -> c
           | None -> failwith "No start coord found."

    /// Get all visitable neighbours from a given coordinate.
    let getNeighbours coords garden =
        let row,col = coords
        let maxRows,maxCols = (garden |> getNoRows), (garden |> getNoCols)
        [row-1,col; row+1,col; row,col-1; row,col+1]
        |> List.filter (fun (r,c) -> r >= 0 && c >= 0 && r < maxRows && c < maxCols)
        |> List.filter (fun c -> ['O';'#';','] |> List.contains (garden |> getPlot c) |> not)

    /// Recursively walk around a bit.
    let rec walk maxSteps coords garden =
        match coords |> List.length with
        | 0 -> garden
        | _ -> 
            match coords with
            | head::tail ->
                let (coord,index) = head
                if (garden |> getPlot coord) = 'O' then
                    garden |> walk maxSteps tail
                else                    
                    if index % 2 = 0 then garden |> setPlot coord 'O'
                    let neighbours = garden |> getNeighbours coord
                    if index < maxSteps then
                        let newNeighbours = tail @ List.zip neighbours (List.init (neighbours.Length) (fun _ -> index+1))
                        garden |> walk maxSteps newNeighbours
                    else
                        garden |> walk maxSteps tail
            | [] -> garden        

    /// Count all the plots visited.
    let getNoVisitedPlots garden =
        let noRows, noCols = garden |> getNoRows, garden |> getNoCols
        [for row in [0..noRows-1] do
            for col in [0..noCols-1] ->
                if garden |> getPlot (row,col) = 'O' then 1 else 0]
        |> List.sum

    /// Display a garden.
    let display garden =
        let noRows, noCols = garden |> getNoRows, garden |> getNoCols
        for row in [0..noRows-1] do
            printf "%02d " row
            for col in [0..noCols-1] do
                let plotContents = garden |> getPlot (row,col)
                printf "%c" plotContents
            printfn "" 