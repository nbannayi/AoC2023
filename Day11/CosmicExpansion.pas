(* Advent of Code 2023, Day 11 - Cosmic Expansion. *)
program CosmicExpansion;
uses
    Math;
type
    TStringArray = array of string;
    TIntegerArray = array of integer;
    TCoord = record
        R: Integer;
        C: Integer;
    end;
    TCoordArray = array of TCoord;
    TCoordPair = record
        Coord1: TCoord;
        Coord2: TCoord;
    end;
    TCoordPairArray = array of TCoordPair;

(* Display all passed galaxies. *)
procedure DisplayGalaxies(galaxies: TStringArray);
var
    i: integer;
begin
    for i := 0 to High(galaxies) do
    begin
        writeln(galaxies[i]);
    end;
end;

(* Get all galaxy pairs in expanding universe. *)
function GetGalaxyCoordPairs(galaxies: TCoordArray): TCoordPairArray;
var
    i, j: integer;
    result: TCoordPairArray;
    pair: TCoordPair;
begin
    SetLength(result, 0);
    for i := 0 to High(galaxies)-1 do
    begin
        for j := i+1 to High(galaxies) do
        begin
            SetLength(result, Length(result) + 1);
            pair.Coord1 := galaxies[i];
            pair.Coord2 := galaxies[j];
            result[High(result)] := pair;
        end;
    end;
    GetGalaxyCoordPairs := result;
end;

(* Get distance between galaxies *)
function GetDistanceBetweenGalaxies(
    galaxy1: TCoord; 
    galaxy2: TCoord; 
    expandingRows: TIntegerArray; 
    expandingColumns: TIntegerArray;
    expansionFactor: int64): int64;
var
    rdist, cdist, rexpansion, cexpansion, r, c: int64;
begin    
    rexpansion := 0;
    for r := 0 to Length(expandingRows)-1 do
    begin
        if (expandingRows[r] >= Min(galaxy1.R, galaxy2.R)) and (expandingRows[r] <= Max(galaxy1.R, galaxy2.R)) then
        begin
            rexpansion := rexpansion + expansionFactor;
        end;
    end;
    cexpansion := 0;
    for c := 0 to Length(expandingColumns)-1 do
    begin
        if (expandingColumns[c] >= Min(galaxy1.C, galaxy2.C)) and (expandingColumns[c] <= Max(galaxy1.C, galaxy2.C)) then
        begin
            cexpansion := cexpansion + expansionFactor;
        end;
    end;
    rdist := Abs(galaxy2.R - galaxy1.R);
    cdist := Abs(galaxy2.C - galaxy1.C);
    GetDistanceBetweenGalaxies := rdist + cdist + rexpansion + cexpansion;
end;

(* Get all galaxy coords (unexpanded) *)
function GetGalaxyCoords(galaxies: TStringArray): TCoordArray;
var
    r, c, rowLength: integer;
    result: TCoordArray;
    coord: TCoord;
begin
    SetLength(result, 0);
    rowLength := Length(galaxies[0]);
    for r := 0 to High(galaxies) do
    begin
        for c := 0 to rowLength do
        begin
            if galaxies[r][c] = '#' then
            begin
                coord.r := r;
                coord.c := c-1;
                SetLength(result, Length(result) + 1);
                result[High(result)] := coord;
            end;
        end;
    end;
    GetGalaxyCoords := result;
end;

(* Return array containing all expanding rows *)
function GetExpandingRows(galaxies: TStringArray): TIntegerArray;
var
    r: integer;
    result: TIntegerArray;
begin
    SetLength(result, 0);
    for r := 0 to High(galaxies) do
    begin
        if Pos('#', galaxies[r]) = 0 then
        begin
            SetLength(result, Length(result) + 1);
            result[High(result)] := r;
        end;
    end;
    GetExpandingRows := result;
end;

(* Return array containing all expanding cols *)
function GetExpandingColumns(galaxies: TStringArray): TIntegerArray;
var
    r, c, rowLength: integer;
    result: TIntegerArray;
    found: Boolean;
begin
    SetLength(result, 0);
    rowLength := Length(galaxies[0]);
    for c := 1 to rowLength do
    begin
        found := False;
        for r := 0 to High(galaxies) do
        begin
            if galaxies[r][c] = '#' then
            begin
                found := True;
                break;
            end;            
        end;
        if found = False then
        begin
            SetLength(result, Length(result) + 1);
            result[High(result)] := c-1;            
        end;
    end;
    GetExpandingColumns := result;
end;

(* Parse input and return array of galaxies. *)
function ParseInput(const fileName: string): TStringArray;
var
    inputFile: text;  
    inputLine: string;
    result: TStringArray;
begin    
    assign(inputFile, fileName);
    SetLength(result, 0);
    reset(inputFile);
    while not eof(inputFile) do
    begin
        readln(inputFile, inputLine);        
        SetLength(result, Length(result) + 1);
        result[High(result)] := inputLine;
    end;
    close(inputFile);
    ParseInput := result;
end;

(* Put it all together to get sum of shortest paths for all pairs of glaxies in the expanded universe *)
function GetSumShortestPaths(galaxies: TStringArray; expansionFactor: int64): int64;
var
    i: int64;
    total: int64;
    expandingRows, expandingColumns: TIntegerArray;
    galaxyCoords: TCoordArray;
    coord1, coord2: TCoord;
    galaxyCoordPairs: TCoordPairArray;
begin
    (* Get expanding rows and columns *)
    expandingRows := GetExpandingRows(galaxies);
    expandingColumns := GetExpandingColumns(galaxies);

    (* Get galaxy coords *)
    galaxyCoords := GetGalaxyCoords(galaxies);

    (* Get all galaxy pairs *)
    galaxyCoordPairs := GetGalaxyCoordPairs(galaxyCoords);

    (* Loop through all pairs, work out distance and add to total *)
    total := 0;
    for i := 0 to High(galaxyCoordPairs) do
    begin
        coord1 := galaxyCoordPairs[i].Coord1;
        coord2 := galaxyCoordPairs[i].Coord2;
        total := total + GetDistanceBetweenGalaxies(coord1, coord2, expandingRows, expandingColumns, expansionFactor);
    end;

    GetSumShortestPaths := total;
end;

(* Main program starts here. *)
var
    fileName: string;
    galaxies: TStringArray;
    total: int64;
begin
    (* Parse galaxies input. *)
    fileName := 'Day11Input.txt';
    galaxies := ParseInput(fileName);    

    (* Part 1 *)
    total := GetSumShortestPaths(galaxies, 1);
    writeln('Part 1 answer: ',total);

    (* Part 2 *)
    total := GetSumShortestPaths(galaxies, 1000000-1);
    writeln('Part 2 answer: ',total);
end.