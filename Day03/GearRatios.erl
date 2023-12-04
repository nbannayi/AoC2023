-module(gearratios).
-export([start/0]).

start() -> 
    % Input file and read into Schematic list. 
    {ok, File} = file:open("Day03Input.txt",[read]),
    {ok, Lines} = file:read(File,1024 * 1024),
    Schematic = string:tokens(Lines, "\n"),            
    %%% Part 1 %%%
    AllValidItems = get_all_valid_items([], Schematic, 1, length(Schematic)+1),
    TotalValidItems = sum(fun(X, Acc) -> X+Acc end, AllValidItems, 0),
    io:fwrite("Part 1 answer: ~p~n", [TotalValidItems]),
    %%% Part 2 %%%
    GearCoords = get_all_gear_coords([],Schematic, 1, length(Schematic)+1),
    GearItemRows = get_all_gear_item_rows([],Schematic, GearCoords, 1, length(GearCoords)+1),
    GearItems = get_all_gear_items([], Schematic, GearItemRows, 1, length(GearItemRows)+1),
    GearPowers = get_powers([], GearItems, 1, length(GearItems)+1),
    TotalGearPowers = sum(fun(X, Acc) -> X+Acc end, GearPowers, 0),
    io:fwrite("Part 1 answer: ~p~n", [TotalGearPowers]),    
    ok.

% Get the Schematic character at 1-indexed position (Row,Col)
get_schematic_item(Schematic, Row, Col) ->
    NoRows = length(Schematic),
    RowLength = get_row_Length(Schematic),
    if 
        (Row < 1) or (Row > NoRows) or (Col < 1) or (Col > RowLength) ->
            ".";
        true ->
            RowLine = lists:nth(Row, Schematic),
            string:slice(RowLine, Col-1, 1)
    end.

% Gets the length of each line in schematic.
get_row_Length(Schematic) ->
    RowLine = lists:nth(1, Schematic),
    string:length(RowLine).

% Determine if a schematic item is a special character.
is_special_character(Item) ->
    ItemPos = string:str("0123456789.", Item),
    if
        ItemPos > 0 ->
            false;
        true ->
            true
    end.

% Determine if a schematic item is a number.
is_a_number(Item) ->
    ItemPos = string:str("0123456789", Item),
    if
        ItemPos > 0 ->
            true;
        true ->
            false
    end.

% This function returns pairs of integers in a list where the items are.
get_item_locations(Output, _, _, StopIndex, StopIndex) -> 
    OutputLength = length(Output),
    if 
        OutputLength rem 2 == 1 ->
            lists:concat([Output, [StopIndex-1]]);
        true ->
            Output
    end;    
get_item_locations(Output, Schematic, Row, CurrentIndex, StopIndex) ->    
    Item = get_schematic_item(Schematic, Row, CurrentIndex),
    PrevItem = 
        if 
            CurrentIndex > 1 ->
                get_schematic_item(Schematic, Row, CurrentIndex-1);
            true ->
                "."
        end,
    ItemIsNumber = is_a_number(Item),
    PrevItemIsNumber = is_a_number(PrevItem), 
    IndexLeft = 
        if
            (ItemIsNumber and not PrevItemIsNumber) ->
                CurrentIndex;
            true ->
                0
        end,
    IndexRight =
        if 
            (not ItemIsNumber and PrevItemIsNumber) ->
                CurrentIndex-1;
            true ->
                IndexLeft                
        end,        
    NewOutput = 
        if 
            IndexRight > 0 ->
                lists:concat([Output, [IndexRight]]);
            true ->
                Output
        end,    
    get_item_locations(NewOutput, Schematic, Row, CurrentIndex+1, StopIndex).

% Gets the number associated with a row and left and right coordinates.
get_item_by_coords(Output, Schematic, Row, StopIndex, StopIndex) ->
    Item = get_schematic_item(Schematic, Row, StopIndex),
    list_to_integer(string:concat(Output, Item));
get_item_by_coords(Output, Schematic, Row, CurrentIndex, StopIndex) ->
    Item = get_schematic_item(Schematic, Row, CurrentIndex),
    NewOutput = string:concat(Output, Item),
    get_item_by_coords(NewOutput, Schematic, Row, CurrentIndex+1, StopIndex).

% Check all surrounding squares to determine if a specific square is valid.
is_item_square_valid(Schematic, Row, Col) ->
    TopLeft = is_special_character(get_schematic_item(Schematic, Row-1, Col-1)),
    TopMiddle = is_special_character(get_schematic_item(Schematic, Row-1, Col)),
    TopRight = is_special_character(get_schematic_item(Schematic, Row-1, Col+1)),
    MiddleLeft = is_special_character(get_schematic_item(Schematic, Row, Col-1)),
    MiddleRight = is_special_character(get_schematic_item(Schematic, Row, Col+1)),
    BottomLeft = is_special_character(get_schematic_item(Schematic, Row+1, Col-1)),
    BottomMiddle = is_special_character(get_schematic_item(Schematic, Row+1, Col)),
    BottomRight = is_special_character(get_schematic_item(Schematic, Row+1, Col+1)),
    TopLeft or TopMiddle or TopRight or MiddleLeft or MiddleRight or BottomLeft or BottomMiddle or BottomRight.

% Determine if a number associated with a row and left and right coordimates is valid.
is_item_valid_by_coords(Output, Schematic, Row, StopIndex, StopIndex) ->
    ItemSquareValid = is_item_square_valid(Schematic, Row, StopIndex),
    NewOutput = lists:concat([Output, [ItemSquareValid]]),
    lists:any((fun(E) -> E == true end), NewOutput);
is_item_valid_by_coords(Output, Schematic, Row, CurrentIndex, StopIndex) ->
    ItemSquareValid = is_item_square_valid(Schematic, Row, CurrentIndex),
    NewOutput = lists:concat([Output, [ItemSquareValid]]),
    is_item_valid_by_coords(NewOutput, Schematic, Row, CurrentIndex+1, StopIndex).

% Get all valid items in given row.
get_valid_items_by_row(Output, _, _, _, StopIndex, StopIndex) ->
    Output;
get_valid_items_by_row(Output, Schematic, Row, RowList, CurrentIndex, StopIndex) ->
    ItemLeft = lists:nth(CurrentIndex,RowList),
    ItemRight = lists:nth(CurrentIndex+1,RowList),
    ItemValid = is_item_valid_by_coords([], Schematic, Row, ItemLeft, ItemRight),
    NewOutput = 
        if
            ItemValid ->
                Item = get_item_by_coords("", Schematic, Row, ItemLeft, ItemRight),
                lists:concat([Output,[Item]]);
            true ->
                Output
        end,
    get_valid_items_by_row(NewOutput, Schematic, Row, RowList, CurrentIndex+2, StopIndex).

% Get all valid items in Schematic.
get_all_valid_items(Output, _, StopIndex, StopIndex) ->
    Output;
get_all_valid_items(Output, Schematic, CurrentIndex, StopIndex) ->
    ItemLocations = get_item_locations([],Schematic, CurrentIndex,1,get_row_Length(Schematic)+1),
    ValidItems = get_valid_items_by_row([], Schematic, CurrentIndex, ItemLocations, 1, length(ItemLocations)+1),
    NewOutput = lists:concat([Output,ValidItems]),
    get_all_valid_items(NewOutput, Schematic, CurrentIndex+1, StopIndex).

% Get the posiitons of all stars (gears) in a row.
get_gear_positions(Output, _, _, StopIndex, StopIndex) ->
    Output;
get_gear_positions(Output, Schematic, Row, CurrentIndex, StopIndex) ->    
    IsGear = is_gear(Schematic, Row, CurrentIndex),
    NewOutput =
        if             
            IsGear ->
                lists:concat([Output, [Row,CurrentIndex]]);
            true ->
                Output
        end,
    get_gear_positions(NewOutput, Schematic, Row, CurrentIndex+1, StopIndex).

% Get the coordinates of all stars (gears) in schematic - row, col pairs.
get_all_gear_coords(Output, _, StopIndex, StopIndex) ->
    Output;
get_all_gear_coords(Output, Schematic, CurrentIndex, StopIndex) ->
    GearPositions = get_gear_positions([],Schematic, CurrentIndex, 1, get_row_Length(Schematic)+1),    
    NewOutput = lists:concat([Output, GearPositions]),
    get_all_gear_coords(NewOutput, Schematic, CurrentIndex+1, StopIndex).

% Return true if star is a gear, false otherwise.
is_gear(Schematic, Row, Col) ->
    Item = get_schematic_item(Schematic, Row, Col),
    IsStar = 
        if Item == "*" ->
            true;
        true ->
            false
        end,
    RowTopLeftFlag = is_a_number(get_schematic_item(Schematic, Row-1,Col-1)),
    RowTopMiddleFlag = is_a_number(get_schematic_item(Schematic, Row-1,Col)),
    RowTopRightFlag = is_a_number(get_schematic_item(Schematic, Row-1,Col+1)),
    RowMiddleLeftFlag = is_a_number(get_schematic_item(Schematic, Row,Col-1)),
    RowMiddleRightFlag = is_a_number(get_schematic_item(Schematic, Row,Col+1)),
    RowBottomLeftFlag = is_a_number(get_schematic_item(Schematic, Row+1,Col-1)),
    RowBottomMiddleFlag = is_a_number(get_schematic_item(Schematic, Row+1,Col)),
    RowBottomRightFlag = is_a_number(get_schematic_item(Schematic, Row+1,Col+1)),
    RowTopScoreTemp = 
        if 
            (RowTopLeftFlag and RowTopMiddleFlag and RowTopRightFlag) or
            (RowTopLeftFlag and not RowTopMiddleFlag and not RowTopRightFlag) or
            (not RowTopLeftFlag and not RowTopMiddleFlag and RowTopRightFlag) or
            (RowTopLeftFlag and RowTopMiddleFlag and not RowTopRightFlag) or
            (not RowTopLeftFlag and RowTopMiddleFlag and RowTopRightFlag) or
            (not RowTopLeftFlag and RowTopMiddleFlag and not RowTopRightFlag) ->
                1;
            true ->
                0
        end,        
    RowTopScore = 
        if 
            (RowTopLeftFlag and not RowTopMiddleFlag and RowTopRightFlag) ->
                2;
            true ->
                RowTopScoreTemp
        end,
    RowMiddleScoreTemp =
        if 
            (RowMiddleLeftFlag and RowMiddleRightFlag) ->
                2;
            true ->
                0
        end,
    RowMiddleScore =
        if
            (RowMiddleLeftFlag and (not RowMiddleRightFlag)) or 
            ((not RowMiddleLeftFlag) and RowMiddleRightFlag) ->             
                1;
            true ->
                RowMiddleScoreTemp
        end,
    RowBottomScoreTemp = 
        if 
            (RowBottomLeftFlag and RowBottomMiddleFlag and RowBottomRightFlag) or
            (RowBottomLeftFlag and not RowBottomMiddleFlag and not RowBottomRightFlag) or
            (not RowBottomLeftFlag and not RowBottomMiddleFlag and RowBottomRightFlag) or
            (RowBottomLeftFlag and RowBottomMiddleFlag and not RowBottomRightFlag) or
            (not RowBottomLeftFlag and RowBottomMiddleFlag and RowBottomRightFlag) or
            (not RowBottomLeftFlag and RowBottomMiddleFlag and not RowBottomRightFlag) ->
                1;
            true ->
                0
        end,    
    RowBottomScore =     
        if 
            (RowBottomLeftFlag and not RowBottomMiddleFlag and RowBottomRightFlag) ->                
                2;
            true ->
                RowBottomScoreTemp
        end,
    TotalScore = RowTopScore + RowMiddleScore + RowBottomScore,
    if 
        (TotalScore == 2) and IsStar ->
            true;
        true ->
            false
    end.

% Get associated gear item rows.
get_gear_item_rows(Schematic, Row, Col) ->
    RowTopLeftFlag = is_a_number(get_schematic_item(Schematic, Row-1,Col-1)),
    RowTopMiddleFlag = is_a_number(get_schematic_item(Schematic, Row-1,Col)),
    RowTopRightFlag = is_a_number(get_schematic_item(Schematic, Row-1,Col+1)),
    RowMiddleLeftFlag = is_a_number(get_schematic_item(Schematic, Row,Col-1)),
    RowMiddleRightFlag = is_a_number(get_schematic_item(Schematic, Row,Col+1)),
    RowBottomLeftFlag = is_a_number(get_schematic_item(Schematic, Row+1,Col-1)),
    RowBottomMiddleFlag = is_a_number(get_schematic_item(Schematic, Row+1,Col)),
    RowBottomRightFlag = is_a_number(get_schematic_item(Schematic, Row+1,Col+1)),
    Rows1 =
        if  RowTopLeftFlag or RowTopMiddleFlag or RowTopRightFlag ->
            [Row-1];
        true ->
            []
        end,
    Rows2 =
        if  RowMiddleLeftFlag or RowMiddleRightFlag ->
            lists:concat([Rows1,[Row]]);
        true ->
            Rows1
        end,    
    Rows3 =
        if  RowBottomLeftFlag or RowBottomMiddleFlag or RowBottomRightFlag ->
            lists:concat([Rows2,[Row+1]]);
        true ->
            Rows2
        end,    
    Rows3.

% Get all gear items in supplied row.
get_gear_items_in_row(Output, _, _, _, _, StopIndex, StopIndex) ->
    Output;
get_gear_items_in_row(Output, Schematic, Row, GearRow, GearCol, CurrentIndex, StopIndex) ->
    ItemLocations = get_item_locations([],Schematic, Row,1,get_row_Length(Schematic)+1),
    ItemLeft = lists:nth(CurrentIndex, ItemLocations),
    ItemRight = lists:nth(CurrentIndex+1, ItemLocations),
    NewOutput =
        if 
            (ItemLeft =< GearCol+1) and (ItemRight >= GearCol-1) ->
                Item = get_item_by_coords("", Schematic, Row, ItemLeft, ItemRight), 
                lists:concat([Output, [Item]]);
            true ->
                Output
        end,
    get_gear_items_in_row(NewOutput, Schematic, Row, GearRow, GearCol, CurrentIndex+2, StopIndex).

% Get all gear item rows for all gear coords.
get_all_gear_item_rows(Output, _, _, StopIndex, StopIndex) ->
    Output;
get_all_gear_item_rows(Output, Schematic, GearCoords, CurrentIndex, StopIndex) ->
    GearRow = lists:nth(CurrentIndex, GearCoords),
    GearCol = lists:nth(CurrentIndex+1, GearCoords),
    ItemRows = get_gear_item_rows(Schematic,GearRow,GearCol),
    NewOutput = lists:concat([Output, [[GearRow,GearCol],ItemRows]]),
    get_all_gear_item_rows(NewOutput, Schematic, GearCoords, CurrentIndex+2, StopIndex).

% Get all gear items from a gear item rows collection.
get_all_gear_items(Output, _, _, StopIndex, StopIndex) ->
    Output;
get_all_gear_items(Output, Schematic, GearItemRows, CurrentIndex, StopIndex) ->
    GearCoord = lists:nth(CurrentIndex, GearItemRows),
    GearRow = lists:nth(1, GearCoord),
    GearCol = lists:nth(2, GearCoord),
    Rows = lists:nth(CurrentIndex+1, GearItemRows),    
    NoRows = length(Rows),
    GearItemsTotal =
        if
            NoRows == 1 ->
                Row1 = lists:nth(1, Rows),                         
                ItemLocations = get_item_locations([],Schematic, Row1,1,get_row_Length(Schematic)),
                GearItems = get_gear_items_in_row([], Schematic, Row1, GearRow, GearCol, 1, length(ItemLocations)+1),
                GearItems;
            true ->
                Row1 = lists:nth(1, Rows),
                ItemLocations2 = get_item_locations([],Schematic, Row1,1,get_row_Length(Schematic)),
                GearItems1 = get_gear_items_in_row([], Schematic, Row1, GearRow, GearCol, 1, length(ItemLocations2)+1),                
                Row2 = lists:nth(2, Rows),
                ItemLocations3 = get_item_locations([],Schematic, Row2,1,get_row_Length(Schematic)),
                GearItems2 = get_gear_items_in_row([], Schematic, Row2, GearRow, GearCol, 1, length(ItemLocations3)+1),
                lists:concat([GearItems1,GearItems2])
        end,
    NewOutput = lists:concat([Output, [GearItemsTotal]]),
    get_all_gear_items(NewOutput, Schematic, GearItemRows, CurrentIndex+2, StopIndex).

% Reduce gear list to powers.
get_powers(Output, _, StopIndex, StopIndex) ->
    Output;
get_powers(Output, GearItems, CurrentIndex, StopIndex) ->
    GearItem = lists:nth(CurrentIndex, GearItems),
    FirstNum = lists:nth(1, GearItem),
    SecondNum = lists:nth(2, GearItem),
    Power = FirstNum * SecondNum,
    NewOutput = lists:concat([Output, [Power]]),
    get_powers(NewOutput, GearItems, CurrentIndex+1, StopIndex).

% Generic sum function.
sum(Func, Data, Acc) ->
    lists:foldr(Func, Acc, Data).
