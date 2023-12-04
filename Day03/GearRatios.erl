-module(gearratios).
-export([start/0]).

start() -> 
    % Input file and read into Schematic list. 
    {ok, File} = file:open("Day03Input.txt",[read]),
    {ok, Lines} = file:read(File,1024 * 1024),
    Schematic = string:tokens(Lines, "\n"),            
    AllValidItems = get_all_valid_items([], Schematic, 1, length(Schematic)+1),
    TotalValidItems = sum(fun(X, Acc) -> X+Acc end, AllValidItems, 0),
    io:fwrite("Part 1 answer: ~p~n", [TotalValidItems]),
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

% Check aLL surrounding squares to determine if a specific square is valid.
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

% Get all valid items in Schematoc.
get_all_valid_items(Output, _, StopIndex, StopIndex) ->
    Output;
get_all_valid_items(Output, Schematic, CurrentIndex, StopIndex) ->
    ItemLocations = get_item_locations([],Schematic, CurrentIndex,1,get_row_Length(Schematic)+1),
    ValidItems = get_valid_items_by_row([], Schematic, CurrentIndex, ItemLocations, 1, length(ItemLocations)+1),
    NewOutput = lists:concat([Output,ValidItems]),
    get_all_valid_items(NewOutput, Schematic, CurrentIndex+1, StopIndex).

% Generic sum function.
sum(Func, Data, Acc) ->
    lists:foldr(Func, Acc, Data).
