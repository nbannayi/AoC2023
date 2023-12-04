-module(gearratios).
-export([start/0]).

start() -> 
    % Input file and read into Schematic list. 
    {ok, File} = file:open("Day03Input.txt",[read]),
    {ok, Lines} = file:read(File,1024 * 1024),
    Schematic = string:tokens(Lines, "\n"),            
    io:fwrite("~p~n", [Schematic]),
    ItemLocations = get_item_locations([],Schematic,42,1,get_row_Length(Schematic)+1),
    io:fwrite("~p~n", [ItemLocations]),
    ok.

% Get the Schematic character at 1-indexed position (Row,Col)
get_schematic_item(Schematic, Row, Col) ->
    RowLine = lists:nth(Row, Schematic),
    string:slice(RowLine, Col-1, 1).

% Gets the length of each line in schematic.
get_row_Length(Schematic) ->
    RowLine = lists:nth(1, Schematic),
    string:length(RowLine).

% Determine if a schematic item is a special character.

%is_special_character(item) ->
%    ItemPos = string:str("0123456789.", item),
%    if
%        ItemPos > 0 ->
%            false;
%        true ->
%            true
%    end.

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