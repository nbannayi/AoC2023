-- Adevent of Code Day 04 - Scratchcards.
-- Lua.

-- Split lines by a separator.
function split(str, sep)
    local result = {}
    local regex = ("([^%s]+)"):format(sep)    
    for each in str:gmatch(regex) do
       table.insert(result, each)
    end
    return result
 end
 
-- Trim string with leading and trailing spaces.
function trim(s)
    return (s:gsub("^%s*(.-)%s*$", "%1"))
end

-- Print a list.
function display_list(list)
    io.write("[ ")
    if list ~= nil then
        for i=1,#list do
            io.write(list[i].." ");
        end
    end
    io.write("]")
end

-- Get puzzle input and input into lines array.
function get_file_lines(file_name)
    file = io.open(file_name, "r")
    io.input(file)
    lines, i = {}, 0
    while true do
        i = i + 1
        line = io.read()
        if (line == nil) then break end
        lines[i] = line
    end
    io.close(file)
    return lines
end

 -- Get scratchcard details.
function get_scratchcard_numbers(lines)    
    local player_numbers = {}
    local winning_numbers = {}
    for i,line in ipairs(lines) do
        -- Parse player numbers.
        tokens1 = split(trim(split(line,"|")[1])," ")
        player_numbers[i] = {}
        for j = 3,#tokens1 do
            player_numbers[i][j-2] = tokens1[j]
        end
        -- Parse winning numbers.
        tokens2 = split(trim(split(line,"|")[2])," ")
        winning_numbers[i] = {}
        for j = 1,#tokens2 do
            winning_numbers[i][j] = tokens2[j]
        end
    end
    return player_numbers, winning_numbers
end

-- Check a given line for winning numbers.
function check_line(player_num_line, winning_num_line)
    matches = 0
    score = 0
    for _,p in ipairs(player_num_line) do
        for _,w in ipairs(winning_num_line) do
            if (p == w)
            then
                matches = matches + 1
            end
        end
    end
    if matches > 0
    then
        score = math.pow(2, matches-1)
    end
    return matches, score 
end

-- Get overall score for all lines on a scratchcard.
function calculate_scratchcard(player_numbers, winning_numbers)
    total_score = 0
    for i = 1,#player_numbers do
        player_num_line = player_numbers[i]
        winning_num_line = winning_numbers[i]
        _, score = check_line(player_num_line, winning_num_line)
        total_score = total_score + score
    end
    return total_score
end

-- Get card number to winner mappings.
function get_card_winners(player_numbers, winning_numbers)    
    card_winners = {}
    for i = 1,#player_numbers do
        player_num_line = player_numbers[i]
        winning_num_line = winning_numbers[i]
        matches, _ = check_line(player_num_line, winning_num_line)
        card_winners[i] = {}
        for j = i+1,i+matches do
            table.insert(card_winners[i], j)
        end
    end
    return card_winners
end

-- Display card winners table.
function display_card_winners(card_winners)
    for i = 1,#card_winners do
        io.write(i..") ")
        display_list(card_winners[i])
        print()
    end
end

-- Display cards table.
function display_cards(cards)
    for i = 1,#cards do
        io.write(i..") "..cards[i][1].." ")
        display_list(cards[i][2])
        print()
    end
end

-- Process crazy scratchcard game.
-- Way too slow for part 2 - need to write a better version!
function process_cards_1(card_numbers, winning_numbers)
    processed_card_numbers = {}
    while (#card_numbers > 0)
    do
        top_card = table.remove(card_numbers, 1)
        table.insert(processed_card_numbers, top_card)
        no_winners = winning_numbers[top_card]
        for i = 1,no_winners do
            table.insert(card_numbers, top_card+i)
        end
    end
    return #processed_card_numbers
end

-- Process using some kind of memoisation (is it?)
-- (God I hope this works - if it doesn't I'm out of ideas!)
function process_cards_2(player_numbers, winning_numbers)
    -- Process all cards for 1 pass through.
    function process_pass(cards)
        for n=1,#cards do
            winners = cards[n][2]
            cards[n][1] = cards[n][1] + #winners
            new_winners = {}
            for i=1,#winners do
                w = winners[i]
                for j=1,#card_winners[w] do
                    table.insert(new_winners, card_winners[w][j])
                end        
            end
            cards[n][2] = new_winners
        end
    end        
    -- Return true when fully processed.
    function is_processed(cards)
        winners = 0
        for n=1,#cards do
            winners = winners + #cards[n][2]
        end
        return winners == 0
    end
    -- Get current card count.
    function get_count(cards)
        count = 0
        for n=1,#cards do
            count = count + cards[n][1]
        end
        return count
    end        
    -- Set up card winners table and cards table to process through passes.
    card_winners = get_card_winners(player_numbers, winning_numbers)
    cards = {}
    for i=1,#card_winners do
        cards[i] = {}
        table.insert(cards[i], 1)
        table.insert(cards[i], card_winners[i])
    end
    -- Finally, run through to completion...
    while (not is_processed(cards)) do    
        process_pass(cards)
    end
    -- ...and return count.
    return get_count(cards)    
end

-- Parse scratchcards.
file_name = "Day04Input.txt"
player_numbers, winning_numbers = get_scratchcard_numbers(get_file_lines(file_name))

-- Get part 1 result.
print("Part 1 answer: ", calculate_scratchcard(player_numbers, winning_numbers))

-- Get part 2 result.
print("Part 2 answer: ", process_cards_2(player_numbers, winning_numbers))