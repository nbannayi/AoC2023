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

-- Parse scratchcards.
file_name = "Day04Input.txt"
player_numbers, winning_numbers = get_scratchcard_numbers(get_file_lines(file_name))

-- Get final result.
print("Part 1 answer: ", calculate_scratchcard(player_numbers, winning_numbers))
