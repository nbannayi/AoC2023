# Advent of Code day 7 - Camel Cards.
# Julia.

using Pkg
using Match

# All hand ranks in order.
@enum Rank begin
    high_card
    one_pair
    two_pair
    three_of_a_kind
    full_house
    four_of_a_kind
    five_of_a_kind
end

# Return strength of a card for comparison (no Jokers.)
function get_card_strength_1(card)
    return findfirst(isequal(card), "23456789TJQKA")
end

# Return strength of a card for comparison (with Jokers.)
function get_card_strength_2(card)
    return findfirst(isequal(card), "J23456789TQKA")
end

# Get card groups from a hand.
function get_card_groups(hand)
    cards = collect(hand)
    card_groups = Dict()
    for card in cards
        if !haskey(card_groups, card) card_groups[card] = 1 else card_groups[card] += 1 end
    end
    return card_groups
end

# classify a hand returning the corresponding rank (no Jokers.)
function classify_hand_1(hand)
    card_groups = get_card_groups(hand)
    no_groups = length(card_groups)    
    rank = @match no_groups begin
        1 => five_of_a_kind
        2 => if in(3, values(card_groups)) full_house else four_of_a_kind end
        3 => if in(3, values(card_groups)) three_of_a_kind else two_pair end
        4 => one_pair
        5 => high_card
    end    
    return rank
end

# classify a hand returning the corresponding rank (with Jokers.)
function classify_hand_2(hand)
    card_groups = get_card_groups(hand)
    no_jokers = if haskey(card_groups, 'J') card_groups['J'] else 0 end
    no_groups = length(card_groups)    
    rank = @match (no_groups, no_jokers) begin                
        (1, _)                    => five_of_a_kind        
        (2, 0)                    => if in(3, values(card_groups)) full_house else four_of_a_kind end
        (2, 1)                    => five_of_a_kind
        (2, 2) || (2, 3) || (2,4) => five_of_a_kind                 
        (3, 0)                    => if in(3, values(card_groups)) three_of_a_kind else two_pair end
        (3, 1)                    => if in(3, values(card_groups)) four_of_a_kind else full_house end
        (3, 2) || (3, 3)          => four_of_a_kind        
        (4, 0)                    => one_pair            
        (4, 1) || (4, 2)          => three_of_a_kind        
        (5, 0)                    => high_card
        (5, 1)                    => one_pair 
    end    
    return rank
end

# Comparer of two card sets to enable sorting.
function compare_cards(cards1, cards2, hand_classification_function, card_strength_function)
    rank1 = hand_classification_function(cards1[1])
    rank2 = hand_classification_function(cards2[1])    
    if rank1 != rank2
        rank1 < rank2
    else
        cards1_cards = collect(cards1[1])
        cards2_cards = collect(cards2[1])
        for i in 1:5
            if cards1_cards[i] == cards2_cards[i]
                continue
            else
                return card_strength_function(cards1_cards[i]) < card_strength_function(cards2_cards[i])                
            end
        end
    end
end

# Get total winnings from all hands.
function get_total_winnings(cards, hand_classification_function, card_stength_function)
    sorted_cards = sort(cards, lt=(c1,c2) -> compare_cards(c1, c2, hand_classification_function, card_stength_function))
    total_winnings = 0
    for i in 1:length(sorted_cards)
        total_winnings += i*sorted_cards[i][2]
    end
    return total_winnings
end

# Parse input and return an array of tuples of hand and bid.
function get_cards(input_file) 
    cards = []
    open(input_file) do file
        for line in eachline(file)
            hand, bid = split(line, " ")
            push!(cards,(hand,parse(Int,bid)))  
        end
    end
    return cards
end

# Parse input.
file_name = "Day07Input.txt"
cards = get_cards(file_name)

# Part 1.
print("Part 1 answer: ",get_total_winnings(cards, classify_hand_1, get_card_strength_1),"\n")

# Part 2.
print("Part 2 answer: ",get_total_winnings(cards, classify_hand_2, get_card_strength_2),"\n")