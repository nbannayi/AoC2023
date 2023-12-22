#!/usr/bin/ruby

# Advent of Code 2023, day 12 - Hot Springs.
# Ruby.

require_relative 'ConditionRecord.rb'

# Parse the input and create an array of condition record objects.
condition_records1 = []
input_file = "Day12Input.txt"
File.open(input_file, 'r') do |file|
  file.each_line do |line|
    tokens = line.split(' ')
    pattern = tokens[0]
    groups = tokens[1].split(',').map(&:to_i)
    condition_records1 << ConditionRecord.new(pattern, groups)
  end
end

# Work out sum of counts for part 1.
sum_counts1 = 0
condition_records1.each do |cr|
  sum_counts1 += cr.get_no_valid_candidates
end
puts "Part 1 answer: #{sum_counts1}"

# Work out sum of counts for part 2.
sum_counts2 = 0

condition_records2 = condition_records1.map(&:unfold)
condition_records2.each do |cr|
  sum_counts2 += cr.get_no_valid_candidates_2
end
puts "Part 2 answer: #{sum_counts2}"
