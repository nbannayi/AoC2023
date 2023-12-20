# This class represents a ConditionRecord.
class ConditionRecord
  def initialize(pattern, groups)
    @pattern = pattern
    @groups  = groups
  end

  # Displays the pattern in the condition record.
  def display_pattern
    puts "The pattern is: "
    puts "#{@pattern}"
    puts
  end

  # Displays the groups array in the condition record.
  def display_groups
    puts "The groups are: "
    @groups.each { |number| puts number }
    puts
  end

  # Unfold and return updated object.
  def unfold
    unfolded_pattern = ""
    (1..5).each do |_|
      unfolded_pattern += @pattern+'?'
    end
    unfolded_pattern.slice!(-1)
    unfolded_groups = []
    (1..5).each do |_|
      unfolded_groups << @groups
    end
    unfolded_groups = unfolded_groups.flatten
    self.class.new(unfolded_pattern, unfolded_groups)
  end

  # Validate a given record base don the pattern and groups.
  def validate(condition_record)
    # Firstly checks lengths, if unequal can never match.
    if @pattern.length != condition_record.length
      return false
    end

    # Now check basic masking.
    zipped_elems = @pattern.chars.zip(condition_record.chars)
    zipped_elems.each do |zipped_elem|
      if (zipped_elem[0] != zipped_elem[1]) && zipped_elem[0] != '?'
        return false
      end
    end

    # Finally if we get this far examine groups.
    found_groups = []
    current_group = 0
    condition_record.chars.each do |elem|
      if elem == '.'
        if current_group != 0
          found_groups << current_group
        end
        current_group = 0
      else
        current_group += 1
      end
    end
    if current_group != 0
      found_groups << current_group
    end
    found_groups == @groups
  end

  # Get all possible dot patterns between #'s.'
  private def find_permutations(target_sum, num_elements, current_sum = 0, current_permutation = [])
    (1..current_permutation.length-2).each do |j|
      if current_permutation[j] == 0
        return []
      end
    end
    return [current_permutation] if num_elements == 0 && current_sum == target_sum
    return [] if num_elements == 0
    permutations = []
    (0..target_sum).each do |i|
      next_sum = current_sum + i
      next_permutation = current_permutation + [i]
      if next_sum <= target_sum
        sub_permutations = find_permutations(target_sum, num_elements - 1, next_sum, next_permutation)
        permutations += sub_permutations
      end
    end
    permutations
  end

  # Use a pattern mask to build a condition record.
  private def build_permutation(permutation)
    candidate_result = ''
    candidate_mask = permutation.zip(@groups).flatten
    candidate_mask.pop(1)
    (1..candidate_mask.length).each do |n|
      if n % 2 == 0
        candidate_result += "#" * candidate_mask[n-1]
      else
        candidate_result += "." * candidate_mask[n-1]
      end
    end
    candidate_result
  end

  # Finally get counts of all valid permutations.
  def get_no_valid_candidates
    target_sum = @pattern.length - @groups.sum
    num_elements = @groups.length+1
    find_permutations(target_sum, num_elements)
      .map { |p| build_permutation(p) }
      .select { |c| validate(c) }
      .length
  end
end
