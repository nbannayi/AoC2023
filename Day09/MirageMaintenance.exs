# Advent of Code 2023, day 09 - Mirage Maintenance.
# Elixir.

defmodule AoC2023 do

  # Parse input into a list of lists.
  def parse_input(input_file) do
    {:ok, _} = File.open(input_file, [:read])

    parse_line = fn l ->
      l
      |> String.split(" ")
      |> Enum.map(&String.trim/1)
      |> Enum.map(&String.to_integer/1)
    end

    lines =
      File.stream!(input_file)
      |> Enum.to_list()
      |> Enum.map(parse_line)

    File.close(input_file)
    lines
  end

  # Determine if a sequence is fully processed.
  def is_fully_processed(sequences) do
    Enum.at(sequences, -1)
    |> Enum.all?(&(&1 == 0))
  end

  # Get a sequence history.
  def get_sequence_history(input_sequences) do
    if not is_fully_processed(input_sequences) do
      next_seq =
        Enum.at(input_sequences, -1)
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [a, b] -> b - a end)
      get_sequence_history(input_sequences ++ [next_seq])
    else
      input_sequences
    end
  end

  # From a sequene history work out extrapolated amount for part 1.
  def extrapolate_sequence_history_1(input_sequences, n) do
    cond do
      # 0-case processa and terminate.
      n == 0 ->
        slice1 = Enum.at(input_sequences, 0)
        slice2 = Enum.at(input_sequences, 1)
        slice3 = Enum.slice(input_sequences, 2..(length(input_sequences)-1))
        extrapolated_item = Enum.at(slice2, -1) + Enum.at(slice1, -1)
        extrapolated_line = [slice1 ++ [extrapolated_item]]
        next_sequences = extrapolated_line ++ [slice2] ++ slice3
        Enum.at(Enum.at(next_sequences, 0), -1)
      # Add a trailing 0 to last line.
      n == length(input_sequences)-1 ->
        slice = Enum.slice(input_sequences, 0..(n-1))
        extrapolated = Enum.at(input_sequences, -1) ++ [0]
        next_sequences = slice ++ [extrapolated]
        extrapolate_sequence_history_1(next_sequences, n-1)
      # Non-zero case.
      true ->
        slice1 = Enum.slice(input_sequences, 0..(n-1))
        slice2 = Enum.at(input_sequences, n)
        slice3 = Enum.slice(input_sequences, (n+1)..(length(input_sequences)-1))
        extrapolated_item = Enum.at(slice2, -1) + Enum.at(Enum.at(slice3, 0), -1)
        extrapolated_line = [slice2 ++ [extrapolated_item]]
        next_sequences = slice1 ++ extrapolated_line ++ slice3
        extrapolate_sequence_history_1(next_sequences, n-1)
    end
  end

  # From a sequene history work out extrapolated amount for part 2.
  def extrapolate_sequence_history_2(input_sequences, n) do
    cond do
      # 0-case processa and terminate.
      n == 0 ->
        slice1 = Enum.at(input_sequences, 0)
        slice2 = Enum.at(input_sequences, 1)
        slice3 = Enum.slice(input_sequences, 2..(length(input_sequences)-1))
        extrapolated_item = Enum.at(slice1, 0) - Enum.at(slice2, 0)
        extrapolated_line = [[extrapolated_item] ++ slice1]
        next_sequences = extrapolated_line ++ [slice2] ++ slice3
        Enum.at(Enum.at(next_sequences, 0), 0)
      # Add a leading 0 to last line.
      n == length(input_sequences)-1 ->
        slice = Enum.slice(input_sequences, 0..(n-1))
        extrapolated = [0] ++ Enum.at(input_sequences, -1)
        next_sequences = slice ++ [extrapolated]
        extrapolate_sequence_history_2(next_sequences, n-1)
      # Non-zero case.
      true ->
        slice1 = Enum.slice(input_sequences, 0..(n-1))
        slice2 = Enum.at(input_sequences, n)
        slice3 = Enum.slice(input_sequences, (n+1)..(length(input_sequences)-1))
        extrapolated_item = Enum.at(slice2, 0) - Enum.at(Enum.at(slice3, 0), 0)
        extrapolated_line = [[extrapolated_item] ++ slice2]
        next_sequences = slice1 ++ extrapolated_line ++ slice3
        extrapolate_sequence_history_2(next_sequences, n-1)
    end
  end

  # Extrapolate a single sequence - part 1.
  def extrapolate_sequence_1(sequence) do
    sequence_history = get_sequence_history([sequence])
    n = length(sequence_history)-1
    extrapolate_sequence_history_1(sequence_history, n)
  end

  # Extrapolate a single sequence - part 2.
  def extrapolate_sequence_2(sequence) do
    sequence_history = get_sequence_history([sequence])
    n = length(sequence_history)-1
    extrapolate_sequence_history_2(sequence_history, n)
  end

  # Find sum of all extrapolated sequences in list.
  def extrapolate_sequences(sequences, part) do
    if part == 1 do
      Enum.map(sequences, &extrapolate_sequence_1/1)
      |> Enum.sum()
    else
      Enum.map(sequences, &extrapolate_sequence_2/1)
      |> Enum.sum()
    end
  end

end

# Parts 1 & 2.
input_file = "Day09Input.txt"
sequences = AoC2023.parse_input(input_file)

part_1_result = AoC2023.extrapolate_sequences(sequences, 1)
part_2_result = AoC2023.extrapolate_sequences(sequences, 2)

IO.puts("Part 1 answer: #{part_1_result}")
IO.puts("Part 2 answer: #{part_2_result}")
