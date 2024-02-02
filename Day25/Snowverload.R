# Advent of Code 2023 - Day 25 - Snowverload.
# R.

# Unfortunately the first part of this runs super slow with the real input but I found it so tricky at this stage I don't care.
# Depending on how lucky you are could take anything from a few mins to maybe 30 mins.  The last run went on for around 150 iterations.

source("Graph.R")

# Parse components into a list.
input_file <- "Day25Input.txt"

input_lines <- readLines(input_file)
component_list <- list()
for (line in input_lines) {
  components <- strsplit(line, ":")[[1]]
  s <- trimws(components[1])
  t_list <- strsplit(trimws(components[2]), " ")[[1]]
  component_list[[s]] <- t_list
}

# Get all edges that need to be removed.
no_edges <- 0
result <- list()
component_graph <- list()
no_attempts <- 0
while (no_edges != 3) {
  component_graph <- Graph$new(component_list)
  result <- component_graph$min_cut()
  no_attempts <- no_attempts+1
  no_edges <- result[[1]]
  cat("Found",no_edges,"edges:\n")
  for (edge in result[[2]]) {
    cat(no_attempts,":",edge$get_from(),"-",edge$get_to(),"\n")
  }
}
min_cut_edges <- result[[2]]

# Finally cut out edges and count sets.
component_graph <- Graph$new(component_list)
disjoint_sets <- component_graph$cut_and_count(min_cut_edges)
cat("Part 1 answer:",disjoint_sets[[1]]*disjoint_sets[[2]])