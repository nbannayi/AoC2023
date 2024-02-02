library(R6)

# An edge for the graph.
Edge <- R6Class(
  "Edge",
  
  private = list(
    from = "",
    to = ""
  ),
  
  public = list(
    initialize = function(from = "", to = "") {
      private$from <- from
      private$to <- to
    },
    
    # Get from node in edge.
    get_from = function() {
      return(private$from)
    },
    
    # Get to node in edge.
    get_to = function() {
      return(private$to)
    }
  )
)

# Graph containing edges.
Graph <- R6Class(
  "Graph",
  
  private = list(
    edges = list(),
    adjacency_list = list()
  ),
  
  public = list(
    # Construct the graph using an adjacency list.
    initialize = function(adj_list) {
      private$edges <- list()
      for (s in names(adj_list)) {
        t_list <- adj_list[[s]]
        for (t in t_list) {
          self$add_edge(Edge$new(s,t))
        }
      }
      private$adjacency_list <- adj_list
    },
    
    # Add an edge to the graph.
    add_edge = function(edge) {
      private$edges <- c(private$edges, edge)
    },
    
    # Get all edges in graph.
    get_edges = function() {
      return(private$edges)
    },
    
    # Get adjacency list in graph.
    get_adjacency_list = function() {
      return(private$adjacency_list)
    },
    
    # Get vertices in graph.
    get_vertices = function() {
      vertices <- list()
      for (edge in private$edges) {
        vertices <- c(vertices, edge$get_from(), edge$get_to())
      }
      return(unique(vertices))
    },
    
    # Perform a min-cut using Karger's algorithm.
    # https://en.wikipedia.org/wiki/Karger%27s_algorithm.
    min_cut = function() {
      # Find root and make it the parent of vertex.
      find_root <- function(subsets, vertex) {
        if (subsets[vertex][[1]][[1]] != vertex) {
          subsets[vertex] <- find_root(subsets, subsets[vertex][[1]][[1]])
        }
        return(subsets[vertex][[1]][[1]])
      }
      union = function(subsets, vertex1, vertex2) {
        root1 = find_root(subsets, vertex1)
        root2 = find_root(subsets, vertex2)
        if (subsets[root1][[1]][[2]] < subsets[root2][[1]][[2]]) {
          subsets[root1][[1]][[1]] <- root2 
        } else {
          subsets[root2][[1]][[1]] <- root1
          if (subsets[root1][[1]][[2]] == subsets[root2][[1]][[2]]) {
            subsets[root1][[1]][[2]] <- subsets[root1][[1]][[2]] + 1
          }
        }
        return(subsets)
      }
      subsets <- list()
      vertices <- self$get_vertices()
      for (vertex in vertices) {
        inner_list <- list(vertex, 0)
        subsets[vertex] <- list(inner_list)
      }
      no_vertices <- length(vertices)
      edges <- self$get_edges()
      while (no_vertices > 2) {
        random_edge <- edges[[sample(1:length(edges), size = 1)]]
        vertex1 <- find_root(subsets, random_edge$get_from())
        vertex2 <- find_root(subsets, random_edge$get_to())
        if (vertex1 == vertex2) {
          next
        } else {
          # Contract random_edge ends.
          no_vertices <- no_vertices-1
          subsets <- union(subsets, vertex1, vertex2)
        }
      }
      # Get edges.
      no_cut_edges <- 0
      min_cut_edges = list()
      for (edge in edges) {
        subset1 <- find_root(subsets, edge$get_from())
        subset2 <- find_root(subsets, edge$get_to())
        if (subset1 != subset2) {
          min_cut_edges <- c(min_cut_edges, edge)
          no_cut_edges <- no_cut_edges + 1
        }
      }
      return(list(no_cut_edges, min_cut_edges))
    },
    
    # Cut edges out of graph and return the partition sizes.
    cut_and_count = function(min_cut_edges) {
      # First of all remove min cut edges from adjacency list.
      for (edge in min_cut_edges) {
        s <- edge$get_from()
        t <- edge$get_to()
        for (s_al in names(self$get_adjacency_list())) {
          t_list <- private$adjacency_list[[s]]
          if (s_al == s) {
            t_list <- t_list[t_list != t]
          }
          private$adjacency_list[[s]] <- t_list
        }
        s <- edge$get_to()
        t <- edge$get_from()
        for (s_al in names(self$get_adjacency_list())) {
          t_list <- private$adjacency_list[[s]]
          if (s_al == s) {
            t_list <- t_list[t_list != t]
          }
          private$adjacency_list[[s]] <- t_list
        }
      }
      # Walk through all nodes.
      no_vertices <- length(self$get_vertices())
      self$initialize(private$adjacency_list)
      edges <- self$get_edges()
      start_edge <- edges[[1]]
      start_node <- start_edge$get_from()
      nodes <- list(start_node)
      visited_notes <- list()
      reverse_neighbours <- list()
      for (key in names(private$adjacency_list)) {
        value <- private$adjacency_list[[key]]
        for (node in value) {
          reverse_neighbours[[node]] <- c(reverse_neighbours[[node]], key)
        }
      }
      # Get all neighbours of a node.
      get_neighbours <- function(node) {
        neighbours <- c(private$adjacency_list[[node]],reverse_neighbours[[node]])
        return(unique(neighbours))
      }
      while (length(nodes) > 0) {
        top <- nodes[[1]][[1]]
        visited_nodes <- unique(c(visited_nodes, top))
        nodes <- nodes[-1]
        for (neighbour in get_neighbours(top)) {
          if (!(neighbour %in% visited_nodes)) {
            nodes <- unique(c(nodes, neighbour))
          }
        }
      }
      # Finally get count.
      visited_count <- length(visited_nodes)-1
      return(list(visited_count, no_vertices-visited_count))
    },
    
    # Display all edges in graph.
    display = function() {
      for (edge in self$get_edges()) {
        cat(edge$get_from(), "-", edge$get_to(), "\n")
      }
    }
  )
)