# CA3_1 -> Dominating Set
library(igraph)
add_horiz_edges <- function(row, col, g){
  
  vertex <- row * num_rows + col
  for(k in (col + 1) : num_rows){
    if(k <= num_rows){
      g <- add_edges(g, c(vertex, ( row * num_rows + k)))
    }
  }
  return(g)
}

add_vert_edges <- function(row, col, g){
  
  vertex <- row * num_rows + col
  for(k in (row + 1) : (num_rows - 1)){
    if(k <= (num_rows - 1)){
      g <- add_edges(g, c(vertex, k * num_rows + col))
    }
  }
  return(g)
}

add_diag_edges <- function(row, col, g){
  vertex <- row * num_rows + col
  i <- row + 1
  j <- col + 1
  while( (i <= num_rows - 1) && (j <= num_rows) ){
    g <- add_edges(g, c(vertex, i * num_rows + j))
    i <- i + 1
    j <- j + 1
  }
  
  i <- row - 1
  j <- col - 1
  while((j >= 1) && (i >= 0) ){
    g <- add_edges(g, c(vertex, i * num_rows + j))
    i <- i - 1
    j <- j - 1
  }
  
  i <- row - 1
  j <- col + 1

  while((j <= num_rows) && (i >= 0)){
    g <- add_edges(g, c(vertex, i * num_rows + j))
    i <- i - 1
    j <- j + 1
  }
  
  i <- row + 1
  j <- col - 1
  while((j >= 1) && (i <= num_rows - 1)){
    g <- add_edges(g, c(vertex, i * num_rows + j))
    i <- i + 1
    j <- j - 1
  }
  return(g)
}

num_rows <- 12
queen_graph <- make_empty_graph(num_rows * num_rows, directed = FALSE)
for(i in 0 : (num_rows - 1)){
  for(j in 1  : num_rows){
    queen_graph <- add_horiz_edges(i ,j ,queen_graph)
    queen_graph <- add_vert_edges(i, j, queen_graph)
    queen_graph <- add_diag_edges(i, j, queen_graph)
  }
}
queen_graph <- simplify(queen_graph, remove.multiple = TRUE, remove.loops = TRUE)

greedy_dominating_set <- function(g){
  dominating_set <- list()
  num_of_vertices <- gorder(g)
  V(g)$marked = FALSE
  isolated <- which(degree(g)== 0)
  #adding isolated vertices to dominating set
  if(length(isolated) > 0){
    V(g)[isolated]$marked = TRUE
    dominating_set <- append(dominating_set, isolated)
  }

  for(i in 1 : num_of_vertices - length(isolated)){
    # if all vertices were marked end the loop
    if(length(which(V(g)$marked==TRUE)) == num_of_vertices){
      print('breaking')
      break
    }
    al <- get.adjlist(g)
    marked <- V(g)$marked
    res <- sapply(al, function(x) sum(marked[x]==FALSE))
    res[which(V(g)$marked == TRUE)] = -1
    max_vertices <- which(res == max(res))
    if(length(max_vertices) == 0){
      break
    }
    max_vertex <- max_vertices[1]
    if(V(g)[max_vertex]$marked == FALSE){
      cat('adding ', max_vertex, ' to dominating set')
      print(' ')
      dominating_set <- append(dominating_set, max_vertex)
      to_mark <- neighbors(g, max_vertex)
      V(g)[to_mark]$marked = TRUE
      V(g)[max_vertex]$marked = TRUE
    }
  }
  cat('   number of dominating set vertices', length(dominating_set))
  return(dominating_set)
}

dom_set <- greedy_dominating_set(queen_graph)

#############
#directed dominating set algorithm