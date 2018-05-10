# CA3_1 -> Dominating Set
library(igraph)
num_rows <- 12
queen_graph <- make_empty_graph(num_rows * num_rows, directed = FALSE, LOOPS = FALSE)

add_horiz_edges <- function(row, col, g){
  
  vertex <- row * num_rows + col
  for(k in (col + 1) : num_rows){
    print(row)
    print(k)
    if(k <= num_rows){
      g <- add_edges(g, c(vertex, ( row * num_rows + k)))
    }
  }
  return(g)
}

add_vert_edges <- function(row, col, g){
  
  vertex <- row * num_rows + col
  for(k in (row + 1) : (num_rows - 1)){
    print(col)
    print(k)
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
  while((i * num_rows + j) <= num_rows * num_rows ){
    g <- add_edges(g, c(vertex, i * num_rows + j))
    i <- i + 1
    j <- j + 1
  }
  return(g)
  #plot(queen_graph)
}

for(i in 0 : (num_rows - 1)){
  for(j in 1  : num_rows){
    
    queen_graph <- add_horiz_edges(i ,j ,queen_graph)
    queen_graph <- add_vert_edges(i, j, queen_graph)
    queen_graph <- add_diag_edges(i, j, queen_graph)
    
  }
  
}
queen_graph <- simplify(queen_graph, remove.multiple = TRUE, remove.loops = TRUE)

plot(queen_graph, layout = grid)
degree(queen_graph, 2)


greedy_dominating_set <- function(g){
  dominating_set <- list()
  num_of_vertices <- gorder(g)
  order <- order(degree(g), decreasing = TRUE)
  all_vertices <- V(g)
  print(all_vertices)
  for(i in 1 : num_of_vertices){
    if(length(all_vertices) <= 0){
      break
    }
    v <- order[i]
    if(order[i] %in% all_vertices){

      dominating_set <- append(dominating_set, order[i])
      matches <- match(order[i], all_vertices)
      todel <- matches[!is.na(matches)]
      all_vertices <- all_vertices[-todel]
      matches <- match(c(neighbors(g, order[i])), all_vertices)
      todel <- matches[!is.na(matches)]
      if(length(todel) > 0){
        all_vertices <- all_vertices[-todel]
      }
    }
  }
  cat('final all vertices', all_vertices)
  cat('   number of dominating set vertices', length(dominating_set))

}

greedy_dominating_set(queen_graph)
greedy_dominating_set(make_empty_graph(5))
test <- make_empty_graph(6, directed = FALSE)
test <- add.edges(test, c(1,2))
test <- add.edges(test, c(1,3))
test <- add.edges(test, c(4,5))
test <- add.edges(test, c(1,6))

plot(test)
greedy_dominating_set(test)
