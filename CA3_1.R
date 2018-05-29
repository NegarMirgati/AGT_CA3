# CA3_1 -> Dominating Set
library(igraph)
add_horiz_edges <- function(row, col, g){
  
  vertex <- row * num_rows + col
  for(k in (col + 1) : num_rows){
  #for(k in 1 : num_rows){
    if(k <= num_rows){
      g <- add_edges(g, c(vertex, ( row * num_rows + k)))
    }
  }
  return(g)
}

add_vert_edges <- function(row, col, g){
  
  vertex <- row * num_rows + col
  for(k in (row + 1) : (num_rows - 1)){
  #for(k in 0 : (num_rows - 1)){
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
  al <- get.adjlist(g)
  for(i in 1 : num_of_vertices - length(isolated)){
    # if all vertices were marked end the loop
    if(length(which(V(g)$marked==TRUE)) == num_of_vertices){
      print('breaking')
      break
    }
    #al <- get.adjlist(g)
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
is.directed(queen_graph)
dom_set <- greedy_dominating_set(queen_graph)

############# directed dominating set
directed_greedy_dominating_set <- function(g){
  dominating_set <- list()
  num_of_vertices <- gorder(g)
  V(g)$marked = FALSE
  isolated <- which(degree(g, mode = "out")== 0)
  #adding isolated vertices to dominating set
  if(length(isolated) > 0){
    V(g)[isolated]$marked = TRUE
    dominating_set <- append(dominating_set, isolated)
  }
  al <- get.adjlist(g, mode = "in")
  for(i in 1 : num_of_vertices - length(isolated)){
    # if all vertices were marked end the loop
    if(length(which(V(g)$marked==TRUE)) == num_of_vertices){
      print('breaking')
      break
    }
    #al <- get.adjlist(g, mode = "in")
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
      to_mark <- neighbors(g, max_vertex, mode = "in")
      V(g)[to_mark]$marked = TRUE
      V(g)[max_vertex]$marked = TRUE
    }
  }
  cat('   number of dominating set vertices', length(dominating_set))
  return(dominating_set)
}

# test the function
test_graph <- erdos.renyi.game(50, type = "gnp", directed = TRUE, p = 0.5)
dom_set <- directed_greedy_dominating_set(test_graph)
print(dom_set)

####################### alpha dominating Set
alpha <- as.integer(readline(prompt="Enter alpha: "))
alpha <- alpha / 100

input_graph <- erdos.renyi.game(8, type="gnp", directed = FALSE, p = 0.5)
plot(input_graph)
alpha_dom_set <- alpha_dominating_set(input_graph, alpha)

alpha_dominating_set <- function(test_grph, alpha){
  target <- 0
  dominating_set <- list()
  num_of_vertices <- gorder(test_grph)
  V(test_grph)$marked = FALSE
  isolated <- which(degree(test_grph)== 0)
  #adding isolated vertices to dominating set
  if(length(isolated) > 0){
    V(test_grph)[isolated]$marked = TRUE
    dominating_set <- append(dominating_set, isolated)
    target <- target + length(dominating_set)
  }
  ### alpha
  if(target/num_of_vertices >= alpha){
    #print("Bye!!!!!!")
    cat('   number of dominating set vertices', length(dominating_set))
    return(dominating_set)
  }
  al <- get.adjlist(test_grph)
  for(i in 1 : num_of_vertices - length(isolated)){
    # if all vertices were marked end the loop
    #print(target/num_of_vertices)
    #cat("target = ", target)
    if(length(which(V(test_grph)$marked==TRUE)) == num_of_vertices){
      print('breaking')
      break
    }
    #al <- get.adjlist(test_grph)
    marked <- V(test_grph)$marked
    res <- sapply(al, function(x) sum(marked[x]==FALSE))
    res[which(V(test_grph)$marked == TRUE)] = -1
    max_vertices <- which(res == max(res))
    
    if(length(max_vertices) == 0){
      break
    }
    max_vertex <- max_vertices[1]
    if(V(test_grph)[max_vertex]$marked == FALSE){
      #cat('adding ', max_vertex, ' to dominating set')
      print(' ')
      dominating_set <- append(dominating_set, max_vertex)
      to_mark <- neighbors(test_grph, max_vertex)
      V(test_grph)[to_mark]$marked = TRUE
      V(test_grph)[max_vertex]$marked = TRUE
      ### alpha 
      #cat("adding ", 1 + length(to_mark))
      target <- target + 1 + length(to_mark)
      if(target/num_of_vertices >= alpha){
        #print("bye byeeeeeee")
        cat('   number of dominating set vertices', length(dominating_set))
        return(dominating_set)
      }
    }
  }
  cat('   number of dominating set vertices', length(dominating_set))
  return(dominating_set)
}
##############
library(lpSolve)
test_graph <- erdos.renyi.game(20, type = "gnp", directed = FALSE, p = 0.6)
V(test_graph)$weight <- sample(1:100, gorder(test_graph), replace = TRUE)

  
weighted_dominating_set <- function(graph){
  num_of_vertices <- gorder(graph)
  weights <- V(graph)$weight
  adj_mat <- as_adjacency_matrix(graph)
  adj_mat <- adj_mat + diag(num_of_vertices)
  f.obj <- V(graph)$weight
  f.con <- matrix (adj_mat, nrow=num_of_vertices, byrow=TRUE)
  f.dir <- rep(">=", num_of_vertices)
  f.rhs <- rep(1, num_of_vertices)
  return (lp ("min", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE))

}
ans <- weighted_dominating_set(test_graph) 
print(ans)
cat("dominating set : ", which(ans$solution %in% c(1)))


