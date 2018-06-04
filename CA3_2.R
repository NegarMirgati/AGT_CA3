#CA3_2 -> Coloring
install.packages("lpSolveAPI")
install.packages("lpSolve")
library(lpSolveAPI)
library(igraph)

greedy_coloring <- function(g){
start.time <- Sys.time()
num_of_vertices <- gorder(g)
order <- order(degree(g), decreasing = TRUE)
num_of_colors <- 1

for(i in 1 : num_of_vertices){
  v_neighbors <- neighbors(g, order[i])
  possible_colors <- c(1:num_of_colors)[!(c(1:num_of_colors)) %in% (V(g)[v_neighbors]$color)]
  if(length(possible_colors) > 0){
    V(g)[order[i]]$color <- min(possible_colors)
  }
  else{
    num_of_colors <- num_of_colors + 1
    V(g)[order[i]]$color <- num_of_colors
  }
  
}
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
cat('num_of_colors in greedy coloring = ', num_of_colors)
plot(g, vertex.label = V(g)$color, vertex.size = 4)}

###############
library(lpSolve)
ip_coloring <- function(graph){
  
  num_of_vertices <- gorder(graph)
  num_of_colors <- num_of_vertices
  num_of_edges <- gsize(graph)
  const_row_num <-  num_of_vertices + num_of_vertices * num_of_colors + num_of_edges * num_of_colors 
  const_col_num <- num_of_colors + num_of_vertices * num_of_colors
  const_mat <- matrix(0, nrow = const_row_num, ncol = const_col_num, byrow=TRUE)
  f.obj <- c(rep(1, num_of_colors), rep(0, num_of_colors*num_of_vertices)) #y , x variables, y coeffs = 1, x coeffs = 0
  counter <- 1 # counts the number of constraint rows used
  
  #1
  
  #which(const_mat[1,] %in% c(1))
  for ( i in 1 : num_of_vertices){
    for( k in 1 : num_of_colors){
      const_mat[counter, num_of_colors + (i-1)*num_of_colors + k] <- 1
    }
    counter <- counter + 1
  }
  
  #2
  for(i in 1 : num_of_vertices){
    for(k in 1 : num_of_colors){
      
      const_mat[counter, num_of_colors + (i-1)*num_of_colors + k] <- 1
      const_mat[counter,k] <- -1
      counter <- counter + 1
    }
  }
  #3
  ends <- ends(graph, E(graph))
  for(e in 1 : num_of_edges){
    for(k in 1 : num_of_colors){
      i <- ends[e,1]
      j <- ends[e,2]
      #print(i)
      #print(j)
      #print(k)
      const_mat[counter, num_of_colors + (i-1)*num_of_colors + k] <- 1
      const_mat[counter, num_of_colors + (j-1)*num_of_colors + k] <- 1
      counter <- counter + 1
    }
  }
  
  #f.con <-matrix (const_mat, nrow=const_row_num, byrow=TRUE)
  f.con <- const_mat
  f.rhs <-c( rep(0,num_of_vertices), rep(1,num_of_vertices * num_of_colors), rep(1,num_of_edges * num_of_colors) )
  f.dir <- rep("<=", const_row_num)
  ans <- lp ("min", f.obj, f.con, f.dir, f.rhs, all.bin=TRUE)
  print(ans)
}
##############

g1 <- erdos.renyi.game(50, 500, type = "gnm", directed = F, loops = F)
gorder(graph)
g2 <- g1
greedy_coloring(g1)
ip_coloring(g2)

