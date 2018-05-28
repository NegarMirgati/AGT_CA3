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
ip_coloring <- function(graph){
  # TODO :((
  #num_vertices <- gorder(graph)
  #num_of_colors <- num_of_vertices
  #f.obj <- rep(1, num_colors)
  #f.dir <- c("=", "<=", "<=", "<=")
  #f.rhs <- c(1, 0, 1, 1)
  #lp ("min", f.obj, f.con, f.dir, f.rhs)
  
}
##############

g1 <- erdos.renyi.game(50, 500, type = "gnm", directed = F, loops = F)
g2 <- g1
greedy_coloring(g1)
ip_coloring(g2)

