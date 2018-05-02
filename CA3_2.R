install.packages("lpSolve")
library(lpSolve)
library(igraph)

g <- erdos.renyi.game(100, 0.7)

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
cat('num_of_colors = ', num_of_colors)
plot(g, vertex.label = V(g)$color, vertex.size = 4)}

greedy_coloring(g)

