### CA3_ part 2
library(igraph)
#install.packages("DiagrammeR")
library("DiagrammeR")

directed_greedy_dominating_set <- function(g){
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
    al <- get.adjlist(g, mode = "in")
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

