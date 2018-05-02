# CA3_1 -> Dominating Set
g <- erdos.renyi.game(30, p = .6)
plot(g)
greedy_dominating_set <- function(g){
  dominating_set <- list()
  num_of_vertices <- gorder(g)
  order <- order(degree(g), decreasing = TRUE)
  all_vertices <- V(g)
  for(i in 1 : num_of_vertices){
    if(length(all_vertices) <= 0){
      break
    }
    v <- order[i]
    if(order[i] %in% all_vertices){
      cat('all_vertices',all_vertices)
      #cat('vertex found', order[i])
      dominating_set <- append(dominating_set, order[i])
      all_vertices <- all_vertices[-order[i] ]
      all_vertices <- all_vertices[-c(neighbors(g, order[i]))]
    }
  }
  cat('number of dominating set vertices', length(dominating_set))
  print(dominating_set)
  for(j in 1 : length(dominating_set))
    print(neighbors(g, dominating_set[j]))
}

greedy_dominating_set(g)

