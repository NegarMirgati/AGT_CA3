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

#directed_dominating_set <- function(dir_graph){
  # black <- dominating set white <- unseen grey <- seen
  V(dir_graph)$color <- "white"
  dominating_set <- list()
  #rule 1
  degrees_in <- degree(dir_graph, mode = "in")
  without_preds <- which(degrees_in == 0)
  if(length(without_preds) > 0){
    
    V(dir_graph)[without_preds]$color = "black"
    dominating_set <- append(dominating_set, without_preds)
    
    seen <- neighbors(dir_graph, without_preds)
    V(dir_graph)[seen]$color = "grey"
    for (i in 1 : length(without_preds)){
      without_pred <- without_preds[i]
      seen_v <- neighbors(dir_graph, without_pred)
      dir_graph[ from = rep(without_pred, length(seen_v)), to = seen_v ] <- 0
    }
  }
  #dominating_set
 #rule 2
 degrees_in <- degree(dir_graph, mode = "in")
 one_preds <- which(degrees_in == 1)
 unseens <- which(V(dir_graph)$color == "white")
 one_preds_unseen <- intersect(one_preds, unseens)
 len <- length(one_preds_unseen)
 if(len > 0){
 for( i in 1 : len){

   children <- neighbors(dir_graph, one_preds_unseen[i])
   if(length(which(children$color == "white")) == 0){ # no unseen successors
     pred_v <- neighbors(dir_graph, one_preds_unseen[i], mode = "in")  # its pred
     # if its pred is  unoccupied
     if(pred_v$color == "white"){
       # add its pred to dominating set and set its color to black
       V(dir_graph)[pred_v]$color = "black"
       dominating_set <- append(dominating_set, pred_v)
       # add all pred_v unseen[white] successors(including one_pred_unseen[i]) to seen vertices
       pred_v_children <- neighbors(dir_graph, pred_v)
       unseen_vertices <- which(V(dir_graph)$color == "white")
       pred_v_unseen_children <- intersect(pred_v_children, unseen_vertices)
       V(dir_graph)[pred_v_unseen_children]$color = "grey"
       # delete edges between pred and all its successors 
       dir_graph[ from = rep(pred_v, length(pred_v_unseen_children)), to = pred_v_unseen_children] <- 0
       # delete edges incident to pred
       pred_v_preds <- neighbors(dir_graph, pred_v, mode = "in")
       dir_graph[ from =  pred_v_preds , to = rep(pred_v, length(pred_v_preds) ) ] <- 0
     }
   }
 }
 }
 
 #rule 3 : If an unoccupied but observed vertex l has only a single unobserved successor (say m) in the current digraph D, occupying l is not better than occupying m, therefore the arc (l, m) is deleted from D (Fig. 1C). We emphasize that vertex m is still unobserved after this arc deletion.
 in_degrees <- degree(dir_graph, v = V(dir_graph), mode = "in")
 grey_vertices <- which(V(dir_graph)$color == "grey")
 one_preds <- which(in_degrees == 1)
 grey_one_preds <- intersect(one_preds, grey_vertice)
 len <- length(grey_one_preds)
 if(len > 0){
   for(i in 1 : len){
     v <- grey_one_preds[i]
     
   }
 }
 
   
  return (dominating_set)
  
#}
 

dir_graph <- make_empty_graph(6)
dir_graph <- add.edges(dir_graph, c(1,2))
dir_graph <- add.edges(dir_graph, c(1,3))
dir_graph <- add.edges(dir_graph, c(2,4))
degree(dir_graph, v = V(dir_graph))
plot(dir_graph)
dir_graph <- erdos.renyi.game(21, 0.7, type=c("gnp"), directed = TRUE, loops = FALSE)
plot(dir_g)
dir_dom_set <- directed_dominating_set(dir_g)


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
  
  for(i in 1 : num_of_vertices - length(isolated)){
    # if all vertices were marked end the loop
    i<- 1
    #print(target/num_of_vertices)
    #cat("target = ", target)
    if(length(which(V(test_grph)$marked==TRUE)) == num_of_vertices){
      print('breaking')
      break
    }
    al <- get.adjlist(test_grph)
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
###########
