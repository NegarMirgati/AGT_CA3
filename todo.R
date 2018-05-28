
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

