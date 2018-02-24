library(igraph)
library(disparityfilter)
library(dplyr)
library(ggplot2)

setwd("/Users/angli/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/extracted_graph/network_graph")

filtered_net <- function(net){
  #read the graph data
  #summary(net)
  LabelID = data.frame(V(net)$id, V(net)$label)
  colnames(LabelID) = c("id", "label")

  #backdone filtering out unimportant edges
  net_filtered = backbone(net)
  id = 1:max(c(net_filtered$from, net_filtered$to))
  ID_filter = data_frame(id)
  filtered_IDlabel = merge(ID_filter, LabelID, by = "id")
  
  #form updated network graph
  net_new = graph.edgelist(cbind(net_filtered$from, net_filtered$to), directed=FALSE) 
  #summary(net_new)
  E(net_new)$weight = net_filtered$weight
  V(net_new)$label = as.character(filtered_IDlabel$label)
  
  net_new1 =  simplify(net_new)
  net_new2 = delete_vertices(net_new1, igraph::degree(net_new1)==0)

  # the coreness
  #coreness(net_new2, mode = "all")
  #length(coreness(net_08_new2, mode = "all"))
  V(net_new2)$kcore = coreness(net_new2, mode = "all")
  summary(net_new2)
  
  return(net_new2)
  
}

CorenessLayout <- function(g) {
  coreness <- graph.coreness(g);
  xy <- array(NA, dim=c(length(coreness), 2));
  
  shells <- sort(unique(coreness));
  for(shell in shells) {
    v <- 1 - ((shell-1) / max(shells));
    nodes_in_shell <- sum(coreness==shell);
    angles <- seq(0,360,(360/nodes_in_shell));
    angles <- angles[-length(angles)]; # remove last element
    xy[coreness==shell, 1] <- sin(angles) * v;
    xy[coreness==shell, 2] <- cos(angles) * v;
  }
  return(xy);
}

net = read_graph("net_26.graphml", format = "graphml")
summary(net)
net_new = filtered_net(net)
write_graph(net_new, "newnet_26.graphml", format ="graphml")

net_new = read_graph("newnet_10.graphml", format = "graphml")
table(V(net_new)$kcore)
#coreness
V(net_new)$label[V(net_new)$kcore == max(V(net_new)$kcore)]
V(net_new)$label[V(net_new)$kcore == min(V(net_new)$kcore)]

# plot
plot(igraph::subgraph(net_new, V(net_new)$kcore == max(V(net_new)$kcore)))
make_k_core_plot(net_new)

#
colbar <- rainbow(max(V(net_new)$kcore));
plot(net_new, layout=CorenessLayout(net_new), vertex.size=6, vertex.label.cex=0.9, vertex.color=colbar[V(net_new)$kcore], vertex.frame.color=colbar[V(net_new)$kcore], main='Coreness 08-09');

plot(net_new, edge.width=0.05*E(net_new2)$weight, layout=layout.graphopt)
