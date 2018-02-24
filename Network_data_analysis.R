library(igraph)
library(disparityfilter)
library(dplyr)
library(ggplot2)

setwd("/Users/angli/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/extracted_graph/network_graph")

#read the graph data
net = read_graph("net_08.graphml", format = "graphml")
summary(net)
LabelID = data.frame(V(net)$id, V(net)$label)
colnames(LabelID) = c("id", "label")

#backdone filtering out unimportant edges
net_filtered = backbone(net)
id = min(c(net_filtered$from, net_filtered$to)):max(c(net_filtered$from, net_filtered$to))
ID_filter = data_frame(id)
filtered_IDlabel = merge(ID_filter, LabelID, by = "id")

#form updated network graph
net_new = graph.edgelist(cbind(net_filtered$from, net_filtered$to), directed=FALSE) 
summary(net_new)
E(net_new)$weight = net_filtered$weight
V(net_new)$label = as.character(filtered_IDlabel$label)
  
net_new1 =  simplify(net_new)
net_new2 = delete_vertices(net_new1, igraph::degree(net_new1)==0)

# the coreness
#coreness(net_new2, mode = "all")
#length(coreness(net_08_new2, mode = "all"))
V(net_new2)$kcore = coreness(net_new2, mode = "all")
#summary(net_08)

plot(net_new2, edge.width=0.05*E(net_new2)$weight)
