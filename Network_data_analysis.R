library(igraph)
library(disparityfilter)
library(dplyr)
setwd("/Users/angli/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/extracted_graph/network_graph")

net_08 = read_graph("net_08.graphml", format = "graphml")
summary(net_08)
LabelID08 = data.frame(V(net_08)$id, V(net_08)$label)
net_08_filtered = backbone(net_08)

net_08 = graph.edgelist(cbind(net_08_filtered$from, net_08_filtered$to), directed=FALSE) 
E(net_08)$weight = net_08_filtered$weight
net_08 =  simplify(net_08)
