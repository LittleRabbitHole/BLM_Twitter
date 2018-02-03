setwd("/Users/angli/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/extracted_graph")
library(igraph)
data_08 = read.csv("08_08.csv", header = FALSE)
data_08$V1 = as.character(data_08$V1)
data_08$V2 = as.character(data_08$V2)

#need to keep the id/tashtag unique, maintain a name table
#updating for each day
#global ID same
hashtages = unique(c(as.character(data_08$V1), as.character(data_08$V2)) )
ID = 1:length(hashtages)
hashtags_table = data.frame(ID, hashtages)
#initialize global_tag_table as the first date
global_tag_table = hashtags_table

#updating the global_tag_table with new date
data_09 = read.csv("08_09.csv", header = FALSE)
update_globaltags <- function(new_data, global_tag_table){
  #updating the global_tag_table with new date
  hashtages_new = unique(c(as.character(new_data$V1), as.character(new_data$V2)) )
  new = setdiff(global_tag_table$hashtages, hashtages_new) #new hashtags
  newIDs = (length(global_tag_table$hashtages)+1):(length(global_tag_table$hashtages)+length(new))
  new_tage_table = data.frame(ID = newIDs, hashtages = new)
  global_tag_table = rbind(global_tag_table, new_tage_table)
  return(global_tag_table)
}

global_tag_table = update_globaltags(data_09, global_tag_table)


#merge global hashtable with IDs, output
update_edgelist <- function(edgelistdata, global_tag_table){
  edgelistdata_update = merge(edgelistdata, global_tag_table, by.x = "V1", by.y = "hashtages")
  colnames(edgelistdata_update) = c("V1", "V2", "V3", "V1_ID")
  edgelistdata_update2 = merge(edgelistdata_update, global_tag_table, by.x = "V2", by.y = "hashtages")
  colnames(edgelistdata_update2) = c("V1", "V2", "V3", "V1_ID", "V2_ID")
  return(edgelistdata_update2)
}

edgelist08_update = update_edgelist(data_08, global_tag_table)



edgelist = edgelist08_update
#building nets
hashtag_net <- function(edgelist, global_tag_table){
  #this function is to general the network
  net = graph.edgelist(cbind(edgelist$V1_ID, edgelist$V2_ID), directed=FALSE) 
  fullEdges = length(E(net))
  V(net)$label = as.character(global_tag_table$hashtages)
  V(net)$id = as.character(global_tag_table$ID)
  E(net)$weight = edgelist$V3
  #results = session_net
  return (net)
}

net_08 = hashtag_net(edgelist08_update, global_tag_table)
summary(net_08)
#IGRAPH U-W- 476 783 -- 
#plot(net_08)

#return all the network measures
network_output <- function(edge.net){
  #this function is to calculate the individual network measures
  netID =  V(edge.net)$id
  hashtag = V(edge.net)$label
  transitivity = transitivity(edge.net, type="local")
  betweenness = betweenness(edge.net,weights = E(edge.net)$weight, normalized = FALSE)
  betweenness_norm = betweenness(edge.net,weights = E(edge.net)$weight, normalized = TRUE)
  closeness = closeness(edge.net,weights = E(edge.net)$weight, normalized = FALSE)
  closeness_norm = closeness(edge.net,weights = E(edge.net)$weight, normalized = TRUE)
  degree = degree(edge.net, mode="all",normalized = FALSE)
  degree_norm = degree(edge.net, mode="all",normalized = TRUE)
  Indegree = degree(edge.net, mode="in",normalized = FALSE)
  Indegree_norm = degree(edge.net, mode="in",normalized = TRUE)
  Outdegree = degree(edge.net, mode="out",normalized = FALSE)
  Outdegree_norm = degree(edge.net, mode="out",normalized = TRUE)
  weighted_Indegree = strength(edge.net, mode = "in", weights = E(edge.net)$weight)
  weighted_Outdegree = strength(edge.net, mode = "out", weights = E(edge.net)$weight)
  eigen = eigen_centrality(edge.net, weights = E(edge.net)$weight, directed = TRUE)$vector
  net_measure = cbind(netID, hashtag, transitivity, betweenness, betweenness_norm, 
                      closeness, closeness_norm, degree, degree_norm, Indegree, Indegree_norm, 
                      Outdegree, Outdegree_norm, weighted_Indegree, weighted_Outdegree, eigen) 
  return (net_measure)
}

network_measure_08 = network_output(net_08)
write.csv(network_measure_08, "network_measure_08.csv")
write_graph(net_08, "net_08.graphml", format ="graphml")
