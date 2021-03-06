setwd("/Users/ANG/Documents/GitHub/BLM_Twitter")

source('Network_Functions.R')

setwd("/Users/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/extracted_graph")
setwd("/Users/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/original_graph")
library(igraph)
data_08 = read.csv("08_08.csv", header = FALSE)
data_08 = pre_process(data_08)

#need to keep the id/tashtag unique, maintain a name table
#updating for each day
#global ID same
hashtages = unique(c(as.character(data_08$V1), as.character(data_08$V2)) )
ID = 1:length(hashtages)
hashtags_table = data.frame(ID, hashtages)
#initialize global_tag_table as the first date
global_tag_table = hashtags_table
edgelist08_update = update_edgelist(data_08, global_tag_table)
net_08 = hashtag_net(edgelist08_update, global_tag_table)
summary(net_08)

network_measure_08 = network_output(net_08)
write.csv(network_measure_08, "network_measure_08.csv")
write_graph(net_08, "net_08.graphml", format ="graphml")


#updating the global_tag_table with new date 08/09
data_09 = read.csv("08_09.csv", header = FALSE)
data_09 = pre_process(data_09)
#updating new global table
global_tag_table = update_globaltags(data_09, global_tag_table)
#edge number should be same as the data import edgelist number
edgelist09_update = update_edgelist(data_09, global_tag_table)
net_09 = hashtag_net(edgelist09_update, global_tag_table)
summary(net_09)
network_measure_09 = network_output(net_09)
write.csv(network_measure_09, "network_measure_09.csv")
write_graph(net_09, "net_09.graphml", format ="graphml")

#updating the global_tag_table with new date 08/10
data_10 = read.csv("08_10.csv", header = FALSE)
result = final_output(data_10)
network_measure_10 = result$network_measure
net_10 = result$net_new
write.csv(network_measure_10, "network_measure_10.csv")
write_graph(net_10, "net_10.graphml", format ="graphml")

#updating the global_tag_table with new date 
data_26 = read.csv("08_26.csv", header = FALSE)
result = final_output(data_26)
network_measure_26 = result$network_measure
net_26 = result$net_new
write.csv(network_measure_26, "network_measure_26.csv")
write_graph(net_26, "net_26.graphml", format ="graphml")


## analysis
network_measure = read.csv("network_measure_21.csv")
network_measure$betweenness = as.numeric(network_measure$betweenness)
network_measure$weighted_Indegree = as.numeric(network_measure$weighted_Indegree)
network_measure$closeness = as.numeric(network_measure$closeness)
#colnames(network_measure)
order = network_measure[order(network_measure$betweenness, decreasing = TRUE),]
order$hashtag[c(0:10)]
