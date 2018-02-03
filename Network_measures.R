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
data_08_update = merge(data_08, hashtags_table, by.x = "V1", by.y = "hashtages")
colnames(data_08_update) = c("V1", "V2", "V3", "V1_ID")
data_08_update2 = merge(data_08_update, hashtags_table, by.x = "V2", by.y = "hashtages")
colnames(data_08_update2) = c("V1", "V2", "V3", "V1_ID", "V2_ID")




#building nets
hashtag_net <- function(data){
  #this function is to general the network
  net = graph.edgelist(cbind(data$V1, data$V2), directed=FALSE) 
  fullEdges = length(E(net))
  V(net)$wpid = as.character(all_ids$wpid)
  V(net)$label = as.character(all_ids$label)
  V(net)$id = as.character(all_ids$ID)
  V(net)$first_edit = as.character(all_ids$first_edit)
  V(net)$from_event = all_ids$from_event
  V(net)$final_check = all_ids$final_check
  #session1: IGRAPH D-W- 16287 17321 --
  E(session_net)$weight = session_edge$weight
  
  #results = session_net
  return (session_net)
}