setwd("/Users/angli/Documents/GitHub/BLM_Twitter")

source('Network_Functions.R')

setwd("/Users/angli/ANG/GoogleDrive/GoogleDrive_Pitt_PhD/UPitt_PhD_O/Research/Yu-Ru_Project_GDrive/2017_blacklivesmatter/data/extracted_graph")

all_files = c("08_09.csv", "08_10.csv", "08_11.csv", "08_12.csv", "08_13.csv", "08_14.csv", "08_15.csv", "08_16.csv", "08_17.csv",
              "08_18.csv", "08_19.csv", "08_20.csv", "08_21.csv", "08_22.csv", "08_23.csv", "08_24.csv", "08_25.csv", "08_26.csv")

data_08 = read.csv("08_08.csv", header = FALSE)
all_data = data_08 
for (f in all_files){
  data_date = read.csv(f, header = FALSE)
  all_data = rbind(all_data, data_date)
}


library(dplyr)
group_data <- all_data %>%
              group_by(V1, V2) %>%
              summarise(sum=sum(V3))

write.csv(group_data, "all_hashtag_pairs.csv", row.names = FALSE)

allnetwork_files = c("network_measure_09.csv", "network_measure_10.csv", "network_measure_11.csv", "network_measure_12.csv", "network_measure_13.csv", 
                     "network_measure_14.csv", "network_measure_15.csv", "network_measure_16.csv", "network_measure_17.csv", "network_measure_18.csv", 
                     "network_measure_19.csv", "network_measure_20.csv", "network_measure_21.csv", "network_measure_22.csv", "network_measure_23.csv", 
                     "network_measure_24.csv", "network_measure_25.csv", "network_measure_26.csv")

network_08 = read.csv("network_measure_08.csv")
connections_08 = network_08[c("hashtag", "weighted_Indegree")]
write.csv(connections_08, "connection_network_measure_08.csv", row.names = FALSE)

all_networks = connections_08 
for (f in allnetwork_files){
  network_date = read.csv(f)
  connections_date = network_date[c("hashtag", "weighted_Indegree")]
  name = paste("connection", f, sep="_")
  write.csv(connections_date, name, row.names = FALSE)
  all_networks = rbind(all_networks, connections_date)
}

group_network <- all_networks %>%
  group_by(hashtag) %>%
  summarise(degrees=sum(weighted_Indegree))

write.csv(group_network, "all_hashtag_network.csv", row.names = FALSE)
