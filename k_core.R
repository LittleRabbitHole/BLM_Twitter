###
# 5. K-CORES
###

# The graph.coreness() function in igraph returns a vector containing 
# the degree of the highest-degree k-core to which each vertex 
# belongs. 
coreness = graph.coreness(m182_task)
coreness

# Note that the output of graph.coreness refers simply to the *degree*
# of the k-core, not to the k-core itself; thus, two vertices both with 
# coreness of 3 may not be connected at all and thus may be in separate
# k-cores. 
#
# One way to get a sense of the actual k-core structure is to simply 
# plot the graph and color-code by k-core:
make_k_core_plot <- function (g) {
  lay1 <- layout.fruchterman.reingold(g)
  plot(g, 
       vertex.color = graph.coreness(g), 
       layout=lay1, 
       edge.arrow.size = .5)
} 

make_k_core_plot(m182_friend)
make_k_core_plot(m182_social)
make_k_core_plot(m182_task)

# Here's an artificial example showing two separate 3-cores:
g1 <- graph.ring(10)
g1 <- add_edges(g1, c(1,3, 2,4, 1,4, 6,8, 6,9, 7,9))
graph.coreness(g1)
make_k_core_plot(g1)

# Question #6 - What's the difference between K-cores and cliques? 
# Why might one want to use one over the other?


###
# EXTRA CREDIT:
# igraph's built-in functionality merely shows the the degree of the
# highest-degree k-core to which each vertext belongs. However, 
# vertices with the same coreness may be part of different k-cores,
# as shown in the artifical 3-core example above.
#
# Can you figure out a way to to generate not only the coreness of 
# each vertex, but also an indicator of which specific k-core the 
# vertex belongs to (and thus which other vertices are in the same 
# k-core)?
###