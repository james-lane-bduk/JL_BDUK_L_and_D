library(igraph)
library(dplyr)
library(glue)


#Create graph:

simple_df <- data.frame(from = c("A", "A", "B", "B", "C", "C", "F", "D", "D", "G", "G", "E"), 
                        to = c("B", "C", "D", "E", "E", "F", "H", "E", "G", "I", "E", "I"),
                        weight = c(2, 6, 1, 7, 2, 3, 1, 5, 3, 3, 1, 1))

simple_df_graph <- graph.data.frame(simple_df, directed = FALSE)

plot(simple_df_graph, edge.label = E(simple_df_graph)$weight, layout=layout_with_fr)


#a) Shortest Path from A to I
shortest_A_I <- igraph::shortest_paths(simple_df_graph, "A", "I", output="both", weights=NULL, algorithm = "dijkstra")

shortest_path_edges <- E(simple_df_graph, path = shortest_A_I$vpath[[1]])

#Plot original graph but colour code shortest path edges in red
plot(simple_df_graph, edge.label = E(simple_df_graph)$weight, 
     edge.color = if_else(E(simple_df_graph) %in% shortest_path_edges, "red", "black"), layout=layout_with_fr)





#b) MST 
mst <- igraph::mst(simple_df_graph, algorithm = 'prim')

plot(mst, edge.label = E(simple_df_graph)$weight)



#c) Kou's Algorithm (Sonic Version) - imagine edges C-F and D-E are mandatory, as are nodes A and I (start/end points)
target_list <- c("A", "I", "D", "E", "C", "F")

#Need to set weight of 'mandatory' edges to 0, otherwise we could end up going the long way round unecessarily & not actually including the mandatory edge
simple_df_mod <- simple_df %>%
    mutate(weight = if_else((from == 'C' & to == 'F') | (from == 'D' & to == 'E'), 0, weight))


#Turn into graph again:
simple_df_graph <- graph.data.frame(simple_df_mod, directed = FALSE)


#Compute distance graph
dist_df <- igraph::distances(simple_df_graph, target_list, target_list, weights = NULL, algorithm = "dijkstra")

#Reshape from a matrix into an actual dataframe (i.e. pivot longer)
dist_df <- reshape2::melt(dist_df)

#And re-label the variables
colnames(dist_df) <- c("from", "to", "weight")

#Now create a graph object from dist_df, and compute the Minimum Spanning Tree Graph of it via Prim's algorithm - just to get target edge pairs.
dist_graph <- igraph::graph.data.frame(d=dist_df, directed = FALSE)
dist_mst <- igraph::mst(dist_graph, algorithm = 'prim')

#Store edge pairs
mst_edge_list <- igraph::get.edgelist(dist_mst)

#Now find shortest path connecting each MST 'edge pair' in context of original graph
steiner_edge_list <- c()
    for(row_i in 1:length(mst_edge_list[,1])){
        i <- mst_edge_list[row_i,1]
        j <- mst_edge_list[row_i,2]
        path <- igraph::shortest_paths(simple_df_graph, i, to = j, output = "both", weights = NULL, algorithm = 'dijkstra')
        steiner_edge_list <- c(steiner_edge_list, as_ids(path$epath[[1]]))
    }

#Remove duplicates
steiner_edge_list <- unique(steiner_edge_list)

simple_df_final <- simple_df %>%
    mutate(steiner_edge = if_else((glue("{from}|{to}") %in% steiner_edge_list)
                  |(glue("{to}|{from}") %in% steiner_edge_list), 1, 0))

#Now plot original graph showing Steiner Edges:
simple_df_graph <- graph.data.frame(simple_df_final, directed = FALSE)


plot(simple_df_graph, edge.label = E(simple_df_graph)$weight, 
     edge.color = if_else(simple_df_final$steiner_edge == 1, "red", "black"), layout=layout_with_fr)


