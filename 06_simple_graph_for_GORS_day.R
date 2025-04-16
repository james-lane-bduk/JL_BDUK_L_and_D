library(igraph)


#Create graph:

simple_df <- data.frame(from = c("A", "A", "B", "B", "C", "C", "D", "D"), 
                        to = c("B", "C", "D", "E", "E", "F", "E", "G"),
                        weight = c(2, 4, 3, 4, 2, 3, 5, 3))

simple_df_graph <- graph.data.frame(simple_df, directed = FALSE)


#a) Shortest Path from A to G
shortest_A_G <- igraph::shortest_paths(simple_df_graph, "A", "G", output="both", weights=NULL, algorithm = "dijkstra")



#b) MST 
mst <- igraph::mst(simple_df_graph, algorithm = 'prim')


