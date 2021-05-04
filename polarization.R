if(!require(igraph))install.packages("igraph")
if(!require(igraph))install.packages("Matrix")
library(igraph)
library(Matrix)

polarization <- function(adjacency_list, edges, memberships) {
  
}

graph_polarization <- function(graph, membership, adjacency_matrix = NULL, weight_attr_name = "weight") {
  if(is.null(adjacency_matrix)) adjacency_matrix = as_adjacency_matrix(graph, attr = weight_attr_name)
  community_names = unique(unlist(membership)); nb_comm = length(community_names)
  mask_matrix = Matrix(0, nrow = vcount(graph), ncol = nb_comm, sparse = TRUE)
  colnames(mask_matrix) = community_names
  for(i in 1:length(membership)) for(j in 1:length(membership[[i]])) mask_matrix[i,as.character(membership[[i]][[j]])] = 1
  structural_matrix = adjacency_matrix %*% mask_matrix
  print(mask_matrix)
  print(structural_matrix)
}

#' Return indicatives to conclude about polarization on a graph built with igraph from nodes and edges dataframes
#' 
#' @param df_nodes Dataframe containing nodes and their attributes. Columns format: id, <wanted attributes>. Watch out, no attribute called 'name'.
#' @param df_edges Dataframe containing edges and their attributes. Columns format: from, to, weight (if weighted), <wanted attributes>. from and to columns contains nodes Ids.
#' @param directed Boolean, TRUE if directed, FALSE otherwise.
#' 
#' @return A structure containing the graph ($graph), the antagonism matrix ($antagonism_matrix), the boundaries members ($boundaries), the internal areas' members ($internals) and the porosity values ($porosity)
graph_polarization_from_dataframes <- function(df_nodes, df_edges, directed = FALSE) {
  graph = graph_from_data_frame(df_edges, directed=directed, vertices=df_nodes)
  res_object = graph_polarization(graph)
  res_object$graph = graph
  return (res_object)
}