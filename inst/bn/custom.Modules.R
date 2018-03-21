custom.Modules<-function(graph,weight)
{
  lc <- getLinkCommunities(graph, directed = TRUE, dirweight = weight)
  num_communities <- lc$numbers[3]
  community_list <- vector(mode="list", length=num_communities)
  for(i in 1:num_communities)
  {
    community_list[[i]] <- getNodesIn(lc, clusterids = c(i))
  }
  return(community_list)
}
