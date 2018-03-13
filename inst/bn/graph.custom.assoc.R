graph.custom.assoc <- function(assocNetwork,nodeNames,EvidenceNode,EventNode,Ndegree,Tlayout,shapeVectorAssoc)
{
  tryCatch({
    nodes <- data.frame(name = nodeNames)
    nodes$id <- 0:(nrow(nodes) - 1)
    nodes$group <- "not in use"
    nodes[which(nodes$name %in% EvidenceNode),3] = "Evidence"
    nodes[which(nodes$name == EventNode),3] = "Event"
    visNodes<- data.frame(id = nodeNames,
                          label = nodeNames,
                          group = nodes$group,
                          shape = shapeVectorAssoc)
    visEdges<- data.frame(from = assocNetwork[,1],
                          to = assocNetwork[,2],
                          title = assocNetwork[,3])
    return(visNetwork(visNodes, visEdges, width = "100%") %>%
             visEdges(smooth = T,color = list(color = "grey",highlight = "black",hover = "black"))%>%
             visGroups(groupname = "not in use", color = list(background = "lightblue",highlight = 'blue', hover = "blue")) %>%
             visGroups(groupname = "Event", color = list(background = "lightgreen",highlight = "green", hover = "green"))%>%
             visGroups(groupname = "Evidence", color = list(background = "pink",highlight = "red", hover = "red")) %>%
             visLegend(width = 0.1, position = "left")%>%
             visNodes(shape = "dot") %>%
             visOptions(highlightNearest = list(enabled =TRUE, degree = Ndegree,hover = T, hideColor = 'rgba(200,200,200,0)'), nodesIdSelection =
                          list(enabled = TRUE, style = 'width: 100px; height: 20px;background: #f8f8f8;border:none;outline:none;'))%>%
             visInteraction(navigationButtons = TRUE)%>%
             visIgraphLayout(layout = Tlayout))
  },error=function(e){
    print(e)
  })

}
