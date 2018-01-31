ds <- read.csv(file.choose())

#Rewritten threads to network function to play nicely with plotly - positions of nodes here
#This would be the place to put additional time measurements
#Event time is sequence number
#Size could be
#Color could be
#Nodes are defined in terms of combinations of factors
threads_to_network <- function(et,TN,CF){
  #
   CF<-'actor'
   et <- ds
   TN <- 'threadNum'
  et$time<-as.numeric(et$tStamp)
  # First get the node names & remove the spaces
  node_label = unique(et[[CF]])
  node_label=str_replace_all(node_label," ","_")

  # print("node_label")
  # print(node_label)

  # set up the data frames we need to draw the network
  nodes = data.frame(
    id = 1:length(node_label),
    label = node_label,
    title=node_label)

  node_position_y = data.frame(table(et[[CF]]))
  colnames(node_position_y) <- c('label', 'y_pos')
  node_position_x = aggregate(et$time, list(et[[CF]]), mean)
  colnames(node_position_x) <- c('label', 'x_pos')

  nodes = merge(nodes, node_position_y, by=c("label"))
  nodes = merge(nodes, node_position_x, by=c("label"))

  # get the 2 grams for the edges
  ngdf = count_ngrams(et,TN, CF, 2)


  # need to split 2-grams into from and to
  from_to_str = str_split(str_trim(ngdf$ngrams), " ", n=2)

  # need to find a better way to do this...
  nEdges = length(from_to_str)
  from_labels=matrix(data="", nrow=nEdges,ncol=1)
  to_labels =matrix(data="", nrow=nEdges,ncol=1)
  from=integer(nEdges)
  to=integer(nEdges)
  for (i in 1:length(from_to_str)){

    # Get from and to by spliting the 2-gram
    from_labels[i] = str_split(from_to_str[[i]]," ")[1]
    to_labels[i] = str_split(from_to_str[[i]]," ")[2]

    # use match to lookup the nodeID from the label...
    from[i] = match(from_labels[i], nodes$label)
    to[i] = match(to_labels[i], nodes$label)
  }

  edges = data.frame(
    from,
    to,
    label = paste(ngdf$freq)
  )

  edges = merge(edges, nodes[,c('id', 'y_pos', 'x_pos')], by.x=c('from'), by.y=c('id'))
  edges = merge(edges, nodes[,c('id', 'y_pos', 'x_pos')], by.x=c('to'), by.y=c('id'))
  colnames(edges)<-c('from', 'to', 'label', 'from_y', 'from_x', 'to_y', 'to_x')
  return(list(nodeDF = nodes, edgeDF = edges))
}

#Plotly Function to use information from threads_to_network to put positions of nodes and edges
eventNetwork <- function(n){

  title_phrase = paste("Estimated complexity index =",estimate_network_complexity(n))

  edge_shapes <- list()
  for(i in 1:length(n$edgeDF$from)) {
    E <- n$edgeDF[i,]

    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0.3),
      x0 = E[['from_x']],
      x1 = E[['to_x']],
      y0 = E[['from_y']],
      y1 = E[['to_y']],
      xref = "x",
      yref = "y"
    )

    edge_shapes[[i]] <- edge_shape
  }

  x <- list(
    title = 'Average Time'
  )

  y <- list(
    title = 'Frequency'
  )

  network <- plot_ly(x = ~n$nodeDF$x_pos, y = ~n$nodeDF$y_pos, mode = "markers", text = n$nodeDF$label, hoverinfo = "text")

  p <- layout(
    network,
    title = title_phrase,
    shapes = edge_shapes,
    xaxis = x,
    yaxis = y
  )
  return(p)

}

results<-threads_to_network(ds, 'threadNum', 'action')
results=threads_to_network(ds, 'threadNum', 'actor')
eventNetwork(results)


edge_shapes <- list()
for(i in 1:length(results$edgeDF$from)) {
  E <- results$edgeDF[i,]

  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = E[['from_x']],
    x1 = E[['to_x']],
    y0 = E[['from_y']],
    y1 = E[['to_y']],
    xref = "x",
    yref = "y"
  )

  edge_shapes[[i]] <- edge_shape
}

network <- plot_ly(x = ~results$nodeDF$x_pos, y = ~results$nodeDF$y_pos, mode = "markers", text = results$nodeDF$label, hoverinfo = "text")

p <- layout(
  network,
  title = 'Karate Network',
  shapes = edge_shapes
)
p
