library(networkly)
ds <- read.csv(file.choose())


threads_to_network <- function(et,TN,CF){
  #
  # CF<-'actor'
  # et <- ds
  # TN <- 'threadNum'
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
  node_position_x = aggregate(et$time, list(et$actor), mean)
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

eventNetwork <- function(et,TN, CF){

  # first convert the threads to the network
  n = threads_to_network(et,TN, CF)

#  title_phrase = paste("Estimated complexity index =",estimate_network_complexity(n))

  # print("nodes")
  # print(n$nodeDF)
  #
  # print("edges")
  # print(n$edgeDF)

  # return(visNetwork(n$nodeDF[,1:3], n$edgeDF[,1:3], width = "100%", main=title_phrase) %>%
  #          visEdges(arrows ="to",
  #                   color = list(color = "black", highlight = "red")) %>%
  #          visLayout(randomSeed = 12 ) %>%  # to have always the same network
  #          visIgraphLayout(layout = "layout_in_circle") %>%
  #          #      visIgraphLayout(layout = "layout_as_tree") %>%
  #          visNodes(size = 10) %>%
  #          visOptions(highlightNearest = list(enabled = T, hover = T),
  #                     nodesIdSelection = T)

  #)
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

  network <- plot_ly(x = ~n$nodeDF$x_pos, y = ~n$nodeDF$y_pos, mode = "markers", text = n$nodeDF$label, hoverinfo = "text")

  p <- layout(
    network,
    title = "title_phrase",
    shapes = edge_shapes
  )
  return(p)

}

results<-threads_to_network(ds, 'threadNum', 'actor')
eventNetwork(ds, 'threadNum', 'action')

results$edgeDF

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

edge_shapes




conn<-1 # average number of conenctions per variable
nodes<-10 # number of variables
net_size<-conn*nodes
edge_type<-2 # number of diffrent connections


#color/size
set.seed(555)
id<-factor(sample(1:edge_type,net_size,replace = TRUE))
id2<-factor(sample(1:10,nodes,replace = TRUE))

edge.list<-data.frame(source=sample(1:nodes,net_size,replace=TRUE),
                      target=sample(1:nodes,net_size,replace=TRUE),
                      color=rainbow(edge_type)[id],
                      size=sample(seq(1,10,length.out=10),edge_type,replace=TRUE)[id],
                      names=letters[id],stringsAsFactors = FALSE)
node.data<-data.frame(color=sample(rainbow(10),nodes,replace=TRUE)[id2],
                      size=sample(seq(5,15,length.out=10),nodes,replace=TRUE)[id2],
                      names=sample(LETTERS[1:5],nodes,replace=TRUE)[id2],stringsAsFactors = FALSE)

layout<-"fruchtermanreingold"
#net params
type<-"2d"
color<-'color'
size<-'size'
name<-'names'

#create network objects
obj<-get_network(edge.list,type=type,layout=layout)
net<-c(get_edges(obj,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=FALSE),get_nodes(obj,node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=FALSE))

#add legend
legend<-format_legend(obj,node.data=node.data)

net<-c(net,c(get_edges(legend,color=color,width=size,name=name,type=type,hoverinfo="none",showlegend=TRUE),get_nodes(legend,node.data=legend$node.data,color=color,size=size,name=name,type=type,hoverinfo="name",showlegend=TRUE)))


net<-shiny_ly(net) # works in or out of shiny

#add layout options
layout(net,
       xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'),
       yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, hoverformat = '.2f'))
shiny_ly()
