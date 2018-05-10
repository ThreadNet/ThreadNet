##########################################################################################################
# THREADNET Graphics functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# graphic functions used in Shiny App.
# some plotly, but some from other packages
library(plotly)

###### Pie charts for context factors  ####
# It would be nice to display some other helpful information, perhaps (like the % of possible combinations that occur)

#' Creates pie charts for one or more contextual factors
#'
#' When selecting contextual factors that define threads, events and comparisons, this function provide visual feedback about the number of factors levels
#' and also the number of levels when the factors are combined
#'
#' @family ThreadNet_Graphics
#'
#' @param oc data frame of occurrences
#' @param CF list of contextual factors (columns) to include in the display
#'
#' @return  plotyly pie charts (one or more)
#'
#' @export
CF_multi_pie <- function(oc,CF){

  # avoid unpleasant error messages
  if (length(CF)==0) {return(plotly_empty())}

  # first add the combined column if there is more than one
  if (length(CF) >1){
    oc = combineContextFactors(oc, CF, "COMBINED")
    CF = c(CF, "COMBINED") }

  # get number of plots, which now includes the combined plot
  nPlots = length(CF) #  nPies+1

  ### compute layout information

  # compute the offset = half the width of each plot
  offset = 1/(2*nPlots)

  # Locate the centers for the plots -- this is where the annotations will go
  ctrPlot = (0:(nPlots-1))/nPlots + offset

  # locate upper and lower bounds on the domains of the plots (LB & UB)
  plotDomainLB = ctrPlot - offset
  plotDomainUB = ctrPlot + offset


  # Now loop for each CF, computing entropy and adding on the next "trace" to the plot
  # start with blank plot object
  pies = plot_ly()
  max_combos = 1
  for (i in 1:nPlots) {

    # make table information for each plot, including the combined one
    cfData = as.data.frame(table(oc[CF[i]]))

    # take out rows with zero frequency
    cfData = cfData[(cfData[,"Freq"]>0),]

    #N levels
    CFlevels = length(cfData[,"Freq"])

    # keep track of max possible combinations
    max_combos =  max_combos*CFlevels

    #compute entropy for each plot
    CFentropy = compute_entropy(cfData[,"Freq"])


    # Add the new plots
    pies = pies  %>%
      add_pie(data = cfData, labels = ~Var1, values = ~Freq,
              textinfo='label',textposition='none', name=as.character(CF[i]),
              domain = list(x = c(plotDomainLB[i], plotDomainUB[i])) ) %>%
      add_annotations(text=paste0(CF[i],"<br>N=",CFlevels,"<br>entropy=",format(CFentropy, digits=2)),showarrow=FALSE,xanchor="center",
                      font=list(size="14",color="white"),
                      xref="paper",yref="paper",y=.5,x=ctrPlot[i])
  }

  pies = pies %>%
    layout(showlegend=FALSE,
           xaxis = list(showgrid = FALSE,zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE)
           # ,
           # autosize = F, width = "100%", height = "100px")
    )
  return(pies)
}

####################################################
# use the same general layout, but just for one event
# need to pass in the levels for each CF to use as the column names
# e is a data.frame with events (created by OccToEvents1 or OccToEvents2)
# CF = list of context factor names used to define events
# r is the row name for the event being examined
#
# KNOWN ISSUES:
#  * Need to pass in the factor levels as labels for the pie slices
#  * Need to compute the values differently for a node in the dendrogram or in a zoomed graph
#  * Probably need to pass in the vector of values
#
#  Call this for one CF at a time
# o is the raw occurrences.  This is where we get the labels.
# e is the events.  This is where we get the frequencies
# cf is the column name for one CF (e.g., "actor")
# r is one row (or cluster ID)
# zm is the zoom column number.
make_df_for_one_pie <- function(o,e,cf,r,zm){

  # get the labels from the occurrences (o), get the frequencies from events
  cfdf = data.frame(Freq = aggregate_VCF_for_cluster(e,cf,r,zm), Label= levels(o[[cf]]) )

  return(cfdf)
}


# e = events
# o = occurrences
# CF = list of the event_CF
# zoom level as an integer (so you can grab it from the slider)
# r = row number or cluster number.  Should be the number on the event
# z = integer for zoom column
CF_multi_pie_event <- function(o, e,CF,r, zm){

  # avoid unpleasant error messages
  if (length(CF)==0) {return(plotly_empty())}

  # get number of plots
  nPlots = length(CF)

  # paste "V_" onto the contextual factor names
  # CF = paste0("V_",CF)

  ### compute layout information

  # compute the offset = half the width of each plot
  offset = 1/(2*nPlots)

  # Locate the centers for the plots -- this is where the annotations will go
  ctrPlot = (0:(nPlots-1))/nPlots + offset

  # locate upper and lower bounds on the domains of the plots (LB & UB)
  plotDomainLB = ctrPlot - offset
  plotDomainUB = ctrPlot + offset

  n=length

  # Now loop for each CF, computing entropy and adding on the next "trace" to the plot
  # start with blank plot object
  pies = plot_ly()
  max_combos = 1
  for (i in 1:nPlots) {

    # make table information for each plot
    # cfData = data.frame(Freq=as.matrix(unlist(e[r,CF[i]])),Var1= letters[seq( from = 1, to = length(unlist(e[r,CF[i]])) )])
    cfData = make_df_for_one_pie(o,e,CF[i],r,zm)

    # take out rows with zero frequency
    cfData = cfData[(cfData[,"Freq"]>0),]

    #N levels
    CFlevels = length(cfData[,"Freq"])

    # Add the new plots
    if(CFlevels==1){
      pies = pies  %>%
        add_pie(data = cfData, labels = ~Label, values = ~Freq,
                textinfo='label',textposition='none', name=as.character(CF[i]),
                domain = list(y = c(plotDomainUB[i], plotDomainLB[i])) ) %>%
        add_annotations(text=paste0(CF[i],"<br>",cfData$Label),showarrow=FALSE,xanchor="center",
                        font=list(size="14",color="white"),
                        xref="paper",yref="paper",y=ctrPlot[i],x=.5)
    } else {
    pies = pies  %>%
      add_pie(data = cfData, labels = ~Label, values = ~Freq,
              textinfo='label',textposition='none', name=as.character(CF[i]),
              domain = list(y = c(plotDomainUB[i], plotDomainLB[i])) ) %>%
      add_annotations(text=paste0(CF[i],"<br>N=",CFlevels),showarrow=FALSE,xanchor="center",
                      font=list(size="14",color="white"),
                      xref="paper",yref="paper",y=ctrPlot[i],x=.5)
    }
  }
  pies = pies %>%
     layout(showlegend=FALSE,
            xaxis = list(showgrid = FALSE,zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE,showticklabels = FALSE)
            # ,
            # autosize = F, width = "100%", height = "100px")
     )
  return(pies)
}


######################################################################
# ThreadMap shows the threads in a horizongal layout
#' Shows threads in a horizontal layout
#'
#' Creates a plotly chart of threads in either clock time or event time, depending on the timescale parameter.
#'
#' @family ThreadNet_Graphics
#'
#' @param or Dataframe of threads
#' @param TN name of column with thread number
#' @param timescale name of column that will be used to plot x-axis of events. It can be the can be the time stamp (for clock time) or the sequence number (for event time)
#' @param CF name of contextual factor that will determine the colors
#' @shape shape of plotted points
#'
#' @return  plotly object
#' @export
#'
threadMap <- function(or, TN, timescale, CF, shape){


  # print('in threadMap')
  # print(head(or))

  # setting color palettes
  # first find the number of distinct colors
  nColors = length(unique(or[,CF]))
  pal <- diverge_hcl(nColors)

  if (timescale == 'seqNum') {
    xaxis = 'Event Time'
  } else if (timescale == 'tStamp') {
    xaxis = 'Actual Time'
  } else if (timescale == 'relativeTime') {
    xaxis = 'Relative time'
  }

  return( plot_ly(or, x = ~as.integer(or[[timescale]]), y = ~or[[TN]], color= ~as.character(or[,CF]),
             colors=pal,
             name = 'threads', type = 'scatter', mode='markers', marker=list(size=10, opacity=1), # fill='tonextx',
             text = ~or$label,
             hoverinfo = "text+x+y",
             symbol= "line-ew", symbols=shape, showlegend=FALSE)
             %>%
               layout(
                 xaxis = list(title = xaxis),
                 yaxis = list(title = knitr::combine_words(get_THREAD_CF(), sep = ", "))
               )
        )
}


################################################
#' Create an ngram bar chart
#'
#' Shows the n-grams within a set of threads (but not splitting across threads). This provides a visual indication of how repetitive the threads are.
#'
#' @family ThreadNet_Graphics
#'
#' @param o a dataframe of occurrences or events
#' @param TN the column that contains the threadNum
#' @param CF the contextual factor within which to count the n-grams
#' @param n the length of the ngram
#' @param mincount the minimum count to display
#'
#' @return plotly object
#' @export
#'
#' @examples
ng_bar_chart <- function(o,TN, CF, n, mincount){

  # get the ngrams
  ngdf = count_ngrams(o,TN, CF, n)

  # print("ngdf")
  # print(ngdf)

  # put them in descending order -- tricky (http://stackoverflow.com/questions/40224892/r-plotly-barplot-sort-by-value)
  ngdf$ngrams = factor(ngdf$ngrams, levels =unique(ngdf$ngrams)[order(ngdf$freq, decreasing = TRUE)])

  # make a list so we can return the data and the plot
  ng=new("list")

  # only include if they occur more than the threshold
  ngBars = ngdf[ngdf$freq>=mincount,]

  ngp <- plot_ly( ngBars, x = ~ngrams, y = ~freq, type = "bar",showlegend=FALSE) %>%
    layout(xaxis= list(showticklabels = FALSE, title=paste0(n,"-grams of ",CF, " that occur > ",mincount," times")))

  return(ngp)
}

# this version we pass in the ngram data frame already sorted
ng_bar_chart_freq <- function(ngdf){

  # put them in descending order -- tricky (http://stackoverflow.com/questions/40224892/r-plotly-barplot-sort-by-value)
  ngdf$ngrams = factor(ngdf$ngrams, levels =unique(ngdf$ngrams)[order(ngdf$freq, decreasing = FALSE)])

  ngp <- plot_ly( ngdf, y = ~ngrams, x = ~freq, type = "bar",showlegend=FALSE) %>%
    layout(xaxis= list(showticklabels = TRUE, title='Frequency'))  %>%
    layout(yaxis= list(showticklabels = FALSE, title=''))

  return(ngp)
}


#############################################################################
#' Circular network layout for event network (USES visnetwork)
#'
#' Should be replaced with a more expressive layout in plotly
#'
#' @family ThreadNet_Graphics
#'
#' @param et dataframe with the threads to be graphed
#' @param TN the column with the threadNumber
#' @param CF is the contetual factors (column)
#' @param timesplit time measure
#'
#' @return plotly object
#' @export

eventNetwork <- function(et, TN, CF, timesplit){

  n <- threads_to_network(et, TN, CF, timesplit)


  title_phrase = paste("Estimated complexity index =",estimate_network_complexity(n))

  edge_shapes <- list()
  for(i in 1:length(n$edgeDF$from)) {
    E <- n$edgeDF[i,]

    edge_shape = list(
      type = "line",
      line = list(color = "#030303", width = 0),
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
  color_pal = colorRampPalette(brewer.pal(11,'Spectral'))
  size_pal = (n$nodeDF$y_pos-min(n$nodeDF$y_pos))/(max(n$nodeDF$y_pos)-min(n$nodeDF$y_pos))*15+10
  network <- plot_ly(x = ~n$nodeDF$x_pos, y = ~n$nodeDF$y_pos,
                     width = 0,
                     mode = "markers",
                     marker = list(size= size_pal,
                                   color=color_pal(100)[as.numeric(cut(n$nodeDF$x_pos, breaks=100))]

                     ),
                     text = n$nodeDF$label, key = n$nodeDF$label, hoverinfo = "text", source = 'A')

  p <- layout(
    network,
    title = title_phrase,
    shapes = edge_shapes,
    xaxis = x,
    yaxis = y
  )
  return(p)

}
################################################################
##  Here is the networkD3 version of the same thing.
# it has a bunch of extra code because of the groups...
# needs to be re-written to separate computation of the network from the layout...

#' NetworkD3 layout for event network
#'
#' @family ThreadNet_Graphics
#'
#' @param n = list with data frames for nodes and edges
#'
#' @return networkD3 object
#' @export

forceNetworkD3 <- function(n){

  # zero indexing
  n$nodeDF['id'] =  n$nodeDF['id']-1
  n$edgeDF['from'] =  n$edgeDF['from']-1
  n$edgeDF['to'] =  n$edgeDF['to']-1


  return( forceNetwork(Links = n$edgeDF, Nodes = n$nodeDF, Source = "from",
                       Target = "to", Value = "Value", NodeID = "label",
                       Group = "Group", opacity = 1, zoom = T,arrows=TRUE, bounded = FALSE,
                       clickAction = 'Shiny.onInputChange("Group", d.name)'))
}


######################################################################################
#' Comparison plots
#'
#' Produce a set set of comparison sub-plots in an array.  Ideally, we should be able to use any of the plots. So far it is only bar charts.
#' This is a prototype that could use rather extensive redesign...
#'
#' @family ThreadNet_Graphics
#'
#' @param e dataframe with threads to be plotted
#' @param o dataframe with the original data
#' @param CF contextul factors
#' @param CF_levels  list of levels from whicheve contextual factor was chosen for comprisons (e.g., location =1, 2, 3)
#' @param nTimePeriods how many time periods to divide the data?
#' @param ng_size size of ngram
#' @param zoom_level choose the zoom level, if applicable
#' @param plot_type a type of plotly plot with a function written
#'
#' @return plotly object, including subplots
#' @export
#'
#' @examples
Comparison_Plots <- function(e, o, CF, CF_levels, nTimePeriods=1,  plot_type,role_map_cfs){

  # get the first event of each thread, so we can order them consistently by time
  et = e[e$seqNum==1,]
  et = et[order(et$tStamp),]

  # count the size of everything -- nTimePeriods is giving type error...
  nThreads = nrow(et)
  nLevels = max(1,length(CF_levels))
  nTimeBuckets = as.numeric(max(1,nTimePeriods))
  total_buckets = nLevels * nTimeBuckets

  # Set up the N x M data stuctures to hold the parameters and the plots
  plot_buckets = matrix(rep(list(), total_buckets),nrow = nTimeBuckets , ncol =nLevels)

  # Get the subsets, first by time and then by category.  This will just return thread numbers.
  time_buckets = make_subsets(et$threadNum,nTimeBuckets)

  plot_list = list()

  for (tb in 1:nTimeBuckets){
    for (f in 1:nLevels){

      plotName = paste0("Time-",tb,"-","CF-",f)
      plot_bucket = et[(is.element(et$threadNum,unlist(time_buckets[tb])) & et[CF]==CF_levels[f]),"threadNum"]

      # then make the subplots
      # this gets all of the events in all of the threads that match the criteria
      dfp= e[is.element(e$threadNum,unlist(plot_bucket)),]
      # print("dfp[,1:7]")
      # print(dfp[,1:7])

      # ideally make sure at least one thread is long enough to do an ngram...
      if (nrow(dfp)>2){

        if (plot_type=='Ngrams')
        {plot_list[[plotName]] =  ng_bar_chart(dfp,"threadNum", 1, 2, 2)}
        else if (plot_type=='Role Maps')
        {plot_list[[plotName]] = role_map( dfp, o, role_map_cfs ) }
        else if (plot_type=='Thread Trajectories')
        {plot_list[[plotName]] = threadTrajectory( dfp ) }
         else if (plot_type=='Threads (event time)')
         {plot_list[[plotName]] = threadMap(dfp, "threadNum", "seqNum", 1, 15 ) }
       else
        {plot_list[[plotName]] =  plotly_empty()}
    }
  }}

  # create two versions: one that uses ngrams (1-2-3) and one that shows numbers.
  return(subplot(plot_list,nrows=nLevels))
}



# This one is not currently used.
threadLengthBarchart <- function(o, TN){


  sizes = threadSizeTable(o,TN)

  tgbc <- plot_ly( sizes, x = ~Var1, y = ~Freq, type = "bar", showlegend=FALSE) %>%
    layout(xaxis= list(showticklabels = TRUE, title=paste0("Distribution of thread length")))

  return(tgbc)
}



# Basic Network layout - back from the dead
# accepts the data stucture with nodeDF and edgeDF created by threads_to_network and normalNetwork
circleVisNetwork <- function( n,showTitle=FALSE ){

  if (showTitle==TRUE)
    title_phrase = paste("Estimated complexity index =",round(estimate_network_complexity(n),2))
  else
    title_phrase =''

  # print("nodes")
  # print(n$nodeDF)
  #
  # print("edges")
  # print(n$edgeDF)

  return(visNetwork(n$nodeDF, n$edgeDF, width = "100%", main=title_phrase) %>%
           visEdges(arrows ="to",
                    color = list(color = "black", highlight = "red")) %>%
           visLayout(randomSeed = 12 ) %>%  # to have always the same network
           visIgraphLayout(layout = "layout_in_circle") %>%
           #      visIgraphLayout(layout = "layout_as_tree") %>%
           visNodes(size = 10) %>%
           visOptions(highlightNearest = list(enabled = T, hover = T),
                      nodesIdSelection = T)

  )

}


# e is any set of events
# vcf is the context factor to graph as network for that set of events
# l is the set of labels = factor levels of original data for that VCF
normalNetwork <- function(e, o, cf){

  # First get the node names & remove the spaces just in case
  node_label = levels(o[[cf]])
  node_label=str_replace_all(node_label," ","_")

  # print("node_label")
  # print(node_label)

  # set up the data frames we need to draw the network
  nodes = data.frame(
    id = 1:length(node_label),
    label = node_label,
    title=node_label)

  # get the column name for the vector...
  vcf=paste0('V_',cf)

  # add up the indicators of co-presence in this set of events
  vcf_sum = colSums( matrix( unlist(e[[vcf]]), nrow = length(e[[vcf]]), byrow = TRUE) )

  # compute outer product to get adjacency matrix and then standardize to 0-1
  a=sqrt(vcf_sum %o% vcf_sum)
  a=a/max(a)

  # print(a)
  g=graph_from_adjacency_matrix(a, mode='undirected', weighted=TRUE)

  #E = get.edgelist(g, attr='weight')
  edges=cbind( get.edgelist(g) , round( E(g)$weight, 3 ))

  colnames(edges)=c('from','to','label')


  return(list(nodeDF = nodes, edgeDF = as.data.frame(edges) ))
}


filter_network_edges <- function(n, threshold){
  # print(head(n$edgeDF))
  # print(paste('threshold=',threshold))

  n$edgeDF = n$edgeDF %>% filter(label>threshold)

  # print(head(n$edgeDF))

  return(n)
}

# role map will show "who does what" for any set of events
# cfs contains a list of two contextual factors.
role_map <- function(e, o, cfs){

  if (!length(cfs)==2)
    return(plot_ly())

  # Get the context factors
  vcf_1 = paste0('V_',cfs[1])
  vcf_2 = paste0('V_',cfs[2])

  # add up the indicators of co-presence in this set of events
  vcf_1_sum = colSums( matrix( unlist(e[[vcf_1]]), nrow = length(e[[vcf_1]]), byrow = TRUE) )
  vcf_2_sum = colSums( matrix( unlist(e[[vcf_2]]), nrow = length(e[[vcf_2]]), byrow = TRUE) )

   # compute outer product to get adjacency matrix
  who_does_what= round(vcf_1_sum %o% vcf_2_sum,0)

  return( plot_ly(
          x=levels(o[[cfs[2]]]),
          y=levels(o[[cfs[1]]]),
          z=who_does_what,
          type='heatmap',
          colors= 'Reds') )

}


# this shows relative time versus sequential time
# Inspired by Gergen and Daniger-Schroeder
threadTrajectory <- function(or){

  # setting color palettes
  # first find the number of distinct colors
  nColors = length(unique(or$threadNum))
  pal <- rainbow_hcl(nColors)

  return( plot_ly(or, x = ~or$relativeTime, y = ~or$seqNum, color= as.character(or$threadNum),
                  colors=pal,
                  name = 'threads', type = 'scatter', mode='lines',
                  text = ~paste(or$threadNum,or$label,sep=':'),
                  hoverinfo = "text",
                  showlegend=FALSE)
          %>%
            layout(
              xaxis = list(title='Relative time'),
              yaxis = list(title='Sequence')
            ))
}


movingWindowCorrelation <- function( trace ){
  return( plot_ly(trace, x = ~window, y = ~correlation,
                  name = 'Window', type = 'scatter', mode='lines+markers',
                  text = paste('Thread:',trace$thread),
                  marker=list(size=8, opacity=1),
                  hoverinfo = "text",
                  symbol= "line-ew", symbols=15, showlegend=FALSE
  )
  %>%
    layout(
      xaxis = list(title='Window number'),
      yaxis = list(title='Correlation',
                   range = c(0, 1),
                   autotick = FALSE,
                   ticks = "outside",
                   tick0 = 0,
                   dtick = 0.1,
                   ticklen = 5,
                   tickwidth = 2,
                   showticklabels = TRUE))
  )
}
dualmovingWindowCorrelation <- function( trace ){
  return( plot_ly(trace, x = ~thread, y = ~correlation,
                  name = 'Window', type = 'scatter', mode='lines+markers',
      #          text = ~thread,
                  marker=list(size=8, opacity=1),
       #           hoverinfo = "text",
                  symbol= "line-ew", symbols=15, showlegend=FALSE  #,height = 200
  )
  %>%
    layout(
      xaxis = list(title='Window number'),
      yaxis = list(title='Correlation',
                   range = c(0, 1),
                   autotick = FALSE,
                   ticks = "outside",
                   tick0 = 0,
                   dtick = 0.1,
                   ticklen = 5,
                   tickwidth = 2,
                   showticklabels = TRUE))
  )
}
