##########################################################################################################
# THREADNET Graphics functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# graphic functions used in Shiny App.
# some plotly, but some from other packages


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
#'
#' @return  plotly object
#' @export
#'
threadMap <- function(or,TN, timescale, CF){

  # setting color palettes
  # first find the number of distinct colors
  nColors = length(unique(or[,CF]))
  pal <- diverge_hcl(nColors)

  return( plot_ly(or, x = ~or[[timescale]], y = ~or[[TN]], color= ~or[,CF],
             colors=pal,
             name = 'threads', type = 'scatter', mode='markers', marker=list(size=5, opacity=1), # fill='tonextx',
             symbol= "line-ew", showlegend=FALSE)
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


  library(ngram)
  library(plotly)

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
#'
#' @return visnetwork object
#' @export
eventNetwork <- function(et,TN, CF){

  # first convert the threads to the network
  n = threads_to_network(et,TN, CF)

  title_phrase = paste("Estimated complexity index =",estimate_network_complexity(n))

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


################################################################
##  Here is the networkD3 version of the same thing.
# it has a bunch of extra code because of the groups...
# needs to be re-written to use the network function

#' NetworkD3 force layout for event network
#'
#' Should be replaced with a more expressive layout in plotly
#'
#' @family ThreadNet_Graphics
#'
#' @param et dataframe with the threads to be graphed
#' @param TN the column with the threadNumber
#' @param grp used to color some of the nodes
#' @param zoom_level this is just the contextual factor (column) to be graphed
#'
#' @return networkD3 object
#' @export
#'
eventNetworkD3 <- function(et,TN, grp, zoom_level){
  # et is a dataframe of event threads
  # TN is the column that holds the threadNumber
  # grp is one of the comparison columns
  # zoom_level is the column with the event code (node ID)


  # First get the node names & remove the spaces
  node_label = unique(et[[zoom_level]])
  node_label=str_replace_all(node_label," ","_")
  nNodes = length(node_label)

  node_group=character()
  for (n in 1:nNodes){
    node_group = c(node_group, as.character(unlist( et[which(et[[zoom_level]]==node_label[n]),grp][1]) ) )
  }


  # print("node_label")
  # print(node_label)

  # set up the data frames we need to draw the network
  # needs to have name, group, size
  nodes = data.frame(
    id = 1:length(node_label),
    label = node_label,
    Group = node_group,
    Title=node_label)

  # zero indexing
  nodes$id = nodes$id-1

  # print("nodes")
  # print(nodes)


  # Only count in threads where length is adequate
  text_vector = vector(mode="character")
  j=0
  for (i in unique(et[[TN]])){
    txt =et[et[[TN]]==i,zoom_level]
    # length needs to be longer than n
    if (length(txt)>2){
      j=j+1
      text_vector[j] = concatenate(txt,collapse = "|", rm.space = TRUE)
    }
  }

  # print("text_vector")
  # print(text_vector)

  # try using ngram to get edges since it is so fast
  ngdf = get.phrasetable(ngram(text_vector,2,sep = "|"))

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

  # zero indexing for D3
  from=from-1
  to=to-1

  # ideally need to have source, target, value
  edges = data.frame(
    from,
    to,
    Value = paste(ngdf$freq)
  )

  # print("edges")
  # print(edges)

  return( forceNetwork(Links = edges, Nodes = nodes, Source = "from",
                       Target = "to", Value = "Value", NodeID = "id",
                       Group = "Group", opacity = 1, zoom = T, bounded = T))
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
#' @param CF contextul factors
#' @param CF_levels  list of levels from whicheve contextual factor was chosen for comprisons (e.g., location =1, 2, 3)
#' @param nTimePeriods how many time periods to divide the data?
#' @param ng_size size of ngram
#' @param zoom_level choose the zoom level, if applicable
#'
#' @return plotly object, including subplots
#' @export
#'
#' @examples
Comparison_Plots <- function(e, CF, CF_levels, nTimePeriods=1, ng_size , zoom_level){

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

  # print("time_buckets")
  # print(time_buckets)

  # further divide each by context factor

  # plotList <- function(nplots) {
  #   lapply(seq_len(nplots), function(x) plot_ly())

  plot_list = list()

  for (tb in 1:nTimeBuckets){
    for (f in 1:nLevels){

      plotName = paste0("Time-",tb,"-","CF-",f)

      # print("plotName")
      # print(plotName)
      # print("CF")
      # print(CF)

      plot_bucket = et[(is.element(et$threadNum,unlist(time_buckets[tb])) & et[CF]==CF_levels[f]),"threadNum"]

      # print("plot_bucket")
      # print(plot_bucket)

      # then make the subplots
      # this gets all of the events in all of the threads that match the criteria

      dfp= e[is.element(e$threadNum,unlist(plot_bucket)),]
      # print("dfp[,1:7]")
      # print(dfp[,1:7])

      # ideally make sure at least one thread is long enough to do an ngram...
      if (nrow(dfp)>ng_size){

        plot_list[[plotName]] =  ng_bar_chart(dfp,"threadNum", zoom_level, ng_size, 1)
        #      plot_list[[plotName]] =  eventNetworkD3(dfp,"threadNum", get_COMPARISON_CF(), zoom_level)

      } else
      {plot_list[[plotName]] =  plotly_empty()}


    }
  }

  # create two versions: one that uses ngrams (1-2-3) and one that shows numbers.
  return(subplot(plot_list,nrows=nLevels))
}


###################################################################

#' Use TraMiner plotting function to produce threadmap
#'
#' Would like to re-implement in plotly for mouse-over and for better interactivity and speed
#'
#' @family ThreadNet_Graphics
#'
#' @param df
#' @param TN
#' @param CF
#'
#' @return standard R plot
#' @export
traminer_threadMap <- function(df,TN, CF){

  # setting color palettes
  # first find the number of distinct colors
  nColors = length(unique(df[,CF]))
  pal <- diverge_hcl(nColors)

  #  reformat the data for traminerR
  df = convert_TN_to_TramineR(df, TN, CF)


  #plot sequence - also try seqfplot?
  # add grouping variable?
  return(seqiplot( seqdef(df, cpal=pal) , withlegend = T, main = "ThreadMap", border = NA,idxs=1:nrow(df)) )

  #  return(seqfplot( seqdef(df, cpal=pal) , withlegend = T, main = "ThreadMap", border = NA,idxs=1:nrow(df)) )


}

# This one is not currently used.
threadLengthBarchart <- function(o, TN){


  sizes = threadSizeTable(o,TN)

  tgbc <- plot_ly( sizes, x = ~Var1, y = ~Freq, type = "bar", showlegend=FALSE) %>%
    layout(xaxis= list(showticklabels = TRUE, title=paste0("Distribution of thread length")))

  return(tgbc)
}
