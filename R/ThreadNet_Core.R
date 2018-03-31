##########################################################################################################
# THREADNET:  Core functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# These are the basic functions that convert threads to networks, etc.

#' Converts threads to network
#'
#' Converts a sequentially ordered streams of ;events (threads) and creates a unimodal, unidimensional network.
#' Sequentially adjacent pairs of events become edges in the resulting network.
#' @family ThreadNet_Core
#'
#' @param et dataframe containing threads
#' @param TN name of column in dataframe that contains a unique thread number for each thread
#' @param CF name of the column in dataframe that contains the events that will form the nodes of the network
#' @param timesplit time measure
#'
#' @return a list containing two dataframes, one for the nodes (nodeDF) and one for the edges (edgeDF)
#'
#' @export


threads_to_network <- function(et,TN,CF,timesplit){
  et$time = et[[timesplit]]
  #et$time = et$POVseqNum

  #et$time<-as.numeric(et$tStamp)
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

# Counting ngrams is essential to several ThreadNet functions
#' Counts ngrams in a set of threads
#'
#' This function counts n-grams within threads where the length of the thread is greater than n.
#' @family ThreadNet_Core
#'
#' @param o dataframe containing threads
#' @param TN name of column in dataframe that contains a unique thread number for each thread
#' @param CF name of the column in dataframe that contains the events that will form the nodes of the network
#' @param n length of ngrams to count
#'
#' @return a dataframe with ngram, frequency and proportion in descending order
#'
#' @export
count_ngrams <- function(o,TN,CF,n){

  # print("TN")
  # print(TN)
  # print("CF")
  # print(CF)
  # print("o")
  # print(o)

  # Cannot put all the values in one long string.  Need a vector of strings, one for each thread
  text_vector = vector(mode="character")

  # Only count in threads where length is adequate
  j=0
  for (i in unique(o[[TN]])){
    txt =o[o[[TN]]==i,CF]
    # length needs to be longer than n
    if (length(txt)>n){
      j=j+1
      text_vector[j] = concatenate(o[o[[TN]]==i,CF])
    }
  }

   # print("text_vector")
   # print(text_vector)

  # return a data frame that includes the ngrams
  #    col 1 = ngrams
  #    col 2 = freq
  #    col 3 = prop (proportion that matches)

  return(get.phrasetable(ngram(text_vector,n)))
}


#################################################################
#
#' Make new threads from a new POV
#'
#' Take the raw occurrences from the input file and sort them by time stamp within
#' a set of contextual factors that remain constant for each thread.
#' @family ThreadNet_Core
#'
#' @param  o is the dataframe of cleaned ocurrences
#' #' @param  THREAD_CF is a list of 1 or more context factors that define the threads (and stay constant during each thread)
#' @param  EVENT_CF is a list of 1 or more context factors that define events (and change during threads)
#'
#' @return dataframe containing the same occurrences sorted from a different point of view
#'
#'@export
ThreadOccByPOV <- function(o,THREAD_CF,EVENT_CF){

  # make sure there is a value
  if (length(THREAD_CF) == 0){return(o)}

  # Sort by POV and timestamp. The idea is to get the stream of activities from
  # a particular point of view (e.g., actor, location, etc.)
  # add the new column that combines CFs, if necessary

  # get a new column name based on the thread_CF -- use this to define threads
  nPOV = newColName(THREAD_CF)
  occ = combineContextFactors(o,THREAD_CF,newColName(THREAD_CF))

  # print("nPOV")
  # print(nPOV)
  #
  # print("THREAD_CF")
  # print(THREAD_CF)

  # The event context factors define the new category of events within those threads
  occ = combineContextFactors(occ,EVENT_CF,newColName(EVENT_CF))
  occ = occ[order(occ[nPOV],occ$tStamp),]

  # add two columns to the data frame
  tNum = integer(nrow(occ))
  oNum = integer(nrow(occ))

  # CONSIDER USING variable normal names (drop POV here)
  occ$POVthreadNum = tNum
  occ$POVseqNum =   oNum

  # Also add columns for the time gaps and handoff gaps that appear from this POV
  timeGap = diff_tStamp(occ$tStamp)
  handoffGap = diff_handoffs(occ[EVENT_CF])

  occ$timeGap  =  timeGap
  occ$handoffGap = handoffGap

  # create new column for relative time stamp. Initialize to absolute tStamp and adjust below
  occ$relativeTime = lubridate::mdy_hms(occ$tStamp)

  # then get the unique values in that POV
  occ[nPOV] = as.factor(occ[,nPOV])
  pov_list = levels(occ[[nPOV]])


  # now loop through the pov_list and assign values to the new columns
  start_row=1
  thrd=1
  for (p in pov_list){

    # get the length of the thread
    tlen = sum(occ[[nPOV]]==p)

    # guard against error
    if (tlen>0){

      #compute the index of the end row
      end_row = start_row+tlen-1

      # they all get the same thread number and incrementing seqNum
      occ[start_row:end_row, "POVthreadNum"] <- as.matrix(rep(as.integer(thrd),tlen))
      occ[start_row:end_row, "POVseqNum"] <- as.matrix(c(1:tlen))


      # find the earliest time value for this thread
      start_time = min(lubridate::mdy_hms(occ$tStamp[start_row:end_row]))
      print(start_time)

      # subtract that from all of the time stamps -- I can't get this to work...
      # for (t in start_row:end_row){
      #   occ$relativeTime[t] = lubridate::mdy_hms(occ$tStamp[t]) -  start_time
      # }

      # increment the counters for the next thread
      start_row = end_row + 1
      thrd=thrd+1
    } # tlen>0
  }
  return(occ)
}


##############################################################################################################
#' Maps occurrences into events
#'
#' Thus function provides a place to map occurrences into events, so is is not necessary to interpret individual
#' occurrences in isolation.  There are many ways to accomplish this mapping.
#' @family ThreadNet_Core
#'
#' @param  o  a dataframe of occurrences
#' @param mapping = one-to-one or clustering
#' @param m = method parameter = one of c('Variable chunks','Uniform chunks')
#' @param uniform_chunk_size = used to identify breakpoints -- from input slider
#' @param tThreshold = used to identify breakpoints -- from input slider
#' @param EventMapName = used to store this mapping in an environment (not used yet)
#' @param chunk_CF - context factors used to delineate chunks
#' @param EVENT_CF - context factors used to define events
#' @param compare_CF = context factors used for comparison -- need to be copied over here when the thread is created.
#' @param timescale hours, min or sec
#'
#' @result event data frame, with occurrences aggregated into events.  Dataframe includes: threadNum, seqNum,
#' and set of columns E_1, E_2, ..., that indicate the membership of events in clusters of events.
#'
#' @export
OccToEvents1 <- function(o,EventMapName,EVENT_CF, compare_CF){


  # Only run if eventMapName is filled in; return empty data frame otherwise
  if (EventMapName ==""){return(data.frame())}

  # we are mapping one-to-one, so copy the input to the output and then add/rename some other columns as needed
  e=o

   # occurrences have no duration
   o["eventDuration"] = 0

    # rename the threadNum and seqNum columns
    names(e)[names(e)=="POVthreadNum"] <- "threadNum"
    names(e)[names(e)=="POVseqNum"] <- "seqNum"

    #  these are just equal to the row numbers -- one occurrence per event
    e["occurrences"] =   1:nrow(e)

    # now go through and change each of the CF values to a vector (0,0,0,1,0,0,0,0)
      for (cf in EVENT_CF){
        #make a new column for each CF
        VCF = paste0("V_",cf)
        e[[VCF]]= vector(mode = "integer",length=nrow(e))

        for (r in 1:nrow(e)){
        e[[r,VCF]] = list(convert_CF_to_vector(e,cf,r))
        }
      }

    # I am not sure if we need to do these factors-- and they add a lot of processing time
  #     for (cf in compare_CF){
  #       #make a new column for each CF
  #       VCF = paste0("V_",cf)
  #       e[[VCF]]=vector(mode = "integer",length=nrow(e))
  #
  #       for (r in 1:nrow(e)){
  #         e[[r,VCF]] = list(convert_CF_to_vector(e,cf,r))
  #     }
  # }

    # just add the one column with the combined values
    e["ZM_1"] = as.factor(e[,newColName(EVENT_CF)])

    # Add the mapping to the global list of mappings.  No loneger storing the cluster solution here.
    map = list(name = paste(EventMapName), threads = e)

    GlobalEventMappings <<- append(list(map), GlobalEventMappings )

    print( get_event_mapping_names( GlobalEventMappings ) )
    save(GlobalEventMappings, file="eventMappings.RData")

    return(map)

}

# this one creates chunks based on the handoff index
OccToEvents2 <- function(o, EventMapName,EVENT_CF, compare_CF){

  # put this here for now
  timescale='mins'

  # Only run if eventMapName is filled in; return empty data frame otherwise
  if (EventMapName ==""){return(data.frame())}

  #### First get the break points between the events

        # whenever there is a zero handoff gap that means everything has changed
        breakpoints = which(o$handoffGap == 0)

        # Grab the breakpoints from the beginning of the threads as well
        threadbreaks = which(o$seqNum == 1)
        breakpoints = sort(union(threadbreaks,breakpoints))


  ### Use the break points to find the chunks -- just store the index back to the raw data
  nChunks = length(breakpoints)

  print("nChunks")
  print(nChunks)

  # make the dataframe for the results.  This is the main data structure for the visualizations and comparisons.
  e = make_event_df(EVENT_CF, compare_CF, nChunks)

  # add columns for each of the context factors used for comparison
  # for (cf in compare_CF){
  #   e[cf] = character(nChunks)
  # }

  #  need to create chunks WITHIN threads.  Need to respect thread boundaries
  # take union of the breakpoints, plus thread boundaries, plus 1st and last row

  # counters for assigning thread and sequence numbers
  thisThread=1  # just for counting in this loop
  lastThread=0
  seqNo=0  # resets for each new thread

  for (chunkNo in 1:nChunks){

    # Chunks start at the current breakpoint
    start_idx=breakpoints[chunkNo]

    # Chunks end at the next breakpoint, minus one
    # for the last chunk,the stop_idx is the last row
    if (chunkNo < nChunks){
      stop_idx = breakpoints[chunkNo+1] - 1
    } else if (chunkNo==nChunks){
      stop_idx = nrow(o)
    }

    # assign the occurrences
    e$occurrences[[chunkNo]] = list(start_idx:stop_idx)
   # e$eventStop[chunkNo] = as.integer(stop_idx)
   # e$eventStart[chunkNo] = as.integer(start_idx)

    # assign timestamp and duration
    e$tStamp[chunkNo] = o$tStamp[start_idx]
    e$eventDuration[chunkNo] = difftime(o$tStamp[stop_idx], o$tStamp[start_idx],units=timescale )

    # copy in the threadNum and assign sequence number
    e$threadNum[chunkNo] = o$POVthreadNum[start_idx]
    thisThread = o$POVthreadNum[start_idx]


    # fill in data for each of the context factors
    for (cf in compare_CF){
      e[chunkNo,cf] = o[start_idx,cf]
     }

    for (cf in EVENT_CF){
      VCF = paste0("V_",cf)
      e[[chunkNo, VCF]] = list(aggregate_VCF_for_event(o,e$occurrences[chunkNo],cf ))
    }

    # Advance or reset the seq counters
    if (thisThread == lastThread){
      seqNo = seqNo +1
    } else if (thisThread != lastThread){
      lastThread = thisThread
      seqNo = 1
    }
    e$seqNum[chunkNo] = seqNo

  }

  # convert them to factors
  for (cf in compare_CF){
    e[cf] = as.factor(e[,cf])
  }

  ### Use optimal string alignment to compare the chunks.  This is O(n^^2)
  clust = hclust( dist_matrix(e),  method="ward.D2" )

  ## Create a new column for each cluster solution -- would be faster with data.table
  for (cluster_level in 1:nChunks){

    clevelName = paste0("ZM_",cluster_level)
    e[clevelName] = cutree(clust, k=cluster_level)

      } # for cluster_level



  # for debugging, this is really handy
  #  save(o,e,file="O_and_E.rdata")

  # Add the mapping to the global list of mappings
  map = list(name = paste(EventMapName), threads = e)

  GlobalEventMappings <<- append(list(map), GlobalEventMappings )

  print( get_event_mapping_names( GlobalEventMappings ) )
  save(GlobalEventMappings, file="eventMappings.RData")

  #  need return the threads and also the cluster solution for display
  return(map)
}

# new function for new tab
# e is the event list
# EventMapName is an input selected from the list of available mappings
# cluster_method is either "Sequential similarity" or "Contextual Similarity"
clusterEvents <- function(e, EventMapName, cluster_method){

  ### Use optimal string alignment to compare the chunks.  This is O(n^^2)
  clust = hclust( dist_matrix(e),  method="ward.D2" )

  # number of chunks is the number of rows (the number of events to be clustered)
  nChunks = nrow(e)

  # make new data frame with columns for cluster level
  zm = setNames(data.frame(matrix(ncol = nChunks, nrow = nChunks)), paste0("ZM_", 1:nChunks))

  ## Create a new column for each cluster solution
  for (cluster_level in 1:nChunks){

    clevelName = paste0("ZM_",cluster_level)
    zm[clevelName] = cutree(clust, k=cluster_level)

  } # for cluster_level

  # now append this onto the global mapping


  return(clust)
}

# this function pulls computes their similarity of chunks based on sequence
dist_matrix <- function(e){

  nChunks = nrow(e)
  evector=vector(mode="list", length = nChunks)
  for (i in 1:nChunks){
    evector[i]=unique(as.integer(unlist(e$occurrences[[i]])))
  }
  return( stringdistmatrix( evector, method="osa") )
}

net_adj_matrix <- function(edges){

  return(as_adj(graph_from_edgelist(as.matrix(edges))))

}

# new data structure for events (BTP 3/28)
make_event_df <- function(event_CF,compare_CF,N){

  # Make a data frame with columns for each CF, and put one vector into each column
  e = data.frame(
    tStamp = numeric(N),  # this is the event start time
    eventDuration = numeric(N),
    occurrences = integer(N),
    threadNum = integer(N),
    seqNum = integer(N))

  # add columns for each of the context factors used to define events
  # first make the dataframes for each
#  cf1=setNames(data.frame(matrix(ncol = length(event_CF), nrow = N)), event_CF)
  cf1v=setNames(data.frame(matrix(ncol = length(event_CF), nrow = N)), paste0("V_",event_CF))
  cf2=setNames(data.frame(matrix(ncol = length(compare_CF), nrow = N)), compare_CF)
  # cf2v=setNames(data.frame(matrix(ncol = length(compare_CF), nrow = N)), paste0("V_", compare_CF))


  # Then combine them
  e = cbind(e, cf2,cf1v)

  # and add one more column for the event code/description
  e$ZM_1 = character(N)

  return(e)
}

# this will convert the context factor into a list (like this: 0 0 0 0 0 0 0 0 1 0 0)
# o is the dataframe of occurrences
# CF is the context factor (column)
# r is the row (occurrence number in the One-to-One mapping)
convert_CF_to_vector <- function(o,CF,r){

  return(as.integer((levels(o[[CF]]) ==o[[r,CF]])*1))

}


# take a list of events and aggregate the VCF (CF vector) for that CF
# There are two layers to this.
# 1) Within an single event, aggregate the VCF for the occurrences that make up that event
# 2) Within a cluster level, aggregate the events at that cluster level (e.g., ZM_n)
#
# o is a dataframe of occurrences with the V_ filled in.
# occlist is the list of occurrences of that event (e$occurrences)
# cf is the name of the contextual factor

aggregate_VCF_for_event <- function(o, occList, cf){

  # get the column name for the VCF
  VCF = paste0("V_",cf)

  # start with the first one so the dimension of the vector is correct
  aggCF = convert_CF_to_vector(o, cf, unlist(occList)[1])

   print( aggCF)

  # now add the rest, if there are any
  if (length(unlist(occList)) > 1){
    for (idx in seq(2,length(unlist(occList)),1)){
       print( aggCF)
      aggCF = aggCF + convert_CF_to_vector(o, cf, unlist(occList)[idx])
    }}
  return(aggCF)
}

# this version incorrectly assumes that the VCF is already computed.
# Might come in handy, but it's not correct...
# aggregate_VCF_for_event_oops <- function(o, occList, cf){
#
#   # get the column name for the VCF
#   VCF = paste0("V_",cf)
#
#   # start with the first one so the dimension of the vector is correct
#   aggCF = unlist(o[unlist(occList)[1],VCF])
#
#    # print( aggCF)
#
#   # now add the rest, if there are any
#    if (length(unlist(occList)) > 1){
#   for (idx in seq(2,length(unlist(occList)),1)){
#     # print( aggCF)
#     aggCF = aggCF+unlist(o[[unlist(occList)[idx],VCF]])
#   }}
#   return(aggCF)
# }


# Same basic idea, but works on a set of events within a cluster, rather than a set of occurrences within an event
# so you get get a subset of rows, convert to a matrix and add them up
aggregate_VCF_for_cluster <- function(e, cf, zoom_col, z){

  # get the column name for the VCF
  VCF = paste0("V_",cf)

  # get the subset of events for that cluster  -- just the VCF column
  s =  e[ which(e[[zoom_col]]==z), VCF]

   # print (s)
   # print(paste("length(s)",length(s)))
  if ( is.null(unlist(s) ))
    return(NULL)
  else
    return( colSums( matrix( unlist(s), nrow = length(s), byrow = TRUE) ))
}


