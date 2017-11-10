##########################################################################################################
# THREADNET:  Core functions

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# These are the basic functions that convert threads to networks, etc.

# convert threads to network
threads_to_network <- function(et,TN,CF){

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
  return(list(nodeDF = nodes, edgeDF = edges))
}

###############################################################
# Counting ngrams is essential to several ThreadNet functions
count_ngrams <- function(o,TN,CF,n){
  # o = datafrome of threaded occurrences or events
  # TN = threadNum
  # CF = contextuL factors to focus on
  # n = length of n-gram


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
# Make threads with new POV
# THREAD_CF is a list of 1 or more context factors that define the threads (stay constant during each thread)
# EVENT_CF is a list of 1 or more context factors that define events (change during threads)
# o is the dataframe of cleaned ocurrences
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

      # increment the counters for the next thread
      start_row = end_row + 1
      thrd=thrd+1
    } # tlen>0
  }
  return(occ)
}


##############################################################################################################
OccToEvents <- function(o, m, uniform_chunk_size, tThreshold, chunk_CF, EventMapName,EVENT_CF, compare_CF, timescale){

  # Inputs: o = table of occurrences
  #         m = method parameter = c('Variable chunks','Uniform chunks')
  #         uniform_chunk_size = used to identify breakpoints -- from input slider
  #         tThreshold = used to identify breakpoints -- from input slider
  #         EventMapName = used to store this mapping in an environment
  #         CF_compare = context factors used for comparison -- need to be copied over here when the thread is created.
  #
  # Outputs: e = event data frame, with occurrences aggregated into events
  #           Even with the 1_to_1 method, the data structure is different
  # tStamp
  # tDuration
  # chunk (points back to o)
  # cluster membership (based on distance between chunks)
  # threadNum
  # SeqNum

  # Only run if eventMapName is filled in; return empty data frame otherwise
  if (EventMapName ==""){return(data.frame())}

  #### First get the break points between the occurrances.
  # Method one:  "Variable chunks"
  # Method two:  Time gap
  #  Consider MOVING the code the assigns the gaps INTO THE PREVIOUS SECTION, as a result of re-threading

  if (m=="Variable chunks"){
    breakpoints = which(o$handoffGap == 0)
  } else if (m=="Time gap") {
    breakpoints = which(o$timeGap > tThreshold)
  } else if (m=="Uniform chunks") {
    breakpoints = seq(1,nrow(o),uniform_chunk_size)  # NO -- should be within each thread.
  }

  # Grab the breakpoints from the beginning of the threads as well
  threadbreaks = which(o$seqNum == 1)
  breakpoints = sort(union(threadbreaks,breakpoints))

  ### Use the break points to find the chunks -- just store the index back to the raw data
  nChunks = length(breakpoints)

  # make the dataframe for the results.  This is the main data structure for the visualizations and comparisons.
  e = data.frame(
    tStamp = numeric(nChunks),
    eventDuration = numeric(nChunks),
    eventStart = integer(nChunks),
    eventStop = integer(nChunks),
    threadNum = integer(nChunks),
    seqNum = integer(nChunks))

  # add columns for each of the context factors used for comparison
  for (cf in compare_CF){
    e[cf] = character(nChunks)
  }

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

    # assign the start/stop
    e$eventStart[chunkNo] = as.integer(start_idx)
    e$eventStop[chunkNo] = as.integer(stop_idx)

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


  #  save(o,e,file="O_and_E.rdata")


  ### Use optimal string alignment to compare the chunks.  This is n^^2...
  # only need to compare unique chunks.

  # get the unique chunks -- use the combined column, not all the EVENT_CF columns

  dm = matrix(0,nrow=nChunks,ncol=nChunks)
  for (i in 1:nChunks){
    for (j in 1:i){

      chunk1 = unique(as.integer(unlist(o[e$eventStart[i]:e$eventStop[i],newColName(EVENT_CF)])))
      chunk2 = unique(as.integer(unlist(o[e$eventStart[j]:e$eventStop[j],newColName(EVENT_CF)])))

      # print(newColName(EVENT_CF))
      # print("chunk1")
      # print(chunk1)
      # print("chunk2")
      # print(chunk2)

      dm[i,j] <- seq_dist(chunk1, chunk2, method='osa')

      # print("dm[i,j]")
      # print(dm[i,j])
    }
  }

  ### Cluster the chunks
  clust <- hclust(as.dist(dm), method="ward.D2")

  ## Create a new column for each cluster solution -- would be faster with data.table
  for (cluster_level in 1:nChunks){

    clevelName = paste0("E_",cluster_level)
    e[clevelName] = cutree(clust, k=cluster_level)

  }
#  return(e)
  return(list(threads = e, cluster = clust))
}
