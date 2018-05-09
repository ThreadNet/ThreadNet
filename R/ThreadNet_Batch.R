##########################################################################################################
# THREADNET:  Batch processing for larger data sets

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################


# Take a large number of patient visits and create a data frame where each row contains
# a set of variables that describes a set of one or more visits.

# This is just a test
#' Batch processing for larger numbers of threads
#'
#' ACHR stands for Antecedents of Complexity in Healthcare Routines.  This is function is set up to compute process parameters on thousands of patient visits.
#'
#' @param inFileName name of file (CSV format) containing the raw thread data.
#'
#' @return data frame ready for further analysis
#'
#' @export
#ACHR_batch_V1 <- function(inFileName){
  ACHR_batch_V1 <- function(rawOcc){


# first read in the csv
#rawOcc = fread(inFileName)


# HARD-CODED COLUMNS!! Should probably pass in as a parameters
TN = "Visit_Number"
CFs =  c('Workstation_ID', 'Action', 'Role')
DV= newColName(CFs)


# clean up the ocurrences, add week and month columns
occ = cleanOcc(rawOcc,CFs)

# make threads - this will also make a new column that combines the CFs
threadedOcc <- ThreadOccByPOV(occ,TN,CFs)

# may want to make threads with and without different CFs to define events, as well

# pick subsets -- typically just one thread at a time, but could be more
# write a function for this
criteria <-"threadID"
bucket_list <- make_buckets(threadedOcc, criteria)

# get the size (number of buckets)
N = length(bucket_list)

# pre-allocate the data.table.  Tables are supposed to be faster.
ACHR = data.table(bucket=integer(N),
                  NEvents = integer(N),
                  NDiagnoses = integer(N),
                  NProcedures = integer(N),
                  visitStartTime = numeric(N),  # might need special data type for time
                  VisitDuration=numeric(N),  # might need special data type for time
                  NetComplexity=double(N),
                  CompressRatio = double(N),
                  Clinic = character(N),
                  PrimaryDiagnosis = character(N),
                  PayerType = character(N),
                  Provider = character(N)  # might not be available
                  )

# Now add columns for the IVs.  There will be three for each IV

# Add the IV columns
for (cf in CFs){

  ACHR[, paste0(cf,"_count"):= double(N)]
  ACHR[, paste0(cf,"_compression"):= double(N)]
  ACHR[, paste0(cf,"_entropy"):= double(N)]

}

# loop through the buckets. Result will be data frame with one row per bucket
for (i in 1:N){

  b = i #  as.integer(bucket_list[i])

  # select the threads that go in this bucket
    df = threadedOcc[threadedOcc[[TN]] ==bucket_list[i],]

    # bucket number
    ACHR[b,bucket := b]

    # length of the thread (number of rows)
    ACHR[b,NEvents := nrow(df)]

    # only do the computations if there are more than two occurrences
    if (nrow(df) > 2) {

    # compressibility of DV
    ACHR[b,CompressRatio := compression_index(df,DV)]

    # NetComplexity of DV
    # First get the network
    n = threads_to_network(df,TN, DV)
    ACHR[b,NetComplexity := estimate_network_complexity( n )]

  # compute stuff on each context factor
  for (cf in CFs){

    # Count the unique elements in each cf
    ACHR[b, paste0(cf,"_count") :=  length(unique(df[[cf]])) ]

    # get the compression
    ACHR[b, paste0(cf,"_compression") := compression_index(df,cf) ]

    # get the entropy
    ACHR[b, paste0(cf,"_entropy") := compute_entropy(table(df[[cf]])[table(df[[cf]])>0]) ]

  }
} # kf nrows > 2

} # loop thru buckets

# return the table
return(ACHR)
}


# Each bucket is a list of thread numbers that can be used to subset the list of occurrences
make_buckets <- function(o, criteria){

  return( levels(o[[criteria]]) )

}


# this code is copied/adapted from ThreadNet_Misc.R
# e is the data
# w = window size
# s = step (how far to move the window in each step)
# n is the ngram size
# Use one day per bucket
# zcf is just the role for now
bucket_correlation  <- function(e,w,s=1,n=2,zcf='Role'){

  # make data frame
  vt=data.frame( ngrams=character(), freq=integer(), id=integer() )

  # use the finest  granularity
 # zcf = zoom_upper_limit(e)

  # now many threads?
  nThreads = numThreads(e,'threadNum')


# treat each day as a bucket
  win_num = 0
  for (t in unique(e$vday)){

    win_num = win_num +1

    # get text vector for the whole data set - just keep the first two colomns
    ngdf = count_ngrams(e[e$vday==t,], 'threadNum', zcf, n)[1:2]
     print(paste('nrow ngdf =',nrow(ngdf)))

    # add an ID
    ngdf$id = win_num

    # filter and convert to 0/1
    ngdf$freq = (ngdf$freq>0)  * 1

    # append the columns to the end
    vt=rbind(vt,ngdf)
  }

  # convert to factor
  vt$ngrams = factor(vt$ngrams)

  # compute number of windows.
  nWindows = length(unique(e$vday))

  print(paste('nWindows=',nWindows))

  # get the set of unique ngrams for the whole data set
  vt_unique = data.frame(ngrams=unique(vt$ngrams))

  # put the results here
  ngramFreqMatrix = matrix(0,nrow=nWindows, ncol=nrow(vt_unique))

  for (i in 1:nWindows){

    # get the merged list
    vtmerge = merge(x=vt_unique, y=vt[vt$id==i,], by='ngrams', all.x = TRUE)

    # use the wid.y to get the whole vector, but replace the NA with zeros
    b=vtmerge[vtmerge$id==i,'freq']
    b[is.na(b)] <- 0

    ngramFreqMatrix[i,]=b
  }

   return(ngramFreqMatrix)

  # old way: correlate one row with the next and stick it in a dataframe
  # df =data.frame( id = 1:nWindows,
  #                 vday = unique(e$vday),
  #                 correlation= unlist(lapply(1:nWindows,
  #                                            function(i){cor( ngramFreqMatrix[1, ] ,
  #                                                             ngramFreqMatrix[i, ] )  })))



  #use hamming distance = haw many edges are different?
  df =data.frame( id = 1:nWindows,
                  vday = unique(e$vday),
                  correlation= unlist(lapply(1:nWindows,
                                             function(i){sum( ngramFreqMatrix[1, ] !=
                                                              ngramFreqMatrix[i, ] ) })))

  plot(smooth(df$correlation),xlab='Days',ylab='Distance')
   lines(smooth(df$correlation),xlab='Days',ylab='Distance')



  return( df )

  # # get the ngram data and labels
  # b_df=as.data.frame(ngramFreqMatrix)
  # colnames(b_df)=vt_unique$ngrams
  #
  # # stick the ngram frequencies on the end for good measure
  # return(cbind(df,b_df))

}




# borrowed and adapted from threadnet_core.r
ThreadOccByPOV_batch <- function(o,THREAD_CF,EVENT_CF){

  # make sure there is a value
  if (length(THREAD_CF) == 0 | length(EVENT_CF)==0){return(data.frame())}

  # Sort by POV and timestamp. The idea is to get the stream of activities from
  # a particular point of view (e.g., actor, location, etc.)
  # add the new column that combines CFs, if necessary

  # get a new column name based on the thread_CF -- use this to define threads
  nPOV = newColName(THREAD_CF)
  occ = combineContextFactors(o,THREAD_CF, nPOV )

   print("nPOV")
   print(nPOV)
  #
   print("THREAD_CF")
   print(THREAD_CF)

  # The event context factors define the new category of events within those threads
  occ = combineContextFactors(occ,EVENT_CF,newColName(EVENT_CF))
  print(head(occ))
  # occ = occ[order(occ[nPOV],occ$tStamp),]

  # occ = occ[order(occ$tStamp),]


  # add two columns to the data frame
  occ$threadNum = integer(nrow(occ))
  occ$seqNum =   integer(nrow(occ))

  # add new column called label - just copy the new combined event_CF column
  #occ$label = occ[[newColName(EVENT_CF)]]

  # occurrences have zero duration
 # occ$eventDuration = 0

  # Also add columns for the time gapsthat appear from this POV
  # occ$timeGap  =  diff_tStamp(occ$tStamp)


  # create new column for relative time stamp. Initialize to absolute tStamp and adjust below
  # occ$relativeTime = lubridate::ymd_hms(occ$tStamp)

  # then get the unique values in that POV
  # occ[nPOV] = as.factor(occ[,nPOV])
  pov_list = levels(occ[[nPOV]])

  print('length(pov_list)')
  print(length(pov_list))
  # now loop through the pov_list and assign values to the new columns
  start_row=1
  thrd=1
  for (p in pov_list){

    if ((as.integer(p) %% 500) == 0) {print(paste('p=',p))}

    # get the length of the thread
    tlen = sum(occ[[nPOV]]==p)

    # print(paste('start_row=',start_row))
    # print(paste('thrd =', thrd ))
    # print(paste('p =', p ))
    # print(paste('tlen =', tlen ))

    # guard against error
    if (tlen>0){

      #compute the index of the end row
      end_row = start_row+tlen-1
      # print(paste('start_row =', start_row ))
      # print(paste('end_row =',end_row  ))

      # they all get the same thread number and incrementing seqNum
      occ[start_row:end_row, "threadNum"] <- as.matrix(rep(as.integer(thrd),tlen))
      occ[start_row:end_row, "seqNum"] <- as.matrix(c(1:tlen))


      # find the earliest time value for this thread
     # start_time = min(lubridate::ymd_hms(occ$tStamp[start_row:end_row]))
      # print(start_time)


      # increment the counters for the next thread
      start_row = end_row + 1
      thrd=thrd+1
    } # tlen>0
  }

  # split occ data frame by threadNum to find earliest time value for that thread
  # then substract that from initiated relativeTime from above
 # occ_split = lapply(split(occ, occ$threadNum), function(x) {x$relativeTime = x$relativeTime - min(lubridate::ymd_hms(x$tStamp)); x})
  # # row bind data frame back together
 # occ= data.frame(do.call(rbind, occ_split))

  #  these are just equal to the row numbers -- one occurrence per event
 # occ["occurrences"] =   1:nrow(occ)

  # now go through and change each of the CF values to a vector (0,0,0,1,0,0,0,0)
  # for (cf in EVENT_CF){
  #   #make a new column for each CF
  #   VCF = paste0("V_",cf)
  #   occ[[VCF]]= vector(mode = "integer",length=nrow(occ))
  #
  #   for (r in 1:nrow(occ)){
  #     occ[[r,VCF]] = list(convert_CF_to_vector(occ,cf,r))
  #   }
  # }
  #
  # just add the one column with the combined values
  # occ["ZM_1"] = as.integer(occ[,newColName(EVENT_CF)])


  # this will store the event map in the GlobalEventMappings and return events with network cluster added for zooming...
  # e=clusterEvents(occ, 'OneToOne', 'Network Proximity', EVENT_CF,'threads')

  # for debugging, this is really handy
  #   save(occ,e,file="O_and_E_1.rdata")

  print('done converting occurrences...')

  return( occ )

}

map_networks_onto_integers <- function(edge_matrix ){

  # each row in the edge_matrix is a vector of edges for one network
  # convert them to a list of strings
  f=unlist(lapply(1:nrow(edge_matrix), function(i){concatenate(edge_matrix[i,])}))

  # initialize the result
  nf = integer(nrow(edge_matrix))

  nf[1]=1
  for (i in 2:nrow(edge_matrix)){

    ind = grep(f[i], f[1:(i-1)])
    print(ind)
    if (identical(ind, integer(0))){ind=0}

    if (ind >0)
      {nf[i] = ind[1]}
    else
      {nf[i] = i}
  }

return(nf)

}
