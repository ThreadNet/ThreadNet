##########################################################################################################
# THREADNET Misc functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################


##  Make an example data frame for display...
make_example_DF = function(){
  correct_occ = read.table(text="tStamp actor action object location
                            '2017-4-7 17:52:04' jimmy tosses ball playground
                            '2017-4-7 17:52:12' rover fetches ball playground
                            '2017-5-18 9:05:52' jimmy tosses ball forest
                            '2017-5-18 9:06:24' rover fetches stick forest
                            '2017-5-18 9:10:48' jimmy searches ball forest ", header=TRUE)
}



#' numThreads counts how many threads in the data set
#'
#' Threads must have unique thred numbers for this function to work
#'
#' @family ThreadNet_Misc
#' @param o data frame with occurrences or events
#' @param TN column with thread number
#'
#' @return number of threads
#' @export
numThreads = function(o,TN) {length(unique(o[[TN]]))}

# Time range for the data set (not really needed but nice)
timeRange= function(o){
  # get the min/max time in the whole set of occurrences
  start = min(as.POSIXlt.date(o$tStamp))
  finish = max(as.POSIXlt.date(o$tStamp))

  # take the difference
  difftime(finish,start)}

# Put it into a nice phrase
timeRangePhrase = function(tr){
  rangeunits = attr(tr,"units")
  paste(floor(as.numeric(tr)),rangeunits,"from start to finish.")}


# this function is used to split up the threads into n ~equal buckets
make_subsets <- function(d,n){
  return(split(d, ceiling(seq_along(d)/(length(d)/n))))
}

# This function takes a slider value and returns a valid column name for zooming
# if the argument is null, then use ZM_1
zoomColumn <- function(z){
  # print(paste("In zoomColumn z=",z))

  if (is.null(z))
  {r="ZM_1"}
  else
  {r=paste0("ZM_",z)}

  # print(paste("In zoomColumn r=",r))

  return(r)
}

######### Functions that return column names #######

# names of the columns for contextual factors
# grab all of the columns except the first, which has the time stamp
# tStamp in the first column
#' cfnames provides names of all the contextual factors (except the time stamp)
#' @family ThreadNet_Misc
#' @param o data frame with threads
#'
#' @return list of column names
#' @export
#'
cfnames <- function(o){
  colnames(o)[2:length(colnames(o))]}

## this is used to populate the UI for comparison of categories within a CF
#' get_CF_levels returns the levels of a contextual factor
#' @family ThreadNet_Misc
#' @param o data frame with threads
#' @param cf  a contextual factors (column)
#'
#' @return list of unique factor levels
#' @export
get_CF_levels <- function(o,cf){

  return(levels(o[,cf]))
}

##########################################################################################################
# this function adds a new column to the occurrenes table based on a combination of context factors CF)
#' Creates a new column that combines some set of other columns
#'
#' For example, actor+action
#'
#' @family ThreadNet_Misc
#' @param o data frame with threads
#' @param CF contextual factors to be combined.
#' @param newCol  name of the new combined conextual factor
#'
#' @return data frame with the new column
#' @export
combineContextFactors <- function(o,CF,newCol){


  # Use the old column if there is one
  if (!(newCol %in% names(o))) {

    # Need to get the CF parameters into the right format for tidyr::unite function
    cfn= sapply(CF, as.character)
    newCol = as.character(newCol)

    #  unite the columns, but keep the old ones
    o= unite_(o, newCol, cfn, sep="+", remove=FALSE)

  }

  # Coerce the new column into a factor
  o[newCol] = as.factor(o[,newCol])

  return(o)
}

# just keep this simple
newColName <- function(CF_list){
  return(paste0(CF_list,collapse="_")) }


# These were used on the occ-to-event tab to configure the slider
threshold_slider_min <- function(o){
  return(floor(min(o$timeGap)))
}

threshold_slider_max <- function(o){
  return(ceiling(max(o$timeGap)))
}

threshold_slider_selected <- function(o){
  return(min(o$timeGap))
}




#### count the handoffs, but reverse coded -- zero = all different
diff_handoffs <- function(o){

  # initialize the previous row
  previous_row <<- o[1,]

  return(apply(o,1, row_diff_handoff))

}
row_diff_handoff <- function(this_row){

  # just add up the differences.
  d <-sum(this_row==previous_row)

  # store the previous row
  previous_row <<-this_row

  # return the number ofdifferences
  return(d)
}


#### Time gaps -- just pass in the column of time stamps
diff_tStamp <- function(ts){

  # initialize the first row
  previous_row <<- ts[1]

  return(sapply(ts, row_diff_tStamp))

}
row_diff_tStamp <- function(this_row){

  # print(paste("this_row",this_row))
  # print(paste("previous_row",previous_row))


  # just add up the differences.
  d <-max(0,difftime(this_row, previous_row, units="secs"))

  # store the previous row
  previous_row <<-this_row

  # return the time difference
  return(d)
}




#' threadSizeTable provides a distribution of the length of threads
#'
#' This function should work on either ocurrences or events.
#' it returns length and duration of each thread.It requires tStamp field to compute duration.
#'
#' @family ThreadNet_Misc
#'
#' @param o data frame with threads
#' @param TN column comtaining the threadNumber
#'
#' @return data frame with table of thread lengths
#' @export
threadSizeTable <- function(o,TN){


  # get the number of threads
  nThreads = nrow(unique(o[TN]))

  id=integer(nThreads)
  num=integer(nThreads)
  dur=numeric(nThreads)
  sizes = data.frame(id,
                     num,
                     dur)

  for (i in 1:nThreads){
    sizes$id = i
    sizes$num[i] = sum(o[TN]==i)
  }

  s = as.data.frame(table(sizes$num))

  return(s)
}




#########################################################
#' convert_TN_to_TramineR
#'
#' converts the csv format used in ThreadNet to the format used by TraMiner.  Should provide a way to save this, as well.
#'
#' @family ThreadNet_Misc
#' @param df  threads (occurrences or events)
#' @param CF Contextual factor that will be used to define the state sequences in TraMineR
#'
#' @return Dataframe in TraMineR format (state sequeces in horizontal rows)
#' @export
#'
convert_TN_to_TramineR <- function(df, CF){
  # dataframe must be sorted by time or sequence within each threadNumber
  # TN is the threadnumber
  # CF is some attribute we will use in TramineR

  # first find the threads
  threads = unique(df$threadNum)
  nThreads = length(threads)

  # Initialize list of empty lists
  s = rep( list(list()), nThreads )

  for ( th in 1:nThreads){

    #subset of df that contains the sequence
    s[[th]] = as.character(df[df$threadNum==threads[th],CF])
  }

  # add NA to make all the lists the same length
  s = lapply(s, `length<-`, max(lengths(s)))

  # convert to data frame
  df <- data.frame(matrix(unlist(s), nrow=nThreads, byrow=T))


  return(df)

}

# these functions support the moving window
#' get_threadList returns a list of all thread numbers
#'
#' @family ThreadNet_Misc
#'
#' @param e  data frame with threaded events
#' @param TN Column with threadNumber
#' @param SN Column with sequence numbers
#'
#' @return list of thread numbers
#' @export
get_threadList <- function(e,TN,SN){

  # for the current data structure for events, you just pick all of the threads where seqNum == 1
  return(e[e[[SN]]==1,TN])
}

#' get_moving_window returns a set of threads for a moving window
#'
#' @family ThreadNet_Misc
#'
#' @param e data frame with threads (needs to have threadNum and seqNum)
#' @param s size of window
#' @param l location of window
#'
#' @return data from with just the threads in the window
#' @export
#'
get_moving_window <- function(e, s, l ){

  # get the list of threads
  w=get_threadList(e,"threadNum","seqNum")

  # get get the appropriate subset of threads for the window
  w=w[l:(l+s-1)]

  # and now subset the rows for those threads

  return(e[e$threadNum %in% w,])

}

# e is the data
# w = window size
# s = step (how far to move the window in each step)
# n is the ngram size
window_correlation  <- function(e,w,s=1,n=2){

  # make data frame
  vt=data.frame( ngrams=character(), freq=integer(), wid=integer() )

  # use the finest  granularity
  zcf = zoom_upper_limit(e)

  # now many threads?
  nThreads = numThreads(e,'threadNum')

  wcount=0
  # scan through the data
  for (wloc in seq( 1, nThreads, s)){
    wcount= wcount +1
    # print(paste('wloc =',wloc))

    # get text vector for the whole data set - just keep the first two colomns
    ngdf = count_ngrams(get_moving_window(e, w, wloc), 'threadNum', zcf, n)[1:2]
    # print(paste('nrow ngdf =',nrow(ngdf)))

    # add a the row number
    ngdf$wid = wcount

    # append the columns to the end
    vt=rbind(vt,ngdf)
  }

  # convert to factor so that we can compute distances using the factor levels
  vt$ngrams = factor(vt$ngrams)

  nWindows = length(unique(vt$wid))

  # get the set of unique ngrams for the whole data set
  vt_unique = data.frame(ngrams=unique(vt$ngrams))

  # put the results here
  windowFreqMatrix = matrix(0,nrow=nWindows, ncol=nrow(vt_unique))

  for (i in 1:nWindows){

    # get the merged list
    vtmerge = merge(x=vt_unique, y=vt[vt$wid==i,], by='ngrams', all.x = TRUE)

  # use the wid.y to get the whole vector, but replace the NA with zeros
   b=vtmerge[vtmerge$wid==i,'freq']
   b[is.na(b)] <- 0

   windowFreqMatrix[i,]=b
  }


 # old way: correlate one row with the next and stick it in a dataframe
  df =data.frame(window=1:(nWindows-1),
                 thread = seq( 1, nThreads-s, s),
                 correlation= unlist(lapply(1:(nWindows-1),
                                              function(i){abs( cor(windowFreqMatrix[i,],windowFreqMatrix[i+1,])) })))

  # add the last row explicitly
  df = rbind( df, data.frame(window=nWindows, thread=nThreads, correlation=0))

  # return( df )

  # get the ngram data and labels
  b_df=as.data.frame(windowFreqMatrix)
  colnames(b_df)=vt_unique$ngrams

  # stick the ngram frequencies on the end for good measure
 return(cbind(df,b_df))

}

# e is the data
# w = window size
# s = step (how far to move the window in each step)
# n is the ngram size
# similar as above, except one window on each side of a focal thread.
dual_window_correlation  <- function(e,w,s=1,n=2){

  # make data frame
  vt=data.frame( ngrams=character(), freq=integer(), id=integer() )

  # use the finest  granularity
  zcf = zoom_upper_limit(e)

  # now many threads?
  nThreads = numThreads(e,'threadNum')


  # scan through the threads - treat each thread as a window of one
  # can probably do with the split and apply much faster
  for (t in 1:nThreads){

    # print(paste('wloc =',wloc))

    # get text vector for the whole data set - just keep the first two colomns
    ngdf = count_ngrams(get_moving_window(e, 1, t), 'threadNum', zcf, n)[1:2]
    # print(paste('nrow ngdf =',nrow(ngdf)))

    # add a the row number
    ngdf$id = t

    # append the columns to the end
    vt=rbind(vt,ngdf)
  }

  # convert to factor
  vt$ngrams = factor(vt$ngrams)

  # compute number of windows.
  nWindows = floor(nThreads/w)

  # get the set of unique ngrams for the whole data set
  vt_unique = data.frame(ngrams=unique(vt$ngrams))

  # put the results here
  ngramFreqMatrix = matrix(0,nrow=nThreads, ncol=nrow(vt_unique))

  for (i in 1:nThreads){

    # get the merged list
    vtmerge = merge(x=vt_unique, y=vt[vt$id==i,], by='ngrams', all.x = TRUE)

    # use the wid.y to get the whole vector, but replace the NA with zeros
    b=vtmerge[vtmerge$id==i,'freq']
    b[is.na(b)] <- 0

    ngramFreqMatrix[i,]=b
  }

  # return(ngramFreqMatrix)

  # old way: correlate one row with the next and stick it in a dataframe
  df =data.frame( thread = seq(w,nThreads-(w+1),s),
                  correlation= unlist(lapply(seq(w,nThreads-(w+1),s),
                                            function(i){abs( cor(colSums( ngramFreqMatrix[(i-w):i, ] ),
                                                                colSums( ngramFreqMatrix[(i+1):(i+w+1), ] )))  })))

  # # add the last row explicitly
  # df = rbind( df, data.frame( thread=nThreads, correlation=0))

   return( df )

  # # get the ngram data and labels
  # b_df=as.data.frame(ngramFreqMatrix)
  # colnames(b_df)=vt_unique$ngrams
  #
  # # stick the ngram frequencies on the end for good measure
  # return(cbind(df,b_df))

}

# Make a nice dataframe to display
# Issue is that DT::renderdatatable cannot display lists correctly.
make_nice_event_DT <- function(e){

  # Add new column for the occurrences as a character string for display
  # tibble::add_column(e, paste(e$occurrences,sep=","), .after=1)

  e$OccurrenceList = paste(e$occurrences,sep=",")

  # move occurences directly after tstamp
  col_occ = which(colnames(e)=="OccurrenceList")
  e = e[, c(1, col_occ, (2:ncol(e))[-col_occ])]

  # Now remove the columns that have lists
  e$occurrences = NULL
  e$OccurrenceList.1 = NULL
  e[grep("V_",colnames(e))]=NULL

  return(e)
}

# find the biggest column with ZM_, and then get the number that goes with that.
# It will not be the same as the column number.
zoom_upper_limit <- function(e){
  upper_limit = as.integer(str_replace(colnames(e[max(grep("ZM_",colnames(e)))]),"ZM_",""))
  return(upper_limit)
}

# sliderInput("ThreadMapZoomID",
#             "Zoom in and out by event similarity:",
#             1,100,5, step = 1, ticks=FALSE)



######################################################
# Just putting this code here to play with for now.
# this function finds the common events in two subsets of thread data
common_events <- function(ss1, ss2, TN, CF, n){

  # get the list of ngrams for each subset of threads
  e1 = count_ngrams(ss1, TN, CF, n)[1]
  e2 = count_ngrams(ss2, TN, CF, n)[1]

  # return the intersection
  return(intersect(as.matrix(e1), as.matrix(e2)))

}

rr_grams <- function(o,TN, CF, N, R) {
  # N - max length of ngram
  # R = threshold for repetition




}

# Ideas for regex work
# https://stackoverflow.com/questions/35704369/identify-repetitive-pattern-in-numeric-vector-in-r-with-fuzzy-search

# sapply(1:(length(x)/2), function(m) sum(rep(x[1:m], length = length(x)) != x))

# x <- rep(c(1, 4, 2), 10)
# for(k in seq_len(length(x)/2)) {
#   pat <- x[1:k]
#   if (identical(rep(pat, length = length(x)), x)) {
#     print(pat)
#     break
#   }
# }
## [1] 1 4 2
