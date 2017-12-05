##########################################################################################################
# THREADNET Misc functions

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# Read in, check, and clean up the data
# need to see "tStamp" in the first column
#' read_occurrences
#'
#' @family ThreadNet_Misc
#'
#' @param inFile
#'
#' @return dataframe with occurrences
#' @export
#'
#' @examples
read_occurrences <- function(inFile){

  # if it's null return null, otherwise do the whole thing...
  if (is.null(inFile))
    return(NULL)

  # read in the table of occurrences
  o=read.csv(inFile$datapath)

  # check the file format.  Put in humorous example if the format is bad
   if (check_file_format(o)=="badformat")
   {o=make_example_DF() }
  else if (check_file_format(o)=="sequence")
  {o=add_relative_timestamps(o,"sequence", 1) }

  # clean up the data -- remove blanks, etc.
  o = cleanOcc(o,cfnames(o))

  return(o)}

# This could be improved but is an important logical checkpoint
# just checks that a required field is in the first column
check_file_format = function(o){

  if ((colnames(o)[1] == "tStamp"))
  {return("tStamp")}

  else if ((colnames(o)[1] == "sequence"))
  {return("sequence")}

  else
  {return("badformat")}
}

#' Add relative timestamps
#'
#' This function uses the sequence numbers to add a column with time stamp to the data, so that it can be used throughout the rest
#' of the app, which expects to see a time stamp.  Start time for all threads is the same: "2017-01-01 00:00:00"  Happy New Year!
#'
#' @param o   data frame of occurrences
#' @param SN column containing the sequence numbers
#' @param tstep time step for events within each thread.  Default is one minute.
#'
#' @return  data frame of occurrences
#' @export
#'
#' @examples
add_relative_timestamps <- function(o, SN, tstep=1){

  startTime <- as.POSIXlt("2017-01-01 00:00:00")

  # add the column at the beginning
  o <- cbind(startTime + 60*as.numeric(as.character(o[[SN]])), o)

  # set the column name
  colnames(o)[1] <- "tStamp"

  return(o)

}

##  Make an example data frame for display...
make_example_DF = function(){
  correct_occ = read.table(text="tStamp actor action object location
                            '2017-4-7 17:52:04' jimmy tosses ball playground
                            '2017-4-7 17:52:12' rover fetches ball playground
                            '2017-5-18 9:05:52' jimmy tosses ball forest
                            '2017-5-18 9:06:24' rover fetches stick forest
                            '2017-5-18 9:10:48' jimmy searches ball forest ", header=TRUE)
}

# this function will clean up the raw occurrence data
#' Title
#' @family ThreadNet_Misc
#' @param o
#' @param cfnames
#'
#' @return
#' @export
#'
#' @examples
cleanOcc = function(o, cfnames){

  ## clean up the spaces here and make it back into a factor
  for (cf in cfnames){
    o[,cf] = sapply(o[,cf],fixBlanks)
    o[cf] = factor( o[,cf] )
  }

  ## Add the category ">other<" for all of the factors to facilitate recoding later
 # This may not be needed anymore... commented out Dec 3 2017
 # o <- as.data.frame(lapply(o, addOther))

  # add weekday and month
  o$weekday = as.factor(weekdays(as.Date(o$tStamp)))
  o$month = as.factor(months(as.Date(o$tStamp)))

  return(o)
}

## Use this function to remove blanks from the CF data
fixBlanks = function(s){

  # take out blanks
  s=str_replace_all(s," ","_")

  if (s==""){
    s="blank"
  }
  return(s)
}

# NO LONGER NEEDED  12/2017 add the >other< categeory
# addOther <- function(x){
#   if(is.factor(x))
#     return(factor(x, levels=c(levels(x), ">other<")))
#   return(x) }

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

# This function limits the number of rows that get used
# SubsetOfTable <- function(df,pct) {df[1:(floor((pct/100) * nrow(df))),] }
SubsetOfTable <- function(df,r) {df[r[1]:r[2],] }


# this function is used to split up the threads into n ~equal buckets
make_subsets <- function(d,n){
  return(split(d, ceiling(seq_along(d)/(length(d)/n))))
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
#'
#' @examples
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


# These are used on the occ-to-event tab to configure the slider
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
#' @param TN Column with threadNumber
#' @param CF Contextual factor that will be used to define the state sequences in TraMineR
#'
#' @return Dataframe in TraMineR format (state sequeces in horizontal rows)
#' @export
#'
#' @examples
convert_TN_to_TramineR <- function(df, TN, CF){
  # dataframe must be sorted by time or sequence within each threadNumber
  # TN is the threadnumber
  # CF is some attribute we will use in TramineR

  # first find the threads
  threads = unique(df[,TN])
  nThreads = length(threads)

  # Initialize list of empty lists
  s = rep( list(list()), nThreads )

  for ( th in 1:nThreads){

    #subset of df that contains the sequence
    s[[th]] = as.character(df[df[[TN]]==threads[th],CF])
  }

  # add NA to make all the lists the same length
  s = lapply(s, `length<-`, max(lengths(s)))

  # convert to data frame
  df <- data.frame(matrix(unlist(s), nrow=nThreads, byrow=T))

  # add a column for the threadnumber
  # df[TN] = threads

  return(df)

}

# these functions suppose the moving window
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

  # and now subset the rows for those threds

  return(e[e$threadNum %in% w,])

}
