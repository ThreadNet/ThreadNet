##########################################################################################################
# THREADNET:  Batch processing for larger data sets

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################


# Take a large number of patient visits and create a data frame where each row contains
# a set of variables that describes a set of one or more visits.

# This is just a test
ACHR_batch_V1 <- function(inFileName){

  # library(ngram)
  # library(stringr)
  # library(stringdist)
  # library(tidyr)
  #
  # source("ThreadNet_Core.R")
  # source("ThreadNet_Misc.R")
  # source("ThreadNet_Graphics.R")
  # source("ThreadNet_Metrics.R")

# first read in the csv
rawOcc = read.csv(inFileName)


# HARD-CODED COLUMNS!!
TN = "threadID"
CFs =  c("role","workstn","action")



# clean up the ocurrences, add week and month columns
occ = cleanOcc(rawOcc,CFs)

# make threads -- NEED TO CHANGE THESE COLUMN NAMES
threadedOcc <- ThreadOccByPOV(occ,"threadID",c("role","workstn","action"))

# may want to make threads with and without different CFs to define events, as well

# pick subsets -- typically just one thread at a time, but could be more
# write a function for this
criteria <-"threadID"
bucket_list <- make_buckets(threadedOcc, criteria)

# print(bucket_list)

# loop through the buckets. Result will be data frame with one row per bucket
ACHR <- NULL
for (b in bucket_list){

  # select the threads that go in this bucket
    df = threadedOcc[threadedOcc[[TN]] ==b,]

    # length of the thread (number of rows)
    tl = nrow(df)

  # compute stuff on each context factor
    IV <- NULL
  for (cf in CFs){

    # get the compression
    IV_name = paste0(cf,"_compression")
    IV = c(IV,IV_name,  compression_index(df,cf))

    # get the entropy
    IV_name = paste0(cf,"_entropy")
    IV = c(IV,IV_name,  compute_entropy(table(df[[cf]])[table(df[[cf]])>0]))

    # get the network complexity



  }


  # compute each of the IVs and DVs and add them to the table
 ACHR <- rbind(ACHR, list(bucket=b, threadLen=tl, x="x", z="z", IV = IV))

}
# return the table
as.data.frame(ACHR)
}


# Each bucket is a list of thread numbers that can be used to subset the list of occurrences
make_buckets <- function(o, criteria){

  return((unique(o[[criteria]])))

}

