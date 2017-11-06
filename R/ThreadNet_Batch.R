##########################################################################################################
# THREADNET:  Batch processing for larger data sets

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################


# Take a large number of patient visits and create a data frame where each row contains
# a set of variables that describes a set of one or more visits.

# This is just a test
De <- function(inFileName){

  library(ngram)
  library(stringr)
  library(stringdist)
  library(tidyr)

  source("ThreadNet_Core.R")
  source("ThreadNet_Misc.R")
  source("ThreadNet_Graphics.R")
  source("ThreadNet_Metrics.R")

# first read in the csv
rawOcc = read.csv(inFileName)

# clean up the ocurrences, add week and month columns
# HARD-CODED COLUMNS!!
occ = cleanOcc(rawOcc, c("role","workstn","action"))

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
  #  df = threadedOcc[,threadedOcc$threadNum ==b]



  # compute each of the IVs and DVs and add them to the table
 ACHR <- rbind(ACHR, list(bucket=b, x="x", z="z"))

}
# return the table
ACHR
}


# Each bucket is a list of thread numbers that can be used to subset the list of occurrences
make_buckets <- function(o, criteria){

  return((unique(o[[criteria]])))

}

