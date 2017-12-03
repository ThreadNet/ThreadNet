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
ACHR_batch_V1 <- function(inFileName){


# first read in the csv
rawOcc = fread(inFileName)


# HARD-CODED COLUMNS!! Should probably pass in as a parameters
TN = "threadID"
CFs =  c("role","workstn","action")
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

