##########################################################################################################
# THREADNET:  Metrics

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# Functions for metrics:  entropy, complexity, routine-ness, etc.


# takes the output from the function that maps threads to networks
#' Estimates the number of paths in a directed graph
#'
#' This function takes a network descripts (nodes and edges, as generaged by the functino threads_to_network, and estimates the number of paths.
#' as described in Haerem, Pentland and Miller (2015). The estimate correlates with the McCabe's (1975) cyclometric complexity.
#'
#' @name estimate_network_complexity
#' @param net Object with dataframe for nodes and edges
#' @return number
#' @export
estimate_network_complexity <- function(net){ return(estimate_task_complexity_index( nrow(net$nodeDF), nrow(net$edgeDF)) ) }

# this version takes vertices and edges
#' Estimates the number of paths in a directed graph
#'
#' Same as estimate_network_complexity, but takes different parameters
#'
#' @name estimate_task_complexity_index
#' @param v number of vertices (or nodes)
#' @param e number of edges
#' @return number
#' @export
estimate_task_complexity_index <- function(v,e){

  #INPUT ARGS:
  # in MatLab version, arguments were v (vertices) and e (edges)
  # v = number of vertices
  # tested for range of 10 < v < 100
  # e = number of edges
  print("edges")
  print(e)
  print("vertices")
  print(v)
  #
  # OUTPUT ARG:
  # cidx correlates with Log10(simple paths) with r>= 0.8

  # from ORM paper analysis, constant is 0.12.
  # For boundary condition of 2 nodes and 1 edge, complexity index=0, constant = 0.08
  return(  0.08 + 0.08*e - 0.08*v )
}


#################################################################
#' Computes a metric of routineness based on frequency of ngrams
#'
#' Computes the fraction of observed behavior that conforms to an observed pattern.
#' Current version uses ngrams, but it would be good to use spmf pattern mining to avoid including duplicate patterns  (e.g., a-b-c and b-c-d)
#'
#' @name routineness_metric
#' @param o  data frame with occurresnces or events
#' @param TN  name of column with threadNumbers
#' @param CF name of column with contextual factor
#' @param n size of ngram
#' @param m how many of the most frequent ngrams to include. When m > 1, there is a risk of duplication.
#' @return number, index of routineness.
#' @export
routineness_metric <- function(o,TN,CF,n,m){

  # get the ngrams
  ng=count_ngrams(o,TN,CF,n)

# print(ng[1:m,])

  # return the ratio of occurrences in the top m most frequent ngrams to total occurrences
    return( sum(ng$freq[1:m])*n/nrow(o) )
}


#############################################################################
#' Computes the compressibility of the data in one column of a data frame
#'
#' Compressibility is an index of complexity -- more compressible means less complex.  This function computes the ratio of compressed data
#' to the original data.  Should be between zero and one.  Uses built-in functions for in=memory compression
#'
#' @name compression_index
#' @param df  a data frame containing occurrences or events
#' @param CF  a column or contextual factor in that data frame
#' @return number containing compressibility index, 0 < i < 1
#' @export
compression_index <- function(df,CF){ return(
    length(memCompress(paste0(as.character(df[[CF]])),type="gzip")) /
    length(paste0(as.character(df[[CF]]))) ) }


#######################################################################
#compute entropy for a set of observations in a column from a data frame
# freq is typically going to the $freq column from ngram table, or
# the frequency of each level in the CFs, as counted by table()
#' Compute the entropy of a contextual factor
#'
#' Each column in the raw data represents a contextual factor.  This function computes the entropy of each factor that is selected for use in the
#' analysis.
#' @name compute_entropy
#' @param freq is the frequency distribution of the levels in the factor
#' @return number
#' @export
compute_entropy <- function(freq){
  N = sum(freq)
  p = freq/N
  plnp = p*log(p)
  return(-sum(plnp))
}

# code to plot entropy as a function of zoom_level
# need to get the zoom levels -- grep out the 'Z_' column names...
#' @name plot_entropy
#' @param e  data from of events with zoom levels
#' @return R plot
#' @export
plot_entropy <- function(e){
 plot(unlist(lapply(grep('ZM_',colnames(e)),function(i){compute_entropy(table(e[[i]]))})))
}
