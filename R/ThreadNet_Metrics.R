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
#' @param net
#'
#' @return number
#' @export
#'
#' @examples
estimate_network_complexity <- function(net){ return(estimate_task_complexity_index( nrow(net$nodeDF), nrow(net$edgeDF)) ) }

# this version takes vertices and edges
#' Estimates the number of paths in a directed graph
#'
#' Same as estimate_network_complexity, but takes different parameters
#'
#' @param v number of vertices (or nodes)
#' @param e number of edges
#'
#' @return number
#' @export
estimate_task_complexity_index <- function(v,e){

  #INPUT ARGS:
  # in MatLab version, arguments were v (vertices) and e (edges)
  # v = number of vertices
  # tested for range of 10 < v < 100
  # e = number of edges

  #
  # OUTPUT ARG:
  # cidx correlates with Log10(simple paths) with r>= 0.8

  # from ORM paper analysis, constant is 0.12.
  # For boundary condition of 2 nodes and 1 edge, complexity index=0, constant = 0.08
  return(  0.08 + 0.08*e - 0.08*v )
}

# compute Rt for a set of observations in a column from a data frame
# Rt = the fraction (0 < Rt < 1) of the data that conforms to the patterns
# freq = same set of ngrams used for simpson's D, typically the largest portion of the distribution
# total N = sum of occurrences -- should include ALL, not just the ones that
#    were included in the ngrams
#  n = ngram length
#' Title
#'
#' @param freq
#' @param n
#' @param totalN
#'
#' @return
#' @export
#'
#' @examples
compute_Rt <- function(freq, n, totalN){

  # count # of occurrences that match the observed ngrams
  # just multiply count * length
  N = sum(freq)*n

  # return the fraction of the total
  return(N/totalN)
}


# Freq is from the ngram phrasetable
#' Title
#'
#' @param o
#' @param TN
#' @param CF
#' @param n
#' @param m
#'
#' @return
#' @export
#'
#' @examples
routineness_metric <- function(o,TN,CF,n,m){

  # get the ngrams
  ng=count_ngrams(o,TN,CF,n)

print(ng[1:m,])

  # return the ratio of occurrences in the top m most frequent ngrams to total occurrences
    return( sum(ng$freq[1:m])*n/nrow(o) )
}

# compressibility as an index of complexity.
# use built-in functions for in=memory compression
# normalized by length, so it's a compression ratio
# near zero = highly repetitive.  Near 1 = nearly random
#' Title
#'
#' @param df
#' @param CF
#'
#' @return
#' @export
#'
#' @examples
compression_index <- function(df,CF){ return(
    length(memCompress(paste0(as.character(df[[CF]])),type="gzip")) /
    length(paste0(as.character(df[[CF]]))) ) }


#compute entropy for a set of observations in a column from a data frame
# freq is typically going to the $freq column from ngram table, or
# the frequency of each level in the CFs, as counted by table()
#' Title
#'
#' @param freq
#'
#' @return
#' @export
#'
#' @examples
compute_entropy <- function(freq){
  N = sum(freq)
  p = freq/N
  plnp = p*log(p)
  return(-sum(plnp))
}
