##########################################################################################################
# THREADNET:  SHINY GLOBAL

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# 15 October Point of view code has solidified somewhat
# 20 October Starting on NetworkD3

library(shiny)
library(plotly)
library(tidyverse)
library(ngram)
library(stringr)
library(stringdist)
library(ggplot2)
library(networkD3)
library(xesreadR)
library(colorspace)
library(igraph)
library(DT)
library(RColorBrewer)
library(lubridate)
library(visNetwork)

# this is another change

source("ThreadNet_Core.R")
source("ThreadNet_Misc.R")
source("ThreadNet_Graphics.R")
source("ThreadNet_Metrics.R")

# Global variables, config settings, etc. can be defined here

# This is where we store the list of event mappings.  Reactive bindings are added in server.R
GlobalEventMappings <<-list()

