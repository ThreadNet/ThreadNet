##########################################################################################################
# THREADNET:  SHINY GLOBAL

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

# 15 October Point of view code has solidified somewhat
# 20 October Starting on NetworkD3
# June 6 finishing up Package


# add the dependent packages
suppressPackageStartupMessages({
	library(shiny)
	library(shinyjs)
	library(plotly)
	library(ggplot2)
	library(tidyverse)
	library(ngram)
	library(stringr)
	library(stringdist)
	library(igraph)
	library(networkD3)
	library(visNetwork)
	library(xesreadR)
	library(colorspace)
	library(DT)
	library(RColorBrewer)
	library(lubridate)
	library(knitr)
	library(ThreadNet)
})

# visualization types for UI dropdowns
visualizations <- c(
	'Threads (event time)',
	'Threads (actual time)',
	'Threads (relative time)',
	'Event network (circle)',
	'Event network (force)',
	'Other networks',
	'Role Maps',
	'Thread Trajectories'
)

# load functions
# source(file="ThreadNet_Core.R")
# source(file="ThreadNet_Misc.R" )
# source(file="ThreadNet_Graphics.R" )
# source(file="ThreadNet_Metrics.R" )
# source(file="Event_Mappings.R" )


# Set time zone default to avoid errors
Sys.setenv(TZ='GMT')

# This is where we store the list of event mappings.
Global_POV <<- list()
Global_POV_Name <<- list()
Global_POV_Event_CF <<- list()
Global_POV_Thread_CF <<- list()

