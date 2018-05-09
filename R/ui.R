##########################################################################################################
# THREADNET:  SHINY UI

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# Sept 7, 2017 New Shiny R version using architecture and advice from Ezra Brooks & Pat Bills
# March 21, 2018 New organization of tabs.
# May 2, 2018 Separation of tab definitions into files

# pdf(NULL) # prevent plotly errors
library(shiny)
library(shinyjs)
library(networkD3)
library(visNetwork)

ui <- fluidPage(
    useShinyjs,
    visualizations <- c(
        'Threads (event time)',
        'Threads (actual time)',
        'Threads (relative time)',
        'Event network (circle)',
        'Event network (force)',
        'Other networks',
        'Role Maps',
        'Thread Trajectories'
    ),

    # Application title
    tags$h3(align='center', "ThreadNet 3 Development"),

    #  tags$audio( src='tellusastory.mp3',type='audio/mpeg', controls='TRUE'),

    # tab definitions are under "ui" directory
    tabsetPanel(
      id="navbar",
        type = "tabs",
        source(file.path("ui", "readData.R"),          local = TRUE)$value,
        source(file.path("ui", "choosePOV.R"),         local = TRUE)$value,
        source(file.path("ui", "visualize.R"),         local = TRUE)$value,
        source(file.path("ui", "subsets.R"),           local = TRUE)$value,
        source(file.path("ui", "comparisons.R"),       local = TRUE)$value,
        source(file.path("ui", "movingWindow.R"),      local = TRUE)$value,
        source(file.path("ui", "parameterSettings.R"), local = TRUE)$value,
        source(file.path("ui", "acknowledgements.R"),  local = TRUE)$value
    )
)
