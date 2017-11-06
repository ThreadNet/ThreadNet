##########################################################################################################
# THREADNET:  SHINY SERVER

# (c) 2017 Michigan State University. This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# The Shiny server mainly calls functions in other files.

server <- shinyServer(function(input, output, session) {
  options(warn=-1)

  ###  Some basic functions that are used to structure the data as it moves through the pipeline

  # make dataframe of occurrences that depends only file1
  occ <- eventReactive(input$file1,read_occurrences(input$file1))


  #selected columns from the raw data, and the topPct of the table
  selectOcc = reactive(topPctOfTable(occ()[c("tStamp", input$CFcolumnsID)],input$occRowsToInclude ))

  # # recode the occurrences for the thresholds...
  # filterOcc = reactive({recodeThreshold(selectOcc(),get_CF(), cfthresh())})
  #
  # Sort and add columns for threadNum and seqNum for the selected POV
  threadedOcc = reactive({
   ThreadOccByPOV(selectOcc(),get_THREAD_CF(),get_EVENT_CF()) })


  ######  From here on down, we are working with events, not occurrences   #######
  threadedEvents <- reactive({
    input$EventButton
    isolate(OccToEvents(threadedOcc(),
                        input$Event_method_ID,
                        input$uniform_chunk_slider,
                        input$Threshold_slider,
                        input$CHUNK_CF_ID,
                        input$EventMapName,
                        get_COMPARISON_CF(),
                        get_timeScale()) )})


  ##################################################
  # capture reactive values from the UI that are needed elsewhere
  # note the "<<-" assignment, which gives the function global scope

  # get_CF returns the choice of contextual factors from the Data tab.
  get_CF <<- reactive({ return( input$CFcolumnsID ) })

  # get threshold values from the context sliders and buttons on POV tab
  # cfthresh <<- reactive(getCFSliderValues( input, length( get_CF() )))
  # cfbuttons <<- reactive(getCFButtonValues( input, length( get_CF() )))

  # get_POV returns the choice of POV from the POV tab
  get_THREAD_CF <<- reactive({ return(input$THREAD_CF_ID) })
  get_EVENT_CF <<- reactive({ return(input$EVENT_CF_ID) })
  get_COMPARISON_CF <<- reactive({ return(input$COMPARISON_CF_ID) })


  # get an environment here for storing/retriving the information about the events...
  get_ENV <<- reactive({ return(input$EventMapName) })

  # time scale for use throughout the app
  get_timeScale <<- reactive({ return(input$timeScaleID) })

  # This slider controls the zoom level for zooming in-out
  get_Zoom_TM <<- reactive({ return(paste0("E_",input$ThreadMapZoomID))})



  ################## 1.DATA TAB ####################

  output$Data_Tab_Controls_1 =  renderUI({
    tags$div(align="center",
             fileInput("file1",
                       "Please select a .csv file",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
    )
  })

  output$Data_Tab_Controls_2 =  renderUI({
    tags$div(align="center",
             sliderInput("occRowsToInclude",
                         "Occurrences to use",
                         1,nrow(occ()),value=c(1,nrow(occ())),step=1,ticks=FALSE ),

             radioButtons("timeScaleID", "Select temporal granularity:",
                          c("secs","mins","hours","days"),selected="mins",inline=TRUE  )
    )

  })

  output$Data_Tab_Controls_3 =  renderUI({
    checkboxGroupInput("CFcolumnsID","Select columns to include in analysis:",
                       cfnames(occ()),
                       selected=cfnames(occ()),
                       inline=TRUE)
  })


  output$Data_Tab_Output_1 = renderText(paste( nrow(selectOcc()),"Occurrences //",
                                        timeRangePhrase(timeRange(selectOcc())))
  )

  output$Data_Tab_Output_2  = renderTable({ selectOcc() })



  ##################### 2.POV  tab ################################


  # this paints the nice pie charts including the COMBINED column
  output$ContextFlowers_1 = renderPlotly({
    CF_multi_pie(selectOcc(), get_COMPARISON_CF(),length(get_CF()) )
  })

  output$ContextFlowers_2 = renderPlotly({
    CF_multi_pie(selectOcc(), get_THREAD_CF(),length(get_CF()) )
  })

  output$ContextFlowers_3 = renderPlotly({
    CF_multi_pie(selectOcc(), get_EVENT_CF(),length(get_CF()) )
  })

  output$rawOccurrenceThreadMap <- renderPlot({
    traminer_threadMap(threadedOcc(), "POVthreadNum", newColName(get_EVENT_CF())  )
  })

  output$rawOccurrenceThreadMap_2 <- renderPlotly({
    threadMap(threadedOcc(), "POVthreadNum", "tStamp", newColName(get_EVENT_CF())  )
  })
  # output$rawOccurrenceThreadMap_3 <- renderPlotly({
  #   threadMap_test(threadedOcc(), "POVthreadNum", "tStamp", newColName(get_EVENT_CF())  )
  # })

  output$Preview_Thread_Output_1 <- renderText({ paste(numThreads(threadedOcc(), "POVthreadNum"),"threads in the selected data.")})

  output$rawOccurrenceNetwork <- renderVisNetwork({
    eventNetwork(threadedOcc(), "POVthreadNum", newColName(get_EVENT_CF())) })

  output$Thread_Tab_Output_1  = renderTable({ threadedOcc()  })

  # need to create unique ID for each radiobutton based on the CF name
  output$POV_Tab_Controls_1 <- renderUI({
    checkboxGroupInput("COMPARISON_CF_ID","Select columns for comparison:",
                       cfnames(selectOcc()),
                       selected =  get_COMPARISON_CF(),
                       inline=TRUE)
    })

  output$POV_Tab_Controls_2 <- renderUI({
    checkboxGroupInput("THREAD_CF_ID","Select columns to define threads:",
                       cfnames(selectOcc()),
                       selected =  get_THREAD_CF(),
                       inline=TRUE)
  })

  output$POV_Tab_Controls_3 <- renderUI({
    checkboxGroupInput("EVENT_CF_ID","Select columns to mark events:",
                       cfnames(selectOcc()),
                       selected =  get_EVENT_CF(),
                       inline=TRUE)
  })



  ##################### 3.OCC to EVENT  tab ################################

  output$Event_Tab_Controls_1 <- renderUI({tags$div(

    radioButtons("Event_method_ID", "Select method to map occurrences to events:",
                 c('Variable chunks','Uniform chunks', 'Time gap'), selected="Variable chunks", inline=TRUE))
    })

    output$Event_Tab_Controls_2 <- renderUI({
      if (input$Event_method_ID == 'Variable chunks')
      {tags$div(checkboxGroupInput("CHUNK_CF_ID","Start new event when ALL of these change:",
                                   get_EVENT_CF(),
                                   selected =  input$CHUNK_CF_ID,
                                   inline=TRUE))} else if (input$Event_method_ID == 'Uniform chunks')
                                   {tags$div(sliderInput("uniform_chunk_slider", "Select chunk size:", 1,20,1, ticks=FALSE))}
      else
        {sliderInput("Threshold_slider",
                    "Minimum time gap between distinct chunks:",
                    threshold_slider_min(threadedOcc()),
                    threshold_slider_max(threadedOcc()),
                    threshold_slider_selected(threadedOcc()),
                    step = 1, ticks=FALSE)}
    })

    output$Event_Tab_Controls_3 <- renderUI({tags$div(
    textInput("EventMapName", label = h4("Enter label to store/retrieve this mapping"), value = ""),
    actionButton("EventButton", "Go"))
  })

    # show the bar chart
    output$threadGapBarchart <- renderPlotly({
      threadGapBarchart(threadedOcc(),input$Event_method_ID)
    })

  output$Event_Tab_Output_1  = renderText( "Add a nested tabset with table, threadmap, and ngrams... " )

  output$Event_Tab_Output_2  = renderTable({ threadedEvents() })


  ##################### 5.THREAD  tab ################################

  output$Thread_Tab_Controls_1 <- renderUI({tags$div(
  radioButtons("displayTimeForThreads", "Display threads by:",
               c("event time" = "event",
                 "clock time" = "clock"),inline=TRUE))
    sliderInput("ThreadMapZoomID",
                "Zoom in and out by event similarity:",
                1,100,5, step = 1, ticks=FALSE)})

  # output$threadMapEvents <- renderPlotly({
  #   threadMap(threadedEvents(),"threadNum", "seqNum",get_Zoom_TM()) })

  output$threadMapEvents <- renderPlot({
    traminer_threadMap(threadedEvents(), "threadNum", get_Zoom_TM())
  })


  ##################### NGRAM  tab ################################


  output$nGramBarchart = renderPlotly({
    ng_bar_chart(threadedEvents(), "threadNum", get_Zoom_TM(), input$nGramLengthID, input$nGramDisplayThresholdID)
  })

  # controls for the sequences pages
  output$nGramControls <- renderUI({
    tagList(
      # sliderInput("nGramZoomID",
      #             "Zoom in and out by event similarity:",
      #             1,20,5, step = 1, ticks=FALSE),
      sliderInput("nGramLengthID","nGram Size", 1,10,2,step=1,ticks=FALSE ),
      sliderInput("nGramDisplayThresholdID","Display threshold", 1,50,1,step=1,ticks=FALSE )
    )
  })


  ##################### 6.NETWORK  tab ################################

  output$Network_Tab_Controls_1 <- renderUI({tags$div(

    sliderInput("NetworkZoomID",
                "Zoom in and out by event similarity:",
                2,100,2, step = 1, ticks=FALSE))})

  # output$Network_Tab_Output_1 <- renderUI({tags$div(
  #
  #   renderText(paste( "Network complexity index =",
  #                     estimate_network_complexity()
  #                      ))
  #
  # )})


  output$eventNetwork <- renderVisNetwork({
    eventNetwork(threadedEvents(), "threadNum", get_Zoom_TM()) })

  output$Network_Tab_Controls_2 <- renderUI({tags$div(
    checkboxGroupInput("NetworkGroupID","Select columns for comparison:",
                       get_COMPARISON_CF(),
                       selected =get_COMPARISON_CF()[1],
                       inline=TRUE))})

  output$eventNetworkD3 <- renderForceNetwork({
    eventNetworkD3(threadedEvents(), "threadNum", input$NetworkGroupID, get_Zoom_TM())
  })


  ########################  COMPARISON TAB ##############################

  # Get subsets of threadedEvents and create sub-plots for them


  CF_levels = reactive( get_CF_levels( threadedEvents(),input$selectComparisonID) )

  # controls for the comparison input panels
  # Use all of the column names here...
  output$Comparison_Tab_Controls_1 <- renderUI({
    selectizeInput("selectComparisonID","Compare by:", get_COMPARISON_CF())
  })

  output$Comparison_Tab_Controls_2 <- renderUI({
    tagList(
      selectizeInput("selectComparisonGroupsID","Compare specific groups:",
                     CF_levels(), multiple=TRUE),

      radioButtons("NumTimePeriodsToCompare", "How many time periods to compare:",
                   c(1,2,3,4,5), selected =1 ,inline=TRUE),

      sliderInput("nGramLengthCompID","nGram Size", 1,10,2,step=1,ticks=FALSE ),
      sliderInput("ComparisonZoomID",
                  "Zoom in and out by event similarity:",
                  1,100,5, step = 1, ticks=FALSE)
    )
  })

  output$Comparison_Plots <- renderPlotly(
#    input$visualize_DV_button
  Comparison_Plots(threadedEvents(),
                            input$selectComparisonID,
                            input$selectComparisonGroupsID,
                            input$NumTimePeriodsToCompare,
                            input$nGramLengthCompID,
                            get_Zoom_COMP()) )




  output$Moving_Window_Tab_Controls_1 <- renderUI({
    sliderInput("MovingWindowSizeID","Window Size", 1,10,1,step=1,ticks=FALSE )
  })
  output$Moving_Window_Tab_Controls_2 <- renderUI({
    sliderInput("WindowLocationID","Window Location (pct)", 1,100,1,step=1,ticks=FALSE )
  })

  # just leave it blank for now...
 output$MovingWindow_Plot <- renderPlotly( plotly_empty() )


  ############################  Admin, params, etc  ##############################

  output$currentParameterSettings <- renderTable({

    # start with an empty stucture and add rows.
    p<- NULL

    # Add each name-value pair... adjust as necessary.  Lists need to be pasted and unlisted...
    p <- addRow( p, "File name", input$file1[1] )
    p <- addRow( p, "Columns to include",  paste( unlist(input$CFcolumnsID), collapse=', ') )
    p <- addRow( p, "Range of occurrences included",  paste( unlist(input$occRowsToInclude), collapse=', ') )
    p <- addRow( p, "Temporal granularity",  input$timeScaleID  )

    p <- addRow( p, "Define threads by",  paste( unlist(input$THREAD_CF_ID), collapse=', ') )
    p <- addRow( p, "Define events by",  paste( unlist(input$EVENT_CF_ID), collapse=', ') )
    p <- addRow( p, "Make comparisons by",  paste( unlist(input$COMPARISON_CF_ID), collapse=', ') )


    # convert to data frame and add column names
    p <- as.data.frame(p)
    names(p) <- c("Parameter","Value")

    # return the name-value data frame
    p
  })

  addRow <- function(Vals, Name, Value){ return( rbind(Vals, c(as.character(Name), as.character(Value))))  }


  ############################  Explantory tool tips ##############################
 # requires package shinybs
   # addTooltip(session, "PctOccToDisplayID", "For large data sets, this can be helpful", placement = "bottom", trigger = "hover",
  #            options = NULL)


})
