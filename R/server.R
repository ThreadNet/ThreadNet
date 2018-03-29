##########################################################################################################
# THREADNET:  SHINY SERVER

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################
# The Shiny server mainly calls functions in other files.

server <- shinyServer(function(input, output, session) {
  options(warn=-1)
  options(shiny.maxRequestSize=30*1024^2)

  ##### make the global variables reactive  #########
  # # One for the event mappings
     observe( makeReactiveBinding("GlobalEventMappings", env=.GlobalEnv) )
  #
  # # One for the event clusters
     observe( makeReactiveBinding("GlobalEventClusters", env=.GlobalEnv) )


  ###  Some basic functions that are used to structure the data as it moves through the pipeline

  # make dataframe of occurrences that depends only file1
  occ <- eventReactive(input$file1,read_occurrences(input$file1))


  #selected columns from the raw data, and the subset of the table
  selectOcc = reactive(occ()[c("tStamp", input$CFcolumnsID)] )

  selectOccFilter = reactive(selectOcc()[input$Data_Tab_Output_2_rows_all,])

  # # recode the occurrences for the thresholds...
  # filterOcc = reactive({recodeThreshold(selectOcc(),get_CF(), cfthresh())})
  #
  # Sort and add columns for threadNum and seqNum for the selected POV
  threadedOcc = reactive({
    ThreadOccByPOV(selectOccFilter(),get_THREAD_CF(),get_EVENT_CF()) })


  ######  From here on down, we are working with events, not occurrences   #######
  threadedEventCluster <- reactive({
    input$EventButton1
    isolate(OccToEvents1(threadedOcc(),
                        input$EventMapName1,
                        get_EVENT_CF(),
                        get_COMPARISON_CF()
                        ) )})

  # These go on the occ to event page
  threadedEvents <- reactive({make_nice_event_DT(threadedEventCluster()[["threads"]])})
 # threadedCluster <- reactive({threadedEventCluster()[["cluster"]]})

  # this is for the chunks
  threadedEventCluster2 <- reactive({
    input$EventButton2
    isolate(OccToEvents2(threadedOcc(),
                         input$EventMapName2,
                         get_EVENT_CF(),
                         get_COMPARISON_CF()
    ) )})

  # These go on the occ to event page
  threadedEvents2 <- reactive({make_nice_event_DT(threadedEventCluster2()[["threads"]])})
 # threadedCluster2 <- reactive({threadedEventCluster2()[["cluster"]]})



  # These will work for the Visualize tab.  Need parallel functions for the other tabs.
  threadedEventsViz <- reactive({ print(paste0('reactive inputID', input$VisualizeEventMapInputID))
                            get_event_mapping_threads( GlobalEventMappings, input$VisualizeEventMapInputID ) })
#  threadedClusterViz <- reactive({ get_event_mapping_cluster( GlobalEventMappings, input$VisualizeEventMapInputID ) })


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
  get_COMPARISON_CF <<- reactive({ return(setdiff(get_CF(), union(get_THREAD_CF(),get_EVENT_CF() ))) })


  # get an environment here for storing/retriving the information about the events...
  get_ENV <<- reactive({ return(input$EventMapName) })

  # time scale for use throughout the app
  get_timeScale <<- reactive({ return(input$timeScaleID) })

  # This slider controls the zoom level for zooming in-out
  get_Zoom_TM <<- reactive({ return( ifelse (input$VisualizeEventMapInputID =="One-to-One", "ZM_1", paste0("ZM_",input$ThreadMapZoomID))) })
  get_Zoom_COMP <<- reactive({ return( ifelse (input$CompareMapInputID =="One-to-One", "ZM_1", paste0("ZM_",input$ComparisonZoomID))) })



  ################## 1.READ DATA TAB ####################

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
    checkboxGroupInput("CFcolumnsID","Select columns to include in analysis:",
                       cfnames(occ()),
                       selected=cfnames(occ()),
                       inline=TRUE)
  })

  # output$Data_Tab_Output_1 = renderText(paste( nrow(selectOcc()),"Occurrences //",
  #                                              timeRangePhrase(timeRange(selectOcc())))
  # )

  output$Data_Tab_Output_2  = DT::renderDataTable({
    selectOcc()
  }, filter = "top")


  ##################### 2.POV  tab ################################


  # this paints the nice pie charts including the COMBINED column

  output$ContextFlowers_2 = renderPlotly({
    CF_multi_pie(selectOccFilter(), get_THREAD_CF()  )
  })

  output$ContextFlowers_3 = renderPlotly({
    CF_multi_pie(selectOccFilter(), get_EVENT_CF()  )
  })

  output$rawOccurrenceThreadMap <- renderPlotly({
    threadMap(threadedOcc(), "POVthreadNum", "POVseqNum", newColName(get_EVENT_CF()), 15  )
  })

  output$rawOccurrenceThreadMap_2 <- renderPlotly({
    threadMap(threadedOcc(), "POVthreadNum", "tStamp", newColName(get_EVENT_CF()), 16  )
  })


  output$Preview_Thread_Output_1 <- renderText({ paste(numThreads(threadedOcc(), "POVthreadNum"),"threads in the selected data.")})

  output$Preview_Network_Tab_Controls_0 <- renderUI({
    radioButtons("Timesplit", "Time Measure:", choices = c('POVseqNum','timeGap'), selected="POVseqNum", inline=TRUE)
  })

  output$rawOccurrenceNetwork <- renderPlotly({
    req(input$Timesplit)
    eventNetwork(threadedOcc(), "POVthreadNum", newColName(get_EVENT_CF()), input$Timesplit)
  })

  output$Thread_Tab_Output_1  = DT::renderDataTable({ threadedOcc()  })

  # need to create unique ID for each radiobutton based on the CF name
  output$POV_Tab_Controls_1 <- renderUI({
    checkboxGroupInput("COMPARISON_CF_ID","Select columns for comparison:",
                       cfnames(selectOccFilter()),
                       selected =  get_COMPARISON_CF(),
                       inline=TRUE)
  })

  output$POV_Tab_Controls_2 <- renderUI({
    checkboxGroupInput("THREAD_CF_ID","Select columns to define threads:",
                       cfnames(selectOccFilter()),
                       selected =  get_THREAD_CF(),
                       inline=TRUE)
  })

  output$POV_Tab_Controls_3 <- renderUI({
    checkboxGroupInput("EVENT_CF_ID","Select columns to mark events:",
                       cfnames(selectOccFilter()),
                       selected =  get_EVENT_CF(),
                       inline=TRUE)
  })



  ##################### 3.OCC to EVENT  tab ################################

  output$One_to_One_controls  = renderUI({
    tags$div(align="left",
             tags$h4("One-to-One: Each occurrence in the raw data is interpreted as an event (INPUT = Occurrences)."),
             tags$p(" "),
             textInput("EventMapName1", label = h4("Enter label for this mapping:"), value = "One-to-One"),
             actionButton("EventButton1", "Create New Mapping")  )

  })

    output$Contextual_Chunk_controls = renderUI({
      tags$div(align="left",
               tags$h4("Context-based chunks: Occurrences are grouped into events based on changes in contextual factors (INPUT = Occurrences)."),
               tags$p(paste0("Start new event when ALL of these change:", get_EVENT_CF())),

               textInput("EventMapName2", label = h4("Enter label for this mapping"), value = "Chunks-"),

               actionButton("EventButton2", "Create New Mapping")  )

    })

      output$Regular_Expression_controls = renderUI({
        tags$div(align="left",
                 tags$h4("Regular Expressions: Use regular expressions to form events -- Not implemented yet"),
                 tags$p(" "),
                 selectizeInput("RegExInputID",label = h4("Choose input for this mapping:"), get_event_mapping_names( GlobalEventMappings )  ),
                 tags$p(" "),
                 textInput("RegExForEvents", label = h4("Enter regular expression(s)"), value = ""),
                 tags$p(" "),

                 textInput("EventMapName3", label = h4("Enter label for this mapping"), value = "RegEx-"),

                 actionButton("EventButton3", "Create New Mapping")  )

      })

        output$Frequent_Ngram_controls = renderUI({
          tags$div(align="left",
                   tags$h4("Frequent ngrams: Select ngrams to use in forming events -- Not implemented yet"),
                   tags$p(" "),
                   selectizeInput("NGramInputID","INPUT:", get_event_mapping_names( GlobalEventMappings ) ),

                   textInput("EventMapName4", label = h4("Enter label for this mapping"), value = "Ngrams-"),

                   actionButton("EventButton4", "Create New Mapping")  )

        })

          output$Maximal_Pattern_controls = renderUI({
            tags$div(align="left",
                     tags$h4("Maximal patterns: Form events based on maximal patterns-- Not implemented yet"),

                     selectizeInput("MaximalPatternInputID",label = h4("Choose input for this mapping:"), get_event_mapping_names( GlobalEventMappings ) ),

                     textInput("EventMapName5", label = h4("Enter label for this mapping"), value = "Maximal-"),

                     actionButton("EventButton5", "Create New Mapping")  )

          })

          output$Cluster_Event_controls = renderUI({
            tags$div(align="left",
                     tags$h4("Cluster Events: Group similar events to together to allow zooming"),

                     selectizeInput("ClusterEventsInputID",label = h4("Choose mapping for clustering:"), get_event_mapping_names( GlobalEventMappings ) ),

                     radioButtons("ClusterMethodID", "Cluster based on:", choices = c("Sequential similarity", "Contextual Similarity"), selected="Sequential similarity", inline=TRUE),

                     actionButton("EventButton6", "Cluster Events")  )

          })


          output$clusterResult <- renderDendroNetwork({
            input$EventButton6
            dendroNetwork(clusterEvents( get_event_mapping_threads( GlobalEventMappings,
                                                                    input$ClusterEventsInputID),
                                         ClusterEventsInputID,
                                         input$ClusterMethodID ),
                          treeOrientation = "vertical", textColour = "black")
          })


            output$Manage_Event_Map_controls= renderUI({
              tags$div(align="left",
                       tags$h4("Select event mapping to export or delete -- Not implemented yet"),

                       selectizeInput("ManageEventMapInputID",label = h4("Choose mapping:"), get_event_mapping_names( GlobalEventMappings ) ),

                       actionButton("ExportMappingButton", "Export"),
                       actionButton("DeleteMappingButton", "Delete") )

            })

            # reactive functions for the export and delete buttons
            deleteEventMapping <- reactive({
              input$DeleteMappingButton
              delete_event_mapping( GlobalEventMappings, input$ManageEventMapInputID )
              })

            exportEventMapping <- reactive({
              input$ExportMappingButton
              export_event_mapping( GlobalEventMappings, input$ManageEventMapInputID )
            })


            output$One_to_one_Tab_Output_1  = DT::renderDataTable({
              threadedEvents()
            }, filter = "top")

            output$Contextual_Chunks_Tab_Output_1  = DT::renderDataTable({
              threadedEvents2()
            }, filter = "top")

            output$Event_Tab_Output_4  = renderDendroNetwork({
              #plot(threadedCluster())
              dendroNetwork(threadedCluster(), treeOrientation = "vertical", textColour = "black")
            })

            output$dendro_test = renderPlot({
              plot(threadedCluster())
            })


  #
  # # show the bar chart
  # output$threadGapBarchart <- renderPlotly({
  #   threadGapBarchart(threadedOcc(),input$Event_method_ID)
  # })

  # output$Event_Tab_Output_3  = renderPlot({ if (is.null(threadedCluster())) {plot(table(threadedEvents()["ZM_1"]))} else {plot(threadedCluster()) }})
  #
  # output$Event_Tab_Output_3  = renderPlotly({ ng_bar_chart(threadedEvents(), "threadNum", "ZM_1", 1, 1)} )





  ##################### 4.VISUALIZE tab ################################
  # Controls for the whole set of tabs
output$Visualize_Tab_Controls_1 = renderUI({
  selectizeInput("VisualizeEventMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names( GlobalEventMappings ), selected='One-to-One' )
})

  output$Visualize_Tab_Controls_2 = renderUI({
    if (input$VisualizeEventMapInputID == "One-to-One")
       {tags$h4("Zooming not available with one-to-one mapping of occurrences to events")}
    else
        {sliderInput("ThreadMapZoomID",
               "Zoom in and out by event similarity:",
               1,100,5, step = 1, ticks=FALSE) }
})

  # controls for sub-sequence display
  output$nGramControls <- renderUI({
    tagList(
      sliderInput("nGramLengthID","nGram Size", 1,10,2,step=1,ticks=FALSE ),
      sliderInput("nGramDisplayThresholdID","Display threshold", 1,50,1,step=1,ticks=FALSE )
    )
  })

  #  NGRAM  display #
  output$nGramBarchart = renderPlotly({
    ng_bar_chart(threadedEventsViz(), "threadNum", get_Zoom_TM(), input$nGramLengthID, input$nGramDisplayThresholdID)
  })

# Whole sequence display
  output$WholeSequenceThreadMap <- renderPlotly({
    threadMap(threadedEventsViz(), "threadNum", "seqNum", newColName(get_EVENT_CF()), 15  )
  })

  ##################### 5. COMPARE  tab ################################

  output$Comparison_Tab_Controls_1 <- renderUI({
    selectizeInput("CompareMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names( GlobalEventMappings ) )
    })

  output$Comparison_Tab_Controls_2 <- renderUI({
    radioButtons("CompareTimeSubsetID", "How many time intervals to compare:", choices = c(1, 2, 3, 4, 5, 6), selected="1", inline=TRUE)
  })

  output$Comparison_Tab_Controls_3 <- renderUI(
    if (input$CompareMapInputID == "One-to-One")
    {tags$p("Zooming not available with one-to-one mapping of occurrences to events")}
    else
    {sliderInput("CompareZoomID",
                 "Zoom in and out by event similarity:",
                 1,100,5, step = 1, ticks=FALSE) }
    )

  ######
  output$Pos_Layout_Controls_0 <- renderUI({
    radioButtons("Timesplit2", "Time Measure:", choices = c('seqNum'='seqNum.1','timeGap'='timeGap'), selected="seqNum.1", inline=TRUE)
  })

  # use this to select how to color the nodes in force layout
  output$Network_Tab_Controls_2 <- renderUI({tags$div(
    radioButtons("NetworkGroupID","Select dimension for coloring nodes:",
                       choices = cfnames(selectOccFilter()),
                       selected =  cfnames(selectOccFilter())[1],
                       inline=TRUE))
  })


  # output$eventNetwork <- renderVisNetwork({
  #   req(input$Timesplit2)
  #   eventNetwork(threadedEvents(), "threadNum", get_Zoom_TM(), input$Timesplit2)
  # })

  event.data <- reactive({
    event_data("plotly_click", source="A")
    # click_data = event_data("plotly_click", source="A")
    # click_data$pointNumber = click_data$pointNumber+1
    # click_data$click_name = paste(input$EVENT_CF_ID, click_data$pointNumber, sep="")
    # click_data
  })

  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })

  #EVENT_CF_levels = reactive( get_CF_levels( threadedEvents(), get_EVENT_CF()) )

  eventNetworksubset <- reactive({
    req(event.data())
    TE = threadedEvents()
    #CF_levels()
    #newColName(get_EVENT_CF()) input$EVENT_CF_ID
    ENsubset = subset(TE, actor == event.data()$key)
    #ENsubset = subset(TE,  as.numeric(gsub("\\D", "", actor)) == event.data()$pointNumber)
    ENsubset
  })

  # output$eventNetworksubset_plot <- renderVisNetwork({
  #   eventNetwork(eventNetworksubset(), "threadNum", get_Zoom_TM(), input$Timesplit2)
  # })

  output$eventNetworksubset_data <- renderDataTable({
    test<-eventNetworksubset()
    test
    #event.data()
  })


  output$eventNetworkD3 <- renderForceNetwork({
    eventNetworkD3(threadedEvents(), "threadNum", input$NetworkGroupID, get_Zoom_TM())
  })



  # Get subsets of threadedEvents and create sub-plots for them


  CF_levels = reactive( get_CF_levels( threadedEvents(),input$selectComparisonID) )

  # controls for the comparison input panels
  # Use all of the column names here...
  output$Comparison_Tab_Controls_1_old <- renderUI({
    selectizeInput("selectComparisonID","Compare by:", get_COMPARISON_CF())
  })

  output$Comparison_Tab_Controls_2_old <- renderUI({
    tagList(
      selectizeInput("selectComparisonGroupsID","Compare specific groups:",
                     CF_levels(), multiple=TRUE),

      radioButtons("NumTimePeriodsToCompare", "How many time periods to compare:",
                   c(1,2,3,4,5), selected =1 ,inline=TRUE),

      sliderInput("nGramLengthCompID","nGram Size", 1,10,2,step=1,ticks=FALSE )

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


  ######################## 6. MOVING WINDOW TAB ##############################

  output$Moving_Window_Tab_Controls_1 <- renderUI({
    selectizeInput("MovingWindowMapInputID",label = h4("Choose mapping:"), get_event_mapping_names( GlobalEventMappings ) )
  })

  output$Moving_Window_Tab_Controls_2 <- renderUI(
    if (input$MovingWindowMapInputID == "One-to-One")
    {tags$p("Zooming not available with one-to-one mapping of occurrences to events")}
    else
    {sliderInput("MovingWindowZoomID",
                 "Zoom in and out by event similarity:",
                 1,100,5, step = 1, ticks=FALSE) }
  )

  output$Moving_Window_Tab_Controls_3 <- renderUI({
    sliderInput("MovingWindowSizeID","Window Size", 1,20,1,step=1,ticks=FALSE )
  })
  output$Moving_Window_Tab_Controls_4 <- renderUI({
    sliderInput("WindowLocationID","Window Location", 1,numThreads(threadedEvents(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  output$Moving_Tab_Controls_5 <- renderUI({
    radioButtons("Timesplit3", "Time Measure:", choices = c('seqNum','timeGap'), selected="seqNum", inline=TRUE)
  })

  output$test <- renderDataTable({
    w = get_moving_window(threadedEvents(),input$MovingWindowSizeID, input$WindowLocationID )
    w
  })

  # just leave it blank for now...
  output$MovingWindow_Plot <- renderPlotly({
    w = get_moving_window(threadedEvents(),input$MovingWindowSizeID, input$WindowLocationID )
    eventNetwork(w, "threadNum", get_Zoom_TM(), input$Timesplit3) })


  ############################  Admin, params, etc  ##############################

  output$currentParameterSettings <- renderTable({

    # start with an empty stucture and add rows.
    p<- NULL

    # Add each name-value pair... adjust as necessary.  Lists need to be pasted and unlisted...
    p <- addRow( p, "File name", input$file1[1] )
    p <- addRow( p, "Columns to include",  paste( unlist(input$CFcolumnsID), collapse=', ') )
    # p <- addRow( p, "Range of occurrences included",  paste( unlist(input$occRowsToInclude), collapse=', ') )
    # p <- addRow( p, "Temporal granularity",  input$timeScaleID  )

    p <- addRow( p, "Define threads by",  paste( unlist(input$THREAD_CF_ID), collapse=', ') )
    p <- addRow( p, "Define events by",  paste( unlist(input$EVENT_CF_ID), collapse=', ') )


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
