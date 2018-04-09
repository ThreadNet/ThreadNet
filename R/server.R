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


  # time scale for use throughout the app
  get_timeScale <<- reactive({ return(input$timeScaleID) })


  # These sliders controls the zoom level for zooming in-out
  # they are grouoped here because hopefully they can be replaced by a single function... except that reactive functions don't take parameters
  get_Zoom_VIZ <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$VisualizeEventMapInputID))==1 ,
                                              "ZM_1", paste0("ZM_",input$VisualizeTabZoomID))) })

  get_Zoom_COMP_A <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$CompareMapInputID_A))==1 ,
                                               "ZM_1", paste0("ZM_",input$CompareZoomID_A))) })

  get_Zoom_COMP_B <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$CompareMapInputID_B))==1 ,
                                               "ZM_1", paste0("ZM_",input$CompareZoomID_B))) })

  get_Zoom_DIA_COMP <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$DiaCompareMapInputID))==1 ,
                                                 "ZM_1", paste0("ZM_",input$DiaCompareZoomID))) })

  get_Zoom_MOVE <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$MovingWindowMapInputID))==1 ,
                                              "ZM_1", paste0("ZM_",input$MovingWindowZoomID))) })

  get_Zoom_REGEX <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$RegExInputMapID))==1 ,
                                               "ZM_1", paste0("ZM_",input$regexZoomID))) })

  get_Zoom_freqNgram <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$freqNgramInputMapID))==1 ,
                                                    "ZM_1", paste0("ZM_",input$freqNgramZoomID))) })

  #################################################################
  ################## 1.READ DATA TAB ##############################
  #################################################################

  output$Data_Tab_Controls_1 =  renderUI({
    tags$div(align="center",
             fileInput("file1",
                       "Please select a .csv file",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                        ".csv")) )
  })

  output$Data_Tab_Controls_2 =  renderUI({
    checkboxGroupInput("CFcolumnsID","Select columns to include in analysis:",
                       cfnames(occ()),
                       selected=cfnames(occ()),
                       inline=TRUE)
  })

  output$Data_Tab_Output_2  = DT::renderDataTable({
                      selectOcc() }, filter = "top")

  #  dataframe for occurrences that are read in from file1
  occ <- eventReactive(input$file1,read_occurrences(input$file1))

  # selected columns from the raw data
  selectOcc = reactive(occ()[c("tStamp", input$CFcolumnsID)] )

  # select rows using the nice DT input
  selectOccFilter = reactive(selectOcc()[input$Data_Tab_Output_2_rows_all,])


  #################################################################
  ##################### 2.POV  tab ################################
  #################################################################

  # The POV tabs reconstruct the data into threads by sorting by tStamp and
  # adding columns for threadNum and seqNum for the selected POV in ThreadOccByPOV

  threadedOcc = reactive({
    ThreadOccByPOV(selectOccFilter(),get_THREAD_CF(),get_EVENT_CF()) })


  ########  define threads tab  ###############
  output$POV_Tab_Controls_2 <- renderUI({
    checkboxGroupInput("THREAD_CF_ID","Select columns to define threads:",
                       cfnames(selectOccFilter()),
                       selected =  get_THREAD_CF(),
                       inline=TRUE)
  })

  output$ContextFlowers_2 = renderPlotly({
    CF_multi_pie(selectOccFilter(), get_THREAD_CF()  )
  })

  ########  define events tab  ###############

  output$POV_Tab_Controls_3 <- renderUI({
    checkboxGroupInput("EVENT_CF_ID","Select columns to mark events:",
                       cfnames(selectOccFilter()),
                       selected =  get_EVENT_CF(),
                       inline=TRUE)
  })

  output$ContextFlowers_3 = renderPlotly({
    CF_multi_pie(selectOccFilter(), get_EVENT_CF()  )
  })

  ########  preview threads tab  ###############

  # need to provide ability to toggle between views - just use tStamp for now
  # could use conditional panel on the UI side? If so, the data table view could be included in the radio button

  output$previewThreadMap <- renderPlotly({
    threadMap(threadedOcc(), "POVthreadNum", "tStamp", newColName(get_EVENT_CF()), 16  )
  })

  # output$previewThreadMap <- renderPlotly({
  #   threadMap(threadedOcc(), "POVthreadNum", "POVseqNum", newColName(get_EVENT_CF()), 15  )
  # })

  output$Preview_Thread_Output_1 <- renderText({ paste(numThreads(threadedOcc(), "POVthreadNum"),"threads in the selected data.")})


  output$Preview_Network_Tab_Controls_0 <- renderUI({
    radioButtons("Timesplit", "Time Measure:", choices = c('POVseqNum','timeGap'), selected="POVseqNum", inline=TRUE)
  })

  output$rawOccurrenceNetwork <- renderPlotly({
    req(input$Timesplit)
    eventNetwork(threadedOcc(), "POVthreadNum", newColName(get_EVENT_CF()), input$Timesplit)
  })

  output$Thread_Tab_Output_1  = DT::renderDataTable({ threadedOcc()  })

  ########  Comparison  tab  ############### -- not used for now
  # output$POV_Tab_Controls_1 <- renderUI({
  #   checkboxGroupInput("COMPARISON_CF_ID","Select columns for comparison:",
  #                      cfnames(selectOccFilter()),
  #                      selected =  get_COMPARISON_CF(),
  #                      inline=TRUE)
  # })



  ##########################################################################
  ##################### 3.OCC to EVENT  tab ################################
  ##########################################################################

  ########  one to one tab  ###############

  output$One_to_One_controls  = renderUI({
    tags$div(align="left",
             tags$h4("One-to-One: Each occurrence in the raw data is interpreted as an event (INPUT = Occurrences)."),
             tags$p(" "),
             textInput("EventMapName1", label = h4("Enter label for this mapping:"), value = "One-to-One"),
             actionButton("EventButton1", "Create New Mapping"),
             hr()
             )

  })

  # this function runs when you push the button to create a new mapping
  threadedEventCluster <- reactive({
    input$EventButton1
    isolate(OccToEvents1(threadedOcc(),
                         input$EventMapName1,
                         get_EVENT_CF(),
                         get_COMPARISON_CF()
    ) )})

  # Need to suppress some columns that contain lists that do not display correctly in the DT
  threadedEvents <- reactive({make_nice_event_DT(threadedEventCluster()[["threads"]])})

  output$One_to_one_Tab_Output_1  = DT::renderDataTable({
    threadedEvents()
  }, filter = "top")

  output$One_to_one_Tab_Output_2  = renderPlotly({
    threadMap(threadedEvents(), "threadNum", "seqNum", 'ZM_1', 15  ) })

  ########  contextual chunk tab  ###############

    output$Contextual_Chunk_controls = renderUI({
      tags$div(align="left",
               tags$h4("Context-based chunks: Occurrences are grouped into events based on changes in contextual factors (INPUT = Occurrences)."),
               tags$p(paste0("Start new event when ALL of these change:", paste(get_EVENT_CF(),","))),

               textInput("EventMapName2", label = h4("Enter label for this mapping"), value = "Chunks"),

               actionButton("EventButton2", "Create New Mapping"),
               hr() )
    })

  # this function runs when you push the button to create a new mapping
  # this is for the chunks
  threadedEventCluster2 <- reactive({
    input$EventButton2
    isolate(OccToEvents2(threadedOcc(),
                         input$EventMapName2,
                         get_EVENT_CF(),
                         get_COMPARISON_CF()
    ) )})

  # Need to suppress some columns that contain lists that do not display correctly in the DT
  threadedEvents2 <- reactive({make_nice_event_DT(threadedEventCluster2()[["threads"]])})


    output$Contextual_Chunks_Tab_Output_1  = DT::renderDataTable({
      threadedEvents2()
    }, filter = "top")

    output$Contextual_Chunks_Tab_Output_2  = renderPlotly({
      threadMap(threadedEvents2(), "threadNum", "tStamp", 'ZM_1', 15  ) })

    ########  regular expression tab  ###############
    # Thiscode is ALMOST identical to the regex tab.
    # If there is a bug here, it is probably there as well.

      output$Regular_Expression_controls_1 = renderUI({
        tags$div(align="left",
                 selectizeInput("RegExInputMapID",label = h4("Choose input for this mapping:"), get_event_mapping_names( GlobalEventMappings )  ))
      })

      # get the data that will be the input for this tab
    regexInputEvents <- reactive( get_event_mapping_threads( GlobalEventMappings , input$RegExInputMapID) )



    output$Regular_Expression_controls_2 <- renderUI({
      zoom_limit = zoom_upper_limit(regexInputEvents())
      if (zoom_limit == 1)
      {tags$h4("Zooming not available with this mapping")}
      else
      {sliderInput("regexZoomID",
                   label=h4("Zoom in and out by event similarity:"),
                   1,zoom_limit,1, step = 1, ticks=FALSE) }
    })

    output$Regular_Expression_controls_3 <- renderUI({ maxrows=length(unique(regexInputEvents()[['threadNum']]))
                                                        sliderInput("regexVerbatimRows",
                                                          label=h4("How many threads to view:"),
                                                          min=1,max=maxrows, c(1,min(maxrows,10)), step = 1, ticks=FALSE)
    })

     output$Regular_Expression_controls_4 =   renderText(
       paste(thread_text_vector(regexInputEvents(),'threadNum',get_Zoom_REGEX())[input$regexVerbatimRows[1]:input$regexVerbatimRows[2] ], '\n' )  )

    # or if you prefer HTML
    # output$Regular_Expression_controls_4 =   renderUI(HTML(c("<h4>Threads in text form</h4><br>",
    #                                         paste(thread_text_vector(regexInputEvents(),'threadNum',get_Zoom_REGEX())[input$regexVerbatimRows[1]:input$regexVerbatimRows[2] ], '<br>' )) ))
     output$Regular_Expression_controls_5 <- renderUI({ maxrows=length(unique(regexInputEvents()[['threadNum']]))
     sliderInput("numRegexInputRows",
                 label=h4("How many ngrams/labels to make:"),
                 min=1,max=10, 3, step = 1, ticks=FALSE) })

    # create several rows of inputs
     output$Regular_Expression_controls_6 = renderUI({

       # create some select inputs
       lapply(1:input$numRegexInputRows, function(i) {
         fluidRow(
         column(2,textInput(paste0('regex', i), paste0('Ngram-', i) ), offset=1),
         column(2,textInput(paste0('regexLabel', i), paste0('Label-', i) ))
         # ,
         # column(2,selectizeInput(paste0('regex', i), label=paste0('Label-', i), freqNgramSelections() ))
         ) })
       })

    # need to add commas and probably add slider for upper/lower bound and threshold
    freqNgramSelections <- reactive({ selectize_frequent_ngrams(regexInputEvents() , 'threadNum', get_Zoom_REGEX(), 2, 5, 3)  })

     # get the input values and return data frame with regex & label
    regexInput = reactive({
      data.frame(pattern=unlist(lapply(1:input$numRegexInputRows,
                                   function(i){input[[paste0('regex', i)]]}
                                   )),
                    label=unlist(lapply(1:input$numRegexInputRows,
                                 function(i){input[[paste0('regexLabel', i)]]}
                    )), stringsAsFactors=FALSE )
      })

      output$Regular_Expression_controls_7 = renderUI({
        tags$div(align="left",
                 textInput("EventMapName3", label = h4("Enter label for result"), value = "RegEx_"),
                 radioButtons("KeepIrregularEvents",label=h4("Keep irregular events:"), choices=c('Keep', 'Drop'), inline=TRUE),
                 actionButton("EventButton3", "Create New Mapping")  )

      })

      # this function runs when you push the button to create a new mapping
      threadedEventsRegEx <- observeEvent(
        input$EventButton3,
        {isolate(OccToEvents3(regexInputEvents(),
                             input$EventMapName3,
                             get_EVENT_CF(),
                             get_COMPARISON_CF(),
                             'threadNum',
                             get_Zoom_REGEX(),
                             regexInput(),
                             input$KeepIrregularEvents))
        })


      ########  Frequent n-gram tab  ###############
      # Thiscode is ALMOST identical to the regex tab.
      # If there is a bug here, it is probably there as well.

      output$Frequent_Ngram_controls_1 = renderUI({
        tags$div(align="left",
                 selectizeInput("freqNgramInputMapID",label = h4("Choose input for this mapping:"), get_event_mapping_names( GlobalEventMappings )  ))
      })

      # get the data that will be the input for this tab
      freqNgramInputEvents <- reactive( get_event_mapping_threads( GlobalEventMappings , input$freqNgramInputMapID) )


      output$Frequent_Ngram_controls_2 <- renderUI({
        zoom_limit = zoom_upper_limit(freqNgramInputEvents())
        if (zoom_limit == 1)
        {tags$h4("Zooming not available with this mapping")}
        else
        {sliderInput("freqNgramZoomID",
                     label=h4("Zoom in and out by event similarity:"),
                     1,zoom_limit,1, step = 1, ticks=FALSE) }
      })
      output$Frequent_Ngram_controls_21 <- renderUI({
        tags$div(align="left",
        sliderInput("freqNgramRange",
                    label=h4("Size of nGrams between:"),
                    min=2,max=10, c(2,5), step = 1, ticks=FALSE),
        sliderInput("freqNgramThreshold",
                    label=h4("Frequency Threshold:"),
                    min=2,max=20, 2, step = 1, ticks=FALSE)
        )
      })

      output$Frequent_Ngram_controls_3 <- renderUI({ maxrows=length(unique(freqNgramInputEvents()[['threadNum']]))
      sliderInput("freqNgramVerbatimRows",
                  label=h4("How many threads to view:"),
                  min=1,max=maxrows, c(1,min(maxrows,10)), step = 1, ticks=FALSE)
      })

      output$Frequent_Ngram_controls_4 =   renderText(
        paste(thread_text_vector(freqNgramInputEvents(),'threadNum',get_Zoom_freqNgram())[input$freqNgramVerbatimRows[1]:input$freqNgramVerbatimRows[2] ], '\n' )  )

      # or if you prefer HTML
      # output$Regular_Expression_controls_4 =   renderUI(HTML(c("<h4>Threads in text form</h4><br>",
      #                                         paste(thread_text_vector(regexInputEvents(),'threadNum',get_Zoom_REGEX())[input$regexVerbatimRows[1]:input$regexVerbatimRows[2] ], '<br>' )) ))

    output$Frequent_Ngram_controls_5 <- renderUI({ maxrows=length(unique(freqNgramInputEvents()[['threadNum']]))
      sliderInput("numfreqNgramInputRows",
                  label=h4("How many ngrams/labels to make:"),
                  min=1,max=10, 3, step = 1, ticks=FALSE) })

      # create several rows of inputs
      output$Frequent_Ngram_controls_6 = renderUI({

        # create some select inputs
        lapply(1:input$numfreqNgramInputRows, function(i) {
          fluidRow(
            column(2, selectizeInput(paste0('freqNgram', i), label=paste0('Choose ngram-', i), freqNgramSelections() ), offset=1),
            column(2,textInput(paste0('freqNgramLabel', i), label=paste0('Label-', i) ))
          ) })
      })

      # need to add slider for upper/lower bound and threshold
      freqNgramSelections <- reactive({ selectize_frequent_ngrams(freqNgramInputEvents() , 'threadNum',
                                                                  get_Zoom_freqNgram(),
                                                                  input$freqNgramRange[1],
                                                                  input$freqNgramRange[2],
                                                                  input$freqNgramThreshold )  })

      # get the input values and return data frame with regex & label
      freqNgramInput = reactive({
        data.frame(pattern=unlist(lapply(1:input$numfreqNgramInputRows,
                                         function(i){input[[paste0('freqNgram', i)]]}
        )),
        label=unlist(lapply(1:input$numfreqNgramInputRows,
                            function(i){input[[paste0('freqNgramLabel', i)]]}
        )), stringsAsFactors=FALSE )
      })


      output$Frequent_Ngram_controls_7 = renderUI({
        tags$div(align="left",
                 textInput("EventMapName4", label = h4("Enter label for result"), value = "freqNgram_"),
                 radioButtons("KeepIrregularEvents_2",label=h4("Keep irregular events:"), choices=c('Keep', 'Drop'), inline=TRUE),
                 actionButton("EventButton4", "Create New Mapping")  )

      })

      # this function runs when you push the button to create a new mapping
      threadedEventsfreqNgram <- observeEvent(
        input$EventButton4,
        {isolate(OccToEvents3(freqNgramInputEvents(),
                              input$EventMapName4,
                              get_EVENT_CF(),
                              get_COMPARISON_CF(),
                              'threadNum',
                              get_Zoom_freqNgram(),
                              freqNgramInput(),
                              input$KeepIrregularEvents_2))
        })



      ########  maximal pattern tab  ###############

          # output$Maximal_Pattern_controls = renderUI({
          #   tags$div(align="left",
          #            tags$h4("Maximal patterns: Form events based on maximal patterns-- Not implemented yet"),
          #
          #            selectizeInput("MaximalPatternInputID",label = h4("Choose input for this mapping:"), get_event_mapping_names( GlobalEventMappings ) ),
          #
          #            textInput("EventMapName5", label = h4("Enter label for this mapping"), value = "Maximal_"),
          #
          #            actionButton("EventButton5", "Create New Mapping")  )
          #
          # })

      ########  clustering tab  ###############

          output$Cluster_Event_controls_1 = renderUI({
            tags$div(align="left",
                     tags$h4("Cluster Events: Group similar events to together to allow zooming"),

                     selectizeInput("ClusterEventsInputID",label = h4("Choose mapping for clustering:"), get_event_mapping_names( GlobalEventMappings ) ))
          })
            output$Cluster_Event_controls_2 = renderUI({
              tags$div(align="left",
                     textInput("EventMapName6", label = h4("Enter new label for this mapping + clustering"), value =""),

                     radioButtons("ClusterMethodID", "Cluster based on:",
                                  choices = c("Sequential similarity", "Contextual Similarity", "Network Structure"),
                                  selected="Sequential similarity", inline=TRUE),

                     actionButton("EventButton6", "Cluster Events")  )
          })


          output$dendroClusterResult <- renderDendroNetwork({
             input$EventButton6
            isolate( dendroNetwork(clusterEvents( get_event_mapping_threads( GlobalEventMappings,
                                                                    input$ClusterEventsInputID),
                                         input$ClusterEventsInputID,
                                         input$EventMapName6,
                                         input$ClusterMethodID,
                                         get_EVENT_CF()),
                          treeOrientation = "vertical", textColour = "black"))
          })

          ######## manage event mappings tab  ###############

            output$Manage_Event_Map_controls= renderUI({
              tags$div(align="left",
                       tags$h4("Select event mapping to export or delete"),

                       selectizeInput("ManageEventMapInputID",label = h4("Choose mapping:"), get_event_mapping_names( GlobalEventMappings ) ),

                       actionButton("ExportMappingButton", "Export"),
                       actionButton("DeleteMappingButton", "Delete") )

            })

            # reactive functions for the export and delete buttons
            observeEvent(
              input$DeleteMappingButton,
              {delete_event_mapping( GlobalEventMappings, input$ManageEventMapInputID )
                output$delete_confirm = renderText(paste(input$ManageEventMapInputID, " deleted."))
              })

            observeEvent(
              input$ExportMappingButton,
              {export_event_mapping( GlobalEventMappings, input$ManageEventMapInputID )
                output$delete_confirm = renderText(paste(input$ManageEventMapInputID, " exported."))
            })


  ########################################################################
  ##################### 4.VISUALIZE tab ################################
  ########################################################################

  # Controls for the whole set of tabs
  output$Visualize_Tab_Controls_1 = renderUI({
        selectizeInput("VisualizeEventMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names( GlobalEventMappings ), selected='One-to-One' )
  })

  output$Visualize_Tab_Controls_2 = renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$VisualizeEventMapInputID))
    if ( zoom_limit == 1)
       {tags$h4("Zooming not available for this mapping")}
    else
        {sliderInput("VisualizeTabZoomID",
               "Zoom in and out by event similarity:",
               1, zoom_limit, 1, step = 1, ticks=FALSE) }
  })

  ######## create subsets tab  ###############

  output$SelectSubsetControls <- renderUI({
    tags$div(align="left",
    textInput("SelectSubsetMapName", label = h4(paste("Enter label for this subset of the", input$VisualizeEventMapInputID," mapping")),
                                                value = ""),
    actionButton("SelectSubsetButton", "Save Subset") )
  })

  output$SelectSubsetDataTable  = DT::renderDataTable({
    threadedEventsViz() }, filter = "top")

  observeEvent(
    input$SelectSubsetButton,
    {store_event_mapping( input$SelectSubsetMapName, threadedEventsViz()[input$SelectSubsetDataTable_rows_all,] )
    })

 # just for reference selectOccFilter = reactive(selectOcc()[input$Data_Tab_Output_2_rows_all,])

  ######## Repetitive Sub-sequences tab  ###############

  # controls for sub-sequence display
  output$nGramControls <- renderUI({
    tagList(
      sliderInput("nGramLengthID","nGram Size", 1,10,2,step=1,ticks=FALSE ),
      sliderInput("nGramDisplayThresholdID","Display threshold", 1,50,1,step=1,ticks=FALSE )
    )
  })

  # Get data for the Visualize tab.  Need parallel functions for the other tabs.
  threadedEventsViz <- reactive({ print(paste0('reactive inputID', input$VisualizeEventMapInputID))
    get_event_mapping_threads( GlobalEventMappings, input$VisualizeEventMapInputID ) })


  #  NGRAM  display #
  output$nGramBarchart = renderPlotly({
    ng_bar_chart(threadedEventsViz(), "threadNum", get_Zoom_VIZ(), input$nGramLengthID, input$nGramDisplayThresholdID)
  })

  ######## Whole sequence tab  ###############

# Whole sequence display -- allow alternatives

  output$WholeSequenceThreadMap_Sequence <- renderPlotly({
    threadMap(threadedEventsViz(), "threadNum", "seqNum", get_Zoom_VIZ(), 15  )
  })

  output$WholeSequenceThreadMap_ActualTime <- renderPlotly({
    threadMap(threadedEventsViz(), "threadNum", "tStamp", get_Zoom_VIZ(), 15  )
  })


  ######## Circular network tab  ###############
  # output$circularLayoutNetwork = renderPlotly(
  #
  # )


  ######## Force network tab  ###############

  # use this to select how to color the nodes in force layout
  output$Network_Tab_Controls_2 <- renderUI({
    button_choices = intersect(colnames(threadedEventsViz()), cfnames(selectOccFilter()))
    tags$div(
    radioButtons("NetworkGroupID","Select a dimension for coloring nodes:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE))
  })

  output$forceNetworkD3 <- renderForceNetwork({
    forceNetworkD3(threadedEventsViz(), "threadNum", input$NetworkGroupID, get_Zoom_VIZ())
  })

  ######## Custom network tab  ###############

  output$VisualizeCustomNetwork <- renderPlotly({
    eventNetwork(threadedEventsViz(), "threadNum", 1, get_Zoom_VIZ())
  })


  ######################################################################
  ##################### 5. COMPARE  tab ################################
  ######################################################################


  # Make two parallel sets of input and data. Different mapping on each side

  # ####### SUBSET A   ##########
  output$Comparison_Tab_Controls_A1 <- renderUI({
    selectizeInput("CompareMapInputID_A",label = h4("Choose mapping:"),  get_event_mapping_names( GlobalEventMappings ) )
    })

  output$Comparison_Tab_Controls_A2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$CompareMapInputID_A))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("CompareZoomID_A",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,1, step = 1, ticks=FALSE) }
    })

  # Get data for the COMPARE tab mapping A
  threadedEventsComp_A <- reactive({
    get_event_mapping_threads( GlobalEventMappings, input$CompareMapInputID_A ) })

  # just one type of plot for now -- need to select different plot types
  output$Comparison_Plots_A <- renderPlotly({
    threadMap(threadedEventsComp_A(), "threadNum", "seqNum", get_Zoom_COMP_A(), 15  )
  })

   # ####### SUBSET B   ##########
  output$Comparison_Tab_Controls_B1 <- renderUI({
    selectizeInput("CompareMapInputID_B",label = h4("Choose mapping:"),  get_event_mapping_names( GlobalEventMappings ) )
  })

  output$Comparison_Tab_Controls_B2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$CompareMapInputID_B))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("CompareZoomID_B",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,1, step = 1, ticks=FALSE) }
  })

  # Get data for the COMPARE tab mapping B.
  threadedEventsComp_B <- reactive({
    get_event_mapping_threads( GlobalEventMappings, input$CompareMapInputID_B ) })

  # just one type of plot for now -- need to select different plot types
  output$Comparison_Plots_B <- renderPlotly({
    threadMap(threadedEventsComp_B(), "threadNum", "seqNum", get_Zoom_COMP_B(), 15  )
  })

  # ##########  DIACHRONIC Comparison sub-tab   ###########
  output$Diachronic_Comparison_Tab_Controls_1 <- renderUI({
    selectizeInput("DiaCompareMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names( GlobalEventMappings ) )
  })

  output$Diachronic_Comparison_Tab_Controls_2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$DiaCompareMapInputID))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("DiaCompareZoomID",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,1, step = 1, ticks=FALSE) }
  })

   output$Diachronic_Comparison_Tab_Controls_3 <- renderUI({
     radioButtons("DiaCompareTimeSubsetID", "How many time intervals to compare:", choices = c(1, 2, 3, 4, 5, 6), selected="1", inline=TRUE)
   })


   # Get data for the Diachronic COMPARE tab  .
   threadedEventsDiaComp <- reactive({
     get_event_mapping_threads( GlobalEventMappings, input$DiaCompareMapInputID ) })


   # controls for the comparison input panels
   # Use all of the column names here...
   output$Diachronic_Comparison_Tab_Controls_4 <- renderUI({
     selectizeInput("selectComparisonID","Compare by:", colnames(threadedEventsDiaComp()))
   })
   CF_levels = reactive( get_CF_levels( threadedEventsDiaComp(),input$selectComparisonID) )


   output$Diachronic_Comparison_Tab_Controls_5 <- renderUI({
     tagList(
       selectizeInput("selectComparisonGroupsID","Compare specific groups:",
                      CF_levels(), multiple=TRUE),

       sliderInput("nGramLengthCompID","nGram Size", 1,10,2,step=1,ticks=FALSE )

     )
   })

   # Get subsets of events and create sub-plots for them

   output$DiachronicComparisonPlots <- renderPlotly(
     Comparison_Plots(threadedEventsDiaComp(),
                      input$selectComparisonID,
                      input$selectComparisonGroupsID,
                      input$DiaCompareTimeSubsetID,
                      input$nGramLengthCompID,
                      get_Zoom_DIA_COMP()) )


  ############################################################################
  ######################## 6. MOVING WINDOW TAB ##############################
  ############################################################################

  output$Moving_Window_Tab_Controls_1 <- renderUI({
    selectizeInput("MovingWindowMapInputID",label = h4("Choose mapping:"), get_event_mapping_names( GlobalEventMappings ) )
  })

  output$Moving_Window_Tab_Controls_2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads( GlobalEventMappings , input$MovingWindowMapInputID))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("MovingWindowZoomID",
                 label = h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,1, step = 1, ticks=FALSE) }
  })

  output$Moving_Window_Tab_Controls_3 <- renderUI({
    sliderInput("MovingWindowSizeID","Window Size", 1, numThreads(threadedEvents(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_4_A <- renderUI({
    sliderInput("WindowLocation_A_ID","Window Location", 1,numThreads(threadedEvents(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_4_B <- renderUI({
    sliderInput("WindowLocation_B_ID","Window Location", 1,numThreads(threadedEvents(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  # "Timesplit" appears to be used for the custom network plotly layout
  # output$Moving_Tab_Controls_5 <- renderUI({
  #   tags$div(align="center",
  #   radioButtons("Timesplit3", "Time Measure:", choices = c('seqNum','timeGap'), selected="seqNum", inline=TRUE))
  # })

  # Get data for the Moving Window tab.
  threadedEventsMove <- reactive({
    get_event_mapping_threads( GlobalEventMappings, input$MovingWindowMapInputID ) })


   output$MovingWindow_Plot_A <- renderPlotly({
     w = get_moving_window(threadedEventsMove(), input$MovingWindowSizeID, input$WindowLocation_A_ID )
     threadMap(w, "threadNum", "seqNum", get_Zoom_MOVE(), 15  )
   })

   output$MovingWindow_Plot_B <- renderPlotly({
     w = get_moving_window(threadedEventsMove(), input$MovingWindowSizeID, input$WindowLocation_B_ID )
     threadMap(w, "threadNum", "seqNum", get_Zoom_MOVE(), 15  )
   })

  #
  # output$MovingWindow_Plot_A <- renderPlotly({
  #   w = get_moving_window(threadedEvents(),-input$MovingWindowSizeID, input$WindowLocationID )
  #   eventNetwork(w, "threadNum", get_Zoom_MOV(), input$Timesplit3) })
  #
  #
  # output$MovingWindow_Plot_B <- renderPlotly({
  #   w = get_moving_window(threadedEvents(),input$MovingWindowSizeID, input$WindowLocationID )
  #   eventNetwork(w, "threadNum", get_Zoom_MOV(), input$Timesplit3) })
  #

  ######
  output$Pos_Layout_Controls_0 <- renderUI({
    radioButtons("Timesplit2", "Time Measure:", choices = c('seqNum'='seqNum.1','timeGap'='timeGap'), selected="seqNum.1", inline=TRUE)
  })





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
    #newColName(get_EVENT_CF()) input$EVENT_CF_ID
    ENsubset = subset(TE, actor == event.data()$key)
    #ENsubset = subset(TE,  as.numeric(gsub("\\D", "", actor)) == event.data()$pointNumber)
    ENsubset
  })



  output$eventNetworksubset_data <- renderDataTable({
    test<-eventNetworksubset()
    test
    #event.data()
  })






  output$test <- renderDataTable({
    w = get_moving_window(threadedEvents(),input$MovingWindowSizeID, input$WindowLocationID )
    w
  })



  ################################################################################
  ############################  Admin, params, etc  ##############################
  ################################################################################
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



})
