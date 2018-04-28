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
  #  observe( makeReactiveBinding("GlobalEventMappings", env=.GlobalEnv) )
  #

  # create reactive value to force execution of function that gets map names for menus
  rv <-reactiveValues(newmap=0)

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
  get_Zoom_VIZ <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$VisualizeEventMapInputID))==1 ,
                                              "ZM_1", paste0("ZM_",input$VisualizeTabZoomID))) })

  get_Zoom_COMP_A <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$CompareMapInputID_A))==1 ,
                                               "ZM_1", paste0("ZM_",input$CompareZoomID_A))) })

  get_Zoom_COMP_B <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$CompareMapInputID_B))==1 ,
                                               "ZM_1", paste0("ZM_",input$CompareZoomID_B))) })

  get_Zoom_DIA_COMP <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$DiaCompareMapInputID))==1 ,
                                                 "ZM_1", paste0("ZM_",input$DiaCompareZoomID))) })

  get_Zoom_MOVE <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$MovingWindowMapInputID))==1 ,
                                              "ZM_1", paste0("ZM_",input$MovingWindowZoomID))) })

  get_Zoom_REGEX <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$RegExInputMapID))==1 ,
                                               "ZM_1", paste0("ZM_",input$regexZoomID))) })

  get_Zoom_freqNgram <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$freqNgramInputMapID))==1 ,
                                                    "ZM_1", paste0("ZM_",input$freqNgramZoomID))) })

  get_Zoom_CHUNK <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(  input$ChunkInputMapID))==1 ,
                                                    "ZM_1", paste0("ZM_",input$chunkZoomID))) })

  # add reactive value to force update
  get_event_mapping_names <- reactive({rv$newmap
                                      get_event_mapping_name_list() })

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
                      selectOcc() }, filter = "top", options=list(autoWidth = TRUE))

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

   threadedOcc = reactive({ ThreadOccByPOV( selectOccFilter(), input$THREAD_CF_ID, input$EVENT_CF_ID ) })


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

  output$previewThreadMap_1 <- renderPlotly({
    threadMap(threadedOcc(), "threadNum", "tStamp", newColName(get_EVENT_CF()), 16  )
  })

  # output$previewThreadMap_2 <- renderPlotly({
  #   threadMap(threadedOcc(), "threadNum", "seqNum", newColName(get_EVENT_CF()), 15  )
  # })

  output$Preview_Thread_Output_1 <- renderText({ paste(numThreads(threadedOcc(), "threadNum"),"threads in the selected data.")})


  # output$Preview_Network_Tab_Controls_0 <- renderUI({
  #   radioButtons("Timesplit", "Time Measure:", choices = c('POVseqNum','timeGap'), selected="POVseqNum", inline=TRUE)
  # })
  #
  # output$rawOccurrenceNetwork <- renderPlotly({
  #   req(input$Timesplit)
  #   eventNetwork(threadedOcc(), "POVthreadNum", newColName(get_EVENT_CF()), input$Timesplit)
  # })

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

  ########  contextual chunk tab  ###############

  output$chunk_controls_0 = renderUI({
    tags$div(align="left",
             selectizeInput("ChunkInputMapID",label = h4("Start with this view:"), get_event_mapping_names()  ))
  })

  # get the data that will be the input for this tab
  chunkInputEvents <- reactive({rv$newmap
                                get_event_mapping_threads( input$ChunkInputMapID) })

  output$chunk_controls_1 <- renderUI({
    zoom_limit = zoom_upper_limit( chunkInputEvents())
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("chunkZoomID",
                 label=h4("Zoom in and out by event similarity:"),
                 1, zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })

  # handoff controls
  output$chunk_controls_2 = renderUI({
    # input$chunk_CFs
    tags$div(checkboxGroupInput("chunk_CFs","Start new event when ALL of these change:",
                                intersect(colnames(chunkInputEvents()), union(get_EVENT_CF(),get_COMPARISON_CF())),
                                inline=TRUE))
    })

  # time gap controls
  output$chunk_controls_3 = renderUI({
    # input$chunk_time_gap_threshold,
    # input$chunk_timescale,

    sliderInput("chunk_time_gap_threshold",
                "Minimum time gap between chunks (mins):",
                threshold_slider_min(chunkInputEvents()),
                threshold_slider_max(chunkInputEvents()),
                threshold_slider_selected(chunkInputEvents()),
                step = 1, ticks=FALSE)

    })

  # fixed size controls
  output$chunk_controls_4 = renderUI({
    # input$fixed_chunk_size,
    tags$div(sliderInput("fixed_chunk_size", "Select fixed size for chunks:", 1,20,10, ticks=FALSE))
    })

  # mapping name and go button
  output$chunk_controls_5 = renderUI({

    tags$div(align="left",
             textInput("EventMapName2", label = h4("Enter label to save result") ),
             actionButton("EventButton2", "Create New Mapping")  )

    })

  output$chunk_controls_6 <- renderUI({ maxrows=length(unique(chunkInputEvents()[['threadNum']]))
  sliderInput("chunkVerbatimRows",
              label=h4("How many threads to view:"),
              min=1,max=maxrows, c(1,min(maxrows,10)), step = 1, ticks=FALSE)
  })

  # show the results
  output$chunk_controls_7 =   renderText({ paste(thread_text_vector(chunkInputEvents(),'threadNum',
                                                get_Zoom_CHUNK(), ',')[input$chunkVerbatimRows[1]:input$chunkVerbatimRows[2] ], '\n' )  })

  # output$chunk_controls_8 = renderPlotly({
  #   threadMap(chunkInputEvents()[input$chunkVerbatimRows[1]:input$chunkVerbatimRows[2], ], "threadNum", "seqNum", get_Zoom_CHUNK(), 15  )
  # })
  #
  output$chunk_controls_8 = renderPlotly({
    threadMap(chunkInputEvents(), "threadNum", "seqNum", get_Zoom_CHUNK(), 15  )
  })

  # this function runs when you push the button to create a new mapping based on chunks
  observeEvent(
    input$EventButton2,
    {rv$newmap = rv$newmap+1 # trigger reactive value
    isolate( OccToEvents_By_Chunk(chunkInputEvents(),
                                 input$Chunks_method_Button, # which method?
                                 input$EventMapName2,
                                 input$fixed_chunk_size,
                                 input$chunk_time_gap_threshold,
                                 'mins',
                                 input$chunk_CFs,
                                 get_EVENT_CF(),
                                 get_COMPARISON_CF() ) )}, ignoreInit = TRUE)


  # Need to suppress some columns that contain lists that do not display correctly in the DT
  # threadedEvents2 <- reactive({make_nice_event_DT(threadedChunks()[["threads"]])})
  #
  #   output$Contextual_Chunks_Tab_Output_1  = DT::renderDataTable( threadedEvents2() )
  #
  #   output$Contextual_Chunks_Tab_Output_2  = renderPlotly({
  #     threadMap(threadedEvents2(), "threadNum", "tStamp", 'ZM_1', 15  ) })



    ########  regular expression tab  ###############


      output$Regular_Expression_controls_1 = renderUI({
        tags$div(align="left",
                 selectizeInput("RegExInputMapID",label = h4("Start with this view:"), get_event_mapping_names()  ))
      })

      # get the data that will be the input for this tab
    regexInputEvents <- reactive( get_event_mapping_threads( input$RegExInputMapID) )


    output$Regular_Expression_controls_2 <- renderUI({
      zoom_limit = zoom_upper_limit(regexInputEvents())
      if (zoom_limit == 1)
      {tags$h4("Zooming not available with this view")}
      else
      {sliderInput("regexZoomID",
                   label=h4("Zoom in and out by event similarity:"),
                   1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
    })

    output$Regular_Expression_controls_3 <- renderUI({ maxrows=length(unique(regexInputEvents()[['threadNum']]))
                                                        sliderInput("regexVerbatimRows",
                                                          label=h4("How many threads to view:"),
                                                          min=1,max=maxrows, c(1,min(maxrows,10)), step = 1, ticks=FALSE)
    })

     output$Regular_Expression_controls_4 =   renderText(
       paste(thread_text_vector(regexInputEvents(),'threadNum',get_Zoom_REGEX(),',')[input$regexVerbatimRows[1]:input$regexVerbatimRows[2] ], '\n' )  )

    # or if you prefer HTML
    # output$Regular_Expression_controls_4 =   renderUI(HTML(c("<h4>Threads in text form</h4><br>",
    #                                         paste(thread_text_vector(regexInputEvents(),'threadNum',get_Zoom_REGEX(),',')[input$regexVerbatimRows[1]:input$regexVerbatimRows[2] ], '<br>' )) ))
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

         ) })
       })

    # need to add commas and probably add slider for upper/lower bound and threshold
    # freqNgramSelections <- reactive({ selectize_frequent_ngrams(regexInputEvents() , 'threadNum', get_Zoom_REGEX(), 2, 5, 3)  })

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
                 textInput("EventMapName3", label = h4("Enter label to save result"), value = ""),
                 radioButtons("KeepIrregularEvents",label=h4("Keep irregular events:"), choices=c('Keep', 'Drop'), inline=TRUE),
                 actionButton("EventButton3", "Create New Mapping")  )

      })

      # this function runs when you push the button to create a new mapping
      observeEvent(
        input$EventButton3,
       { rv$newmap = rv$newmap+1 # trigger reactive value
        isolate(OccToEvents3(regexInputEvents(),
                             input$EventMapName3,
                             get_EVENT_CF(),
                             get_COMPARISON_CF(),
                             'threadNum',
                             get_Zoom_REGEX(),
                             regexInput(),
                             input$KeepIrregularEvents))}, ignoreInit = TRUE)


      ########  Frequent n-gram tab  ###############
      # This code is similar to the regex tab except for the data table

      output$Frequent_Ngram_controls_1 = renderUI({
        tags$div(align="left",
                 selectizeInput("freqNgramInputMapID",label = h4("Start with this view:"), get_event_mapping_names()  ))
      })

      # get the data that will be the input for this tab
      freqNgramInputEvents <- reactive( get_event_mapping_threads( input$freqNgramInputMapID) )


      output$Frequent_Ngram_controls_2 <- renderUI({
        zoom_limit = zoom_upper_limit(freqNgramInputEvents())
        if (zoom_limit == 1)
        {tags$h4("Zooming not available with this view")}
        else
        {sliderInput("freqNgramZoomID",
                     label=h4("Zoom in and out by event similarity:"),
                     1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
      })
      output$Frequent_Ngram_controls_21 <- renderUI({
        tags$div(align="left",
        sliderInput("freqNgramRange",
                    label=h4("Size of nGrams between:"),
                    min=2,max=10, c(2,5), step = 1, ticks=FALSE)
        )
      })

      output$Frequent_Ngram_controls_3 <- renderUI({ maxrows=length(unique(freqNgramInputEvents()[['threadNum']]))
      sliderInput("freqNgramVerbatimRows",
                  label=h4("How many threads to view:"),
                  min=1,max=maxrows, c(1,min(maxrows,10)), step = 1, ticks=FALSE)
      })

      output$Frequent_Ngram_controls_4 =   renderText(
        paste(thread_text_vector(freqNgramInputEvents(),'threadNum',get_Zoom_freqNgram(),', ')[input$freqNgramVerbatimRows[1]:input$freqNgramVerbatimRows[2] ], '\n' )  )

      # or if you prefer HTML
      # output$Regular_Expression_controls_4 =   renderUI(HTML(c("<h4>Threads in text form</h4><br>",
      #                                         paste(thread_text_vector(regexInputEvents(),'threadNum',get_Zoom_REGEX(),',')[input$regexVerbatimRows[1]:input$regexVerbatimRows[2] ], '<br>' )) ))

    output$Frequent_Ngram_controls_5 <- renderUI({ maxrows=length(unique(freqNgramInputEvents()[['threadNum']]))
      sliderInput("numfreqNgramInputRows",
                  label=h4("How many ngrams/labels to make:"),
                  min=1,max=10, 3, step = 1, ticks=FALSE) })

    fng_select <-reactive(support_level(thread_text_vector(freqNgramInputEvents(),
                                                    'threadNum',
                                                    get_Zoom_freqNgram(),' ' ),
                                                frequent_ngrams(freqNgramInputEvents() ,
                                                 'threadNum',
                                                 get_Zoom_freqNgram(),
                                                 input$freqNgramRange[1],
                                                 input$freqNgramRange[2],
                                                 TRUE)))


    output$freqnGramTable <- DT::renderDataTable( fng_select() , filter = "top")


    # The bottom example shows a server-side table. Make sure you have included row names in the table (as the first column of the table).
    # In the case of server-side processing, the row names of the selected rows are available in input$x3_rows_selected as a character vector.
    #
    selected_ngrams <-reactive({s=as.integer(input$freqnGramTable_rows_selected)
                              data.frame(pattern=unlist(lapply(1:length(s),
                                                                  function(i){ str_replace_all(fng_select()[i,'ngrams'],' ',',') })),
                                            label=unlist(lapply(1:length(s),
                                                                  function(i){paste0("<",
                                                                    str_replace_all(fng_select()[i,'ngrams'],' ','_'),
                                                                    ">")
                                                                  })),
                                            stringsAsFactors=FALSE)
      })


      output$Frequent_Ngram_controls_7 = renderUI({
        tags$div(align="left",
                 textInput("EventMapName4", label = h4("Enter label to save result"), value ="" ),
                 radioButtons("KeepIrregularEvents_2",label=h4("Keep irregular events:"), choices=c('Keep', 'Drop'), inline=TRUE),
                 actionButton("EventButton4", "Create New Mapping")  )

      })

      # this function runs when you push the button to create a new mapping
      observeEvent(
        input$EventButton4,
        {rv$newmap = rv$newmap+1 # trigger reactive value
        isolate( OccToEvents3(freqNgramInputEvents(),
                              input$EventMapName4,
                              get_EVENT_CF(),
                              get_COMPARISON_CF(),
                              'threadNum',
                              get_Zoom_freqNgram(),
                              selected_ngrams(),
                              input$KeepIrregularEvents_2) )}, ignoreInit = TRUE)



      ########  maximal pattern tab  ###############

          # output$Maximal_Pattern_controls = renderUI({
          #   tags$div(align="left",
          #            tags$h4("Maximal patterns: Form events based on maximal patterns-- Not implemented yet"),
          #
          #            selectizeInput("MaximalPatternInputID",label = h4("Choose input for this mapping:"), get_event_mapping_names(  ) ),
          #
          #            textInput("EventMapName5", label = h4("Enter label for this mapping"), value = "Maximal_"),
          #
          #            actionButton("EventButton5", "Create New Mapping")  )
          #
          # })

      ########  clustering tab  ###############

          output$Cluster_Event_controls_1 = renderUI({
            tags$div(align="left",
                     tags$h4("Group similar events to together to allow zooming"),
                     selectizeInput("ClusterEventsInputID",label = h4("Start with this view:"), get_event_mapping_names() ))
          })

            output$Cluster_Event_controls_2 = renderUI({
              tags$div(align="left",
                     radioButtons("ClusterMethodID", label = h4("Cluster based on:"),
                                  choices = c("Network Proximity","Contextual Similarity", "Sequential similarity"  ),
                                  selected="Network Proximity") )
          })
            output$Cluster_Event_controls_3 = renderUI({
              tags$div(align="left",
                       textInput("EventMapName6", label = h4("Enter label to save this view + new clustering"), value =""),
                       actionButton("EventButton6", "Cluster Events")  )
            })

            # separate the cluster calculation from the dendrogram display
            cluster_result <- eventReactive(
              input$EventButton6,
              {rv$newmap = rv$newmap+1 # trigger reactive value
              isolate( clusterEvents( get_event_mapping_threads( input$ClusterEventsInputID),
                                                         input$EventMapName6,
                                                         input$ClusterMethodID,
                                                         get_EVENT_CF(),
                                                        'cluster')
                                      )}, ignoreInit = TRUE )

          output$dendroClusterResult <- renderDendroNetwork({ dendroNetwork(cluster_result(),
                                                                           treeOrientation = "horizontal", textColour = "black" ) })

          ######## create subsets tab  ###############

          # Controls for the whole set of tabs
          output$SelectSubsetControls_1 = renderUI({
            selectizeInput("SelectSubsetMapInputID",label = h4("Choose input mapping:"),  get_event_mapping_names()  )
          })

          output$SelectSubsetControls_2 <- renderUI({
            tags$div(align="left",
                     textInput("SelectSubsetMapName", label = h4(paste("Enter label for this subset of the", input$SelectSubsetMapInputID," mapping")),
                               value = ""),
                     actionButton("SelectSubsetButton", "Save Subset") )
          })

          # Get data for the Visualize tab.  Need parallel functions for the other tabs.
          subsetEventsViz <- reactive({  get_event_mapping_threads( input$SelectSubsetMapInputID ) })

          output$SelectSubsetDataTable  = DT::renderDataTable({ subsetEventsViz() }, filter = "top")

          observeEvent(
            input$SelectSubsetButton,
            {rv$newmap = rv$newmap+1 # trigger reactive value
            store_event_mapping( input$SelectSubsetMapName, subsetEventsViz()[input$SelectSubsetDataTable_rows_all,] )
            }, ignoreInit = TRUE)


          ######## manage event mappings tab  ###############

            output$Manage_Event_Map_controls= renderUI({
              tags$div(align="left",
                       tags$h4("Select event mapping to export or delete"),

                       selectizeInput("ManageEventMapInputID",label = h4("Choose mapping:"), get_event_mapping_names() ),

                       actionButton("ExportMappingRData", "Export RData file"),
                       actionButton("ExportMappingCsv", "Export to csv"),
                       actionButton("DeleteMappingButton", "Delete") )

            })

            # reactive functions for the export and delete buttons
            observeEvent(
              input$DeleteMappingButton,
              {rv$newmap = rv$newmap+1 # trigger reactive value
              delete_event_mapping( input$ManageEventMapInputID )
                output$delete_confirm = renderText(paste(input$ManageEventMapInputID, " deleted."))
              }, ignoreInit = TRUE)

            observeEvent(
              input$ExportMappingRData,
              {export_event_mapping(  input$ManageEventMapInputID )
                output$action_confirm = renderText(paste(input$ManageEventMapInputID, " exported as .RData file"))
              })

            observeEvent(
              input$ExportMappingCsv,
              {export_event_mapping_csv( input$ManageEventMapInputID )
                output$action_confirm = renderText(paste(input$ManageEventMapInputID, " exported as .csv file"))
              })


  ########################################################################
  ##################### 4.VISUALIZE tab ################################
  ########################################################################

  # Controls for the whole set of tabs
  output$Visualize_Tab_Controls_1 = renderUI({
        selectizeInput("VisualizeEventMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names(), selected='OneToOne' )
  })

  output$Visualize_Tab_Controls_2 = renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$VisualizeEventMapInputID))
    if ( zoom_limit == 1)
       {tags$h4("Zooming not available for this mapping")}
    else
        {sliderInput("VisualizeTabZoomID",
               "Zoom in and out by event similarity:",
               1, zoom_limit, zoom_limit, step = 1, ticks=FALSE) }
  })

  # Get data for the Visualize tab.  Need parallel functions for the other tabs.
  threadedEventsViz <- reactive({  get_event_mapping_threads( input$VisualizeEventMapInputID ) })


  ######## Basic ngrams tab  ###############

  # controls for ngrams display
  output$nGramControls <- renderUI({
    tagList(
      sliderInput("nGramLengthID","nGram Size", 1,10,2,step=1,ticks=FALSE ),
      sliderInput("nGramDisplayThresholdID","Display threshold", 1,50,1,step=1,ticks=FALSE )
    )
  })

  #  NGRAM  display #
  output$nGramBarchart = renderPlotly({
    ng_bar_chart(threadedEventsViz(), "threadNum", get_Zoom_VIZ(), input$nGramLengthID, input$nGramDisplayThresholdID)
  })

  ######## Repetitive Sub-sequences tab  ###############

  # controls for sub-sequence display
  # output$freqnGramControls <- renderUI({
  #   tagList(
  #     sliderInput("freqnGramLengthID","nGram Size range", 1,10, c(2,5),step=1,ticks=FALSE ),
  #     sliderInput("freqnGramDisplayThresholdID","Display threshold", 1,50,1,step=1,ticks=FALSE )
  #   )
  # })


  #  frequent NGRAM  displays #
  # just compute this data once -- not sure of the best way to display it... Table?  Bar chart?

  # fng <-reactive(support_level(thread_text_vector(threadedEventsViz(),
  #                                                 'threadNum',
  #                                                 get_Zoom_VIZ(),' ' ),
  #                             frequent_ngrams(threadedEventsViz() ,
  #                                 'threadNum',
  #                                get_Zoom_VIZ(),
  #                                input$freqnGramLengthID[1],
  #                                input$freqnGramLengthID[2],
  #                                TRUE)))

  # output$freqnGramTable <- DT::renderDataTable( fng() , filter = "top")

  # output$freqnGramBarchart = renderPlotly({ ng_bar_chart_freq(fng() )})


  ######## Whole sequence tab  ###############

# Whole sequence display -- allow alternatives

  output$WholeSequenceThreadMap_Sequence <- renderPlotly({ threadMap(threadedEventsViz(), "threadNum", "seqNum", get_Zoom_VIZ(), 15  )  })

  output$WholeSequenceThreadMap_ActualTime <- renderPlotly({ threadMap(threadedEventsViz(), "threadNum", "tStamp", get_Zoom_VIZ(), 15 )  })

  output$WholeSequenceThreadMap_RelativeTime <- renderPlotly({ threadMap(threadedEventsViz(), "threadNum", "relativeTime", get_Zoom_VIZ(), 15 ) })

  ######## Circular network tab  ###############
  # use this to select how to color the nodes in force layout
  output$Circle_Network_Tab_Controls <- renderUI({ sliderInput("circleEdgeTheshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})

  output$circleVisNetwork <- renderVisNetwork({
    # first convert the threads to the network
    n = threads_to_network_original( threadedEventsViz(), "threadNum", get_Zoom_VIZ() )
    n=filter_network_edges(n,input$circleEdgeTheshold)
    circleVisNetwork( n, TRUE ) })

  ######## Other network tab  ###############
  # use this to select how to color the nodes in force layout
  output$Other_Network_Tab_Controls <- renderUI({
    button_choices = get_EVENT_CF()
    tags$div(
      radioButtons("OtherNetworkCF","Graph co-occurrence relation between:",
                   choices = button_choices,
                   selected =  button_choices[1], # always start with the first one
                   inline=TRUE),
    sliderInput("otherEdgeTheshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))
    })

  output$otherVisNetwork <- renderVisNetwork({
    # first convert the threads to the network
    n = normalNetwork( threadedEventsViz(), selectOccFilter(), input$OtherNetworkCF )
    n=filter_network_edges(n,input$otherEdgeTheshold)
    circleVisNetwork( n )
  })


  ######## Force network tab  ###############

  # use this to select how to color the nodes in force layout
  output$Force_Network_Tab_Controls <- renderUI({
    button_choices = intersect(colnames(threadedEventsViz()), cfnames(selectOccFilter()))
    tags$div(
      sliderInput("forceEdgeTheshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ),
    radioButtons("NetworkGroupID","Select a dimension for coloring nodes:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE))
  })

  output$forceNetworkD3 <- renderForceNetwork({
    n = threads_to_network_original( threadedEventsViz(), 'threadNum', get_Zoom_VIZ(), input$NetworkGroupID )
    n = filter_network_edges(n,input$forceEdgeTheshold)
    forceNetworkD3( n )
  })

  ######## Custom network tab  ###############

  output$VisualizeCustomNetwork_Controls_0 <- renderUI({
    radioButtons("Timesplit2", "Time Measure:", choices = c('seqNum'='seqNum','timeGap'='timeGap'), selected="seqNum", inline=TRUE)
  })

  output$VisualizeCustomNetwork_Controls_1 <- renderUI({
    selectizeInput("Event",label = "Choose Event:",  get_EVENT_CF() )
  })

  output$VisualizeCustomNetwork <- renderPlotly({
    req(input$Timesplit2, input$Event)
    eventNetwork(threadedEventsViz(), "threadNum", input$Event, input$Timesplit2)
  })



  output$hover <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  })

  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here" else d
  })

  # output$VisualizeCustomNetwork <- renderPlotly({
  #   req(input$Timesplit2)
  #   eventNetwork(threadedEventsViz(), "threadNum", get_Zoom_VIZ(), input$Timesplit2)
  # })



  ######################################################################
  ##################### 5. COMPARE  tab ################################
  ######################################################################


  # Make two parallel sets of input and data. Different mapping on each side

  # ####### SUBSET A   ##########
  output$Comparison_Tab_Controls_A1 <- renderUI({
    selectizeInput("CompareMapInputID_A",label = h4("Choose mapping A:"),  get_event_mapping_names() )
    })

  output$Comparison_Tab_Controls_A2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$CompareMapInputID_A))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("CompareZoomID_A",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
    })

  # Get data for the COMPARE tab mapping A
  threadedEventsComp_A <- reactive({
    get_event_mapping_threads(  input$CompareMapInputID_A ) })

  # just one type of plot for now -- need to select different plot types
  output$Comparison_Plots_A <- renderPlotly({
    threadMap(threadedEventsComp_A(), "threadNum", "seqNum", get_Zoom_COMP_A(), 15  )
  })

   # ####### SUBSET B   ##########
  output$Comparison_Tab_Controls_B1 <- renderUI({
    selectizeInput("CompareMapInputID_B",label = h4("Choose mapping B:"),  get_event_mapping_names() )
  })

  output$Comparison_Tab_Controls_B2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$CompareMapInputID_B))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("CompareZoomID_B",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })

  # Get data for the COMPARE tab mapping B.
  threadedEventsComp_B <- reactive({
    get_event_mapping_threads( input$CompareMapInputID_B ) })

  # just one type of plot for now -- need to select different plot types
  output$Comparison_Plots_B <- renderPlotly({
    threadMap(threadedEventsComp_B(), "threadNum", "seqNum", get_Zoom_COMP_B(), 15  )
  })

  #### Put all eight here #####
  output$Comp_A_1 <- renderPlotly({ threadMap(threadedEventsComp_A(), "threadNum", "seqNum", get_Zoom_COMP_A(), 15  ) })
  output$Comp_B_1 <- renderPlotly({ threadMap(threadedEventsComp_B(), "threadNum", "seqNum", get_Zoom_COMP_B(), 15  ) })

  output$Comp_A_2 <- renderPlotly({ threadMap(threadedEventsComp_A(), "threadNum", "tStamp", get_Zoom_COMP_A(), 15 ) })
  output$Comp_B_2 <- renderPlotly({ threadMap(threadedEventsComp_B(), "threadNum", "tStamp", get_Zoom_COMP_B(), 15 ) })

  output$Comp_A_3 <- renderPlotly({ threadMap(threadedEventsComp_A(), "threadNum", "relativeTime", get_Zoom_COMP_A(), 15 ) })
  output$Comp_B_3 <- renderPlotly({ threadMap(threadedEventsComp_B(), "threadNum", "relativeTime", get_Zoom_COMP_B(), 15 ) })

  output$Comp_A_4_controls <- renderUI({sliderInput("A_4_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_B_4_controls <- renderUI({sliderInput("B_4_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_A_4 <- renderVisNetwork({
    n = threads_to_network_original( threadedEventsComp_A(), "threadNum", get_Zoom_COMP_A() )
    n=filter_network_edges(n,input$A_4_Theshold)
    circleVisNetwork( n ) })

  output$Comp_B_4 <- renderVisNetwork({
    n = threads_to_network_original( threadedEventsComp_B(), "threadNum", get_Zoom_COMP_B() )
    n=filter_network_edges(n,input$B_4_Theshold)
    circleVisNetwork( n  ) })

  output$Comp_A_5_controls <- renderUI({sliderInput("A_5_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_B_5_controls <- renderUI({sliderInput("B_5_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})
  output$Comp_A_5 <- renderForceNetwork({
      n = threads_to_network_original( threadedEventsComp_A(), 'threadNum', get_Zoom_COMP_A(), 'threadNum' )
      n = filter_network_edges(n,input$A_5_Theshold)
      forceNetworkD3( n )  })

  output$Comp_B_5 <- renderForceNetwork({
    n = threads_to_network_original( threadedEventsComp_B(), 'threadNum', get_Zoom_COMP_B(), 'threadNum' )
    n = filter_network_edges(n,input$B_5_Theshold)
    forceNetworkD3( n )  })

  output$Comp_A_6_controls <- renderUI({button_choices = get_EVENT_CF()
  tags$div(
    radioButtons("A_6_OtherNetworkCF","Graph co-occurrence relation between:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE),
    sliderInput("A_6_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))})

  output$Comp_B_6_controls <- renderUI({button_choices = get_EVENT_CF()
  tags$div(
    radioButtons("B_6_OtherNetworkCF","Graph co-occurrence relation between:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE),
    sliderInput("B_6_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))})

  output$Comp_A_6 <- renderVisNetwork({
    n = normalNetwork( threadedEventsComp_A(), selectOccFilter(), input$A_6_OtherNetworkCF )
    n=filter_network_edges(n,input$A_6_Theshold)
    circleVisNetwork( n )  })

  output$Comp_B_6 <- renderVisNetwork({
    n = normalNetwork( threadedEventsComp_B(), selectOccFilter(), input$B_6_OtherNetworkCF )
    n=filter_network_edges(n,input$B_6_Theshold)
    circleVisNetwork( n ) })

  output$Comp_A_7_controls <- renderUI({checkboxGroupInput("A_7_CFs","Pick Two:", get_EVENT_CF() )})
  output$Comp_B_7_controls <- renderUI({checkboxGroupInput("B_7_CFs","Pick Two:", get_EVENT_CF() )})
  output$Comp_A_7 <- renderPlotly({ role_map( threadedEventsComp_A(), selectOccFilter(), input$A_7_CFs ) })
  output$Comp_B_7 <- renderPlotly({ role_map( threadedEventsComp_A(), selectOccFilter(), input$B_7_CFs ) })

  output$Comp_A_8 <- renderPlotly({ threadTrajectory(threadedEventsComp_A() ) })
  output$Comp_B_8 <- renderPlotly({ threadTrajectory(threadedEventsComp_B() ) })


  # ##########  DIACHRONIC Comparison sub-tab   ###########
  output$Diachronic_Comparison_Tab_Controls_1 <- renderUI({
    selectizeInput("DiaCompareMapInputID",label = h4("Choose mapping:"),  get_event_mapping_names() )
  })

  output$Diachronic_Comparison_Tab_Controls_2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$DiaCompareMapInputID))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("DiaCompareZoomID",
                 label=h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })

   output$Diachronic_Comparison_Tab_Controls_3 <- renderUI({
     radioButtons("DiaCompareTimeSubsetID", "How many time intervals to compare:", choices = c(1, 2, 3, 4, 5, 6), selected="1", inline=TRUE)
   })


   # Get data for the Diachronic COMPARE tab  .
   threadedEventsDiaComp <- reactive({
     get_event_mapping_threads(  input$DiaCompareMapInputID ) })


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
    selectizeInput("MovingWindowMapInputID",label = h4("Choose mapping:"), get_event_mapping_names() )
  })

  output$Moving_Window_Tab_Controls_2 <- renderUI({
    zoom_limit = zoom_upper_limit(get_event_mapping_threads(  input$MovingWindowMapInputID))
    if (zoom_limit == 1)
    {tags$h4("Zooming not available with this mapping")}
    else
    {sliderInput("MovingWindowZoomID",
                 label = h4("Zoom in and out by event similarity:"),
                 1,zoom_limit,zoom_limit, step = 1, ticks=FALSE) }
  })

  output$Moving_Window_Tab_Controls_3 <- renderUI({
    sliderInput("MovingWindowSizeID","Window Size", 1, numThreads(threadedEventsMove(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_4_A <- renderUI({
    sliderInput("WindowLocation_A_ID","Window Location", 1,numThreads(threadedEventsMove(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  output$Moving_Window_Tab_Controls_4_B <- renderUI({
    sliderInput("WindowLocation_B_ID","Window Location", 1,numThreads(threadedEventsMove(),"threadNum" ),1,step=1,ticks=FALSE )
  })

  # Get data for the Moving Window tab.

  threadedEventsMove <- reactive({get_event_mapping_threads(  input$MovingWindowMapInputID )})

  threadedEventsMove_A <- reactive({
    get_moving_window(threadedEventsMove() ,
                      input$MovingWindowSizeID,
                      input$WindowLocation_A_ID ) })

  threadedEventsMove_B <- reactive({
    get_moving_window(threadedEventsMove(),
                      input$MovingWindowSizeID,
                      input$WindowLocation_B_ID ) })

  ## Conditional controls for both side A and B
  output$Moving_4_controls <- renderUI({sliderInput("M_4_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})

  output$Moving_5_controls <- renderUI({sliderInput("M_5_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE )})

  output$Moving_6_controls <- renderUI({button_choices = get_EVENT_CF()
  tags$div(
    radioButtons("M_6_OtherNetworkCF","Graph co-occurrence relation between:",
                 choices = button_choices,
                 selected =  button_choices[1], # always start with the first one
                 inline=TRUE),
    sliderInput("M_6_Theshold","Display edges above", 0,1,0,step=0.01,ticks=FALSE ))})


  output$Moving_7_controls <- renderUI({checkboxGroupInput("M_7_CFs","Pick Two:", get_EVENT_CF() )})


  ##  New moving window outputs
  #### Put all eight here #####
  output$Moving_A_1 <- renderPlotly({ threadMap(threadedEventsMove_A(), "threadNum", "seqNum", get_Zoom_MOVE(), 15  ) })
  output$Moving_B_1 <- renderPlotly({ threadMap(threadedEventsMove_B(), "threadNum", "seqNum", get_Zoom_MOVE(), 15  ) })

  output$Moving_A_2 <- renderPlotly({ threadMap(threadedEventsMove_A(), "threadNum", "tStamp", get_Zoom_MOVE(), 15 ) })
  output$Moving_B_2 <- renderPlotly({ threadMap(threadedEventsMove_B(), "threadNum", "tStamp", get_Zoom_MOVE(), 15 ) })

  output$Moving_A_3 <- renderPlotly({ threadMap(threadedEventsMove_A(), "threadNum", "relativeTime", get_Zoom_MOVE(), 15 ) })
  output$Moving_B_3 <- renderPlotly({ threadMap(threadedEventsMove_B(), "threadNum", "relativeTime", get_Zoom_MOVE(), 15 ) })

   output$Moving_A_4 <- renderVisNetwork({
    n = threads_to_network_original( threadedEventsMove_A(), "threadNum", get_Zoom_MOVE() )
    n=filter_network_edges(n,input$M_4_Theshold)
    circleVisNetwork( n ) })

  output$Moving_B_4 <- renderVisNetwork({
    n = threads_to_network_original( threadedEventsMove_B(), "threadNum", get_Zoom_MOVE() )
    n=filter_network_edges(n,input$M_4_Theshold)
    circleVisNetwork( n  ) })

  output$Moving_A_5 <- renderForceNetwork({
    n = threads_to_network_original( threadedEventsMove_A(), 'threadNum', get_Zoom_MOVE(), 'threadNum' )
    n = filter_network_edges(n,input$M_5_Theshold)
    forceNetworkD3( n )  })

  output$Moving_B_5 <- renderForceNetwork({
    n = threads_to_network_original( threadedEventsMove_B(), 'threadNum', get_Zoom_MOVE(), 'threadNum' )
    n = filter_network_edges(n,input$M_5_Theshold)
    forceNetworkD3( n )  })


  output$Moving_A_6 <- renderVisNetwork({
    n = normalNetwork( threadedEventsMove_A(), selectOccFilter(), input$M_6_OtherNetworkCF )
    n=filter_network_edges(n,input$M_6_Theshold)
    circleVisNetwork( n )  })

  output$Moving_B_6 <- renderVisNetwork({
    n = normalNetwork( threadedEventsMove_B(), selectOccFilter(), input$M_6_OtherNetworkCF )
    n=filter_network_edges(n,input$M_6_Theshold)
    circleVisNetwork( n ) })

  output$Moving_A_7 <- renderPlotly({ role_map( threadedEventsMove_A(), selectOccFilter(), input$M_7_CFs ) })
  output$Moving_B_7 <- renderPlotly({ role_map( threadedEventsMove_A(), selectOccFilter(), input$M_7_CFs ) })

  output$Moving_A_8 <- renderPlotly({ threadTrajectory(threadedEventsMove_A() ) })
  output$Moving_B_8 <- renderPlotly({ threadTrajectory(threadedEventsMove_B() ) })





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
