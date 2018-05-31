##########################################################################################################
# THREADNET:SHINY SERVER

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

server <- shinyServer(function(input, output, session) {

  observe({
    hide(selector = "#navbar li a[data-value=choosePOV]")
    hide(selector = "#navbar li a[data-value=visualize]")
    hide(selector = "#navbar li a[data-value=subsets]")
    hide(selector = "#navbar li a[data-value=comparisons]")
    hide(selector = "#navbar li a[data-value=movingWindow]")
    hide(selector = "#navbar li a[data-value=parameterSettings]")
  })


	options(warn=-1)
	options(shiny.maxRequestSize=30*1024^2)

	# create reactive value to force execution of function that gets map names for menus
	rv <-reactiveValues(newmap=0)

	# get_CF returns the choice of contextual factors from the Data tab.
	get_CF <<- reactive({ return( input$CFcolumnsID ) })

	# get_POV returns the choice of POV from the POV tab
	get_THREAD_CF <<- reactive({ return(input$THREAD_CF_ID) })
	get_EVENT_CF <<- reactive({ return(input$EVENT_CF_ID) })
	get_COMPARISON_CF <<- reactive({ return(setdiff(get_CF(), union(get_THREAD_CF(),get_EVENT_CF() ))) })

	# time scale for use throughout the app
	get_timeScale <<- reactive({ return(input$timeScaleID) })

	# These sliders controls the zoom level for zooming in-out
	# they are grouoped here because hopefully they can be replaced by a single function... except that reactive functions don't take parameters
	get_Zoom_VIZ <<- reactive({ return( ifelse (zoom_upper_limit(get_POV(input$VisualizeEventMapInputID))==1 ,
												"ZM_1", paste0("ZM_",input$VisualizeTabZoomID))) })

	get_Zoom_COMP_A <<- reactive({
	  req(input$CompareMapInputID_A, input$CompareZoomID_A)
	  return( ifelse (zoom_upper_limit(get_POV(input$CompareMapInputID_A))==1 ,
													"ZM_1", paste0("ZM_",input$CompareZoomID_A)))
	  })

	get_Zoom_COMP_B <<- reactive({
	  req(input$CompareMapInputID_B, input$CompareZoomID_B)
	  return( ifelse (zoom_upper_limit(get_POV(input$CompareMapInputID_B))==1 ,
													"ZM_1", paste0("ZM_",input$CompareZoomID_B)))
	  })

	get_Zoom_DIA_COMP <<- reactive({
	  req(input$DiaCompareMapInputID, input$DiaCompareZoomID)
	  return( ifelse (zoom_upper_limit(get_POV(input$DiaCompareMapInputID))==1 ,
													"ZM_1", paste0("ZM_",input$DiaCompareZoomID)))
	  })

	get_Zoom_MOVE <<- reactive({
	  req(input$MovingWindowMapInputID, input$MovingWindowZoomID)
	  return( ifelse (zoom_upper_limit(get_POV(input$MovingWindowMapInputID))==1 ,
												"ZM_1", paste0("ZM_",input$MovingWindowZoomID)))
	  })

	get_Zoom_REGEX <<- reactive({
	  req(input$RegExInputMapID, input$regexZoomID)
	  return( ifelse (zoom_upper_limit(get_POV(input$RegExInputMapID))==1 ,
													"ZM_1", paste0("ZM_",input$regexZoomID)))
	  })

	get_Zoom_freqNgram <<- reactive({
	  req(input$freqNgramInputMapID, input$freqNgramZoomID)
	  return( ifelse (zoom_upper_limit(get_POV(input$freqNgramInputMapID))==1 ,
														"ZM_1", paste0("ZM_",input$freqNgramZoomID)))
	  })

	get_Zoom_CHUNK <<- reactive({
	  req(input$ChunkInputMapID, input$chunkZoomID)
	  return( ifelse (zoom_upper_limit(get_POV(input$ChunkInputMapID))==1 ,
											"ZM_1", paste0("ZM_",input$chunkZoomID))) })

	# add reactive value to force update
	get_POV_names <- reactive({
		rv$newmap
		get_POV_name_list()
	})

	#dataframe for occurrences that are read in from file1
	occ <- eventReactive(input$file1,parseInputData(input$file1))

	# selected columns from the raw data
	selectOcc <- reactive(occ()[c("tStamp", input$CFcolumnsID)] )

	# select rows using the nice DT input
	selectOccFilter <- reactive(selectOcc()[input$Data_Tab_Output_2_rows_all,])

	# The POV tabs reconstruct the data into threads by sorting by tStamp and
	# adding columns for threadNum and seqNum for the selected POV in ThreadOccByPOV
	threadedOcc <- reactive({
	  validate(need(input$THREAD_CF_ID != "", "You must select at least one context factor to define Threads"))
	  validate(need(input$EVENT_CF_ID != "", "You must select at least one context factor to define Events within Threads"))
	  ThreadOccByPOV( selectOccFilter(), input$THREAD_CF_ID, input$EVENT_CF_ID )
	  })

	# Here we need a function to do the subset on the POV tab
	threadedOccSubSet <- reactive(threadedOcc()[input$povDataThreads_rows_all,])

	observeEvent(input$addPOVButton,
	  if (check_POV_name(input$POVMapName)){
	    POVName = input$POVMapName
	    output$EventValidate1 = renderText(paste('POV Name', POVName , 'already exists, please select a different name'))
	  } else {
	  rv$newmap <- rv$newmap+1 # trigger reactive value
	  isolate(
	    store_POV(input$POVMapName, threadedOccSubSet(),input$THREAD_CF_ID,input$EVENT_CF_ID ) # this is the name; need to get object to add
	  )
	  output$EventValidate1 = renderText(paste('New POV named', input$POVMapName ,'has been created'))
	}, ignoreInit = TRUE)



	# get the data that will be the input for this tab
	chunkInputEvents <- reactive({
		rv$newmap
	  req(input$ChunkInputMapID)
		get_POV(input$ChunkInputMapID)
	})

	# this function runs when you push the button to create a new mapping based on chunks
    observeEvent( input$EventButton2,
    if (check_POV_name(input$EventMapName2)){
      mapName2 = input$EventMapName2
      output$EventValidate2 = renderText(paste('POV Name', mapName2 , 'already exists, please select a different name'))
    } else {
        rv$newmap <- rv$newmap+1 # trigger reactive value
        isolate(
            OccToEvents_By_Chunk(
                chunkInputEvents(),
                input$Chunks_method_Button, # which method?
                input$EventMapName2,
                input$fixed_chunk_size,
                input$chunk_time_gap_threshold,
                'mins', #get_timeScale()
                input$chunk_CFs,
                get_POV_THREAD_CF(input$ChunkInputMapID),
                get_POV_EVENT_CF(input$ChunkInputMapID),
                get_POV_COMPARISON_CF(input$ChunkInputMapID, get_CF())
            )
        )
        output$EventValidate2 = renderText(paste('New POV named', input$EventMapName2 ,'has been created'))
    }, ignoreInit = TRUE )

	# get the data that will be the input for this tab
	regexInputEvents <- reactive({
	  req(input$RegExInputMapID)
	  get_POV(input$RegExInputMapID)
	})

	# get the input values and return data frame with regex & label
	regexInput <- reactive({
	  req(input$numRegexInputRows)
		data.frame(
			pattern <- unlist(lapply(1:input$numRegexInputRows,function(i){input[[paste0('regex', i)]]})),
			label   <- unlist(lapply(1:input$numRegexInputRows,function(i){input[[paste0('regexLabel', i)]]})),
			stringsAsFactors = FALSE
		)
	})

	# this function runs when you push the button to create a new mapping
    observeEvent(input$EventButton3,
      if (check_POV_name(input$EventMapName3)){
        mapName3 = input$EventMapName3
      output$EventValidate3 = renderText(paste('POV Name', mapName3 , 'already exists, please select a different name'))
    } else {
        rv$newmap <- rv$newmap+1 # trigger reactive value
        isolate(
            OccToEvents3(
                regexInputEvents(),
                input$EventMapName3,
                get_POV_THREAD_CF(input$RegExInputMapID),
                get_POV_EVENT_CF(input$RegExInputMapID),
                get_POV_COMPARISON_CF(input$RegExInputMapID, get_CF()),
                'threadNum',
                get_Zoom_REGEX(),
                regexInput(),
                input$KeepIrregularEvents
            )
        )
      output$EventValidate3 = renderText(paste('New POV named', input$EventMapName3 ,'has been created'))
    }, ignoreInit = TRUE )

	# get the data that will be the input for this tab
	freqNgramInputEvents <- reactive({
	  req(input$freqNgramInputMapID)
	  get_POV( input$freqNgramInputMapID)
	  })

	fng_select <- reactive(
		support_level(
			thread_text_vector(
				freqNgramInputEvents(),
				'threadNum',
				get_Zoom_freqNgram(),
				' '
			),
			frequent_ngrams(
				freqNgramInputEvents() ,
				'threadNum',
				get_Zoom_freqNgram(),
				input$freqNgramRange[1],
				input$freqNgramRange[2],
				TRUE
			)
		)
	)

	# The bottom example shows a server-side table. Make sure you have included row names in the table (as the first column of the table).
	# In the case of server-side processing, the row names of the selected rows are available in input$x3_rows_selected as a character vector.

	selected_ngrams <- reactive({
	  req(input$freqnGramTable_rows_selected)
		s <- as.integer(input$freqnGramTable_rows_selected)
		data.frame(
			pattern <- unlist(lapply(1:length(s),function(i){ str_replace_all(fng_select()[i,'ngrams'],' ',',') })),
			label   <- unlist(lapply(1:length(s),function(i){paste0("<",str_replace_all(fng_select()[i,'ngrams'],' ','_'),">")})),
			stringsAsFactors=FALSE
		)
	})

	# this function runs when you push the button to create a new mapping
    observeEvent(input$EventButton4,
      if (check_POV_name(input$EventMapName4)){
        mapName4 = input$EventMapName4
        output$EventValidate4 = renderText(paste('POV name', mapName4 , 'already exists, please select a different name'))
      } else {
        rv$newmap <- rv$newmap+1 # trigger reactive value
        isolate(
            OccToEvents3(
                freqNgramInputEvents(),
                input$EventMapName4,
                get_POV_THREAD_CF(input$freqNgramInputMapID),
                get_POV_EVENT_CF(input$freqNgramInputMapID),
                get_POV_COMPARISON_CF(input$freqNgramInputMapID, get_CF()),
                'threadNum',
                get_Zoom_freqNgram(),
                selected_ngrams(),
                input$KeepIrregularEvents_2
            )
        )
        output$EventValidate4 = renderText(paste('New POV named', input$EventMapName4 ,'has been created'))
    }, ignoreInit = TRUE)

	# separate the cluster calculation from the dendrogram display
    cluster_result <- eventReactive(input$EventButton6,{
        validate(need(!(check_POV_name(input$EventMapName6)), paste0('Map Name ',input$EventMapName6,' already exists. Please select a different name.')))
        rv$newmap <- rv$newmap+1 # trigger reactive value

            # return a list with both the cluster_result and the new POV
            # store the POV and return the cluster_result for display
            thread_CF = get_POV_THREAD_CF(input$ClusterEventsInputID)
            event_CF = get_POV_EVENT_CF(input$ClusterEventsInputID)
            cluster_POV = clusterEvents(
                      get_POV(input$ClusterEventsInputID),
                      input$EventMapName6,
                      input$ClusterMethodID,
                      thread_CF,
                      event_CF,
                      'cluster')
            store_POV(input$EventMapName6,
                      cluster_POV[['POV']],
                      thread_CF,
                      event_CF )
            cluster_POV[['cluster_result']]

    } )

	# Get data for the Visualize tab.Need parallel functions for the other tabs.
	subsetEventsViz <- reactive({
	  req(input$SelectSubsetMapInputID)
	  get_POV( input$SelectSubsetMapInputID )
	    })

	# reactive functions for the export and delete buttons
	observeEvent(input$DeleteMappingButton,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		delete_POV(input$ManageEventMapInputID)
		output$delete_confirm <- renderText(paste(input$ManageEventMapInputID, " deleted."))
	}, ignoreInit = TRUE)

	observeEvent(input$ExportMappingRData,{
		export_POV(input$ManageEventMapInputID )
		output$action_confirm <- renderText(paste(input$ManageEventMapInputID, " exported as .RData file"))
	})

	observeEvent(input$ExportMappingCsv,{
		export_POV_csv( input$ManageEventMapInputID )
		output$action_confirm <- renderText(paste(input$ManageEventMapInputID, " exported as .csv file"))
	})

	# Another opportunity to make subsets...
	observeEvent(input$SelectSubsetButton,
                 if (check_POV_name(input$SelectSubsetMapName)){
                   SubsetMapName = input$SelectSubsetMapName
                   output$SelectSubsetValidate = renderText(paste('Map Name', SubsetMapName , 'already exists, please select a different name'))
                 } else {
                   rv$newmap <- rv$newmap+1 # trigger reactive value
                   store_POV( input$SelectSubsetMapName,
                              subsetEventsViz()[input$SelectSubsetDataTable_rows_all,],
                              get_POV_THREAD_CF(input$SelectSubsetMapInputID),
                              get_POV_EVENT_CF(input$SelectSubsetMapInputID))
                   output$SelectSubsetValidate = renderText(paste('New POV named', input$SelectSubsetMapName ,'has been created'))
                 }, ignoreInit = TRUE)


	# Get data for the Visualize tab.Need parallel functions for the other tabs.
#	threadedEventsViz <- reactive({get_POV( input$VisualizeEventMapInputID ) })


	# Get data for the Visualize tab.  Need parallel functions for the other tabs.
	threadedEventsViz_ALL <- reactive({
	  req(input$VisualizeEventMapInputID)
	  get_POV( input$VisualizeEventMapInputID )
	  })

	threadedEventsViz <- reactive({
	  req(input$VisualizeRangeID[1], input$VisualizeRangeID[2])
	  loc = input$VisualizeRangeID[1]
	  width=input$VisualizeRangeID[2] - input$VisualizeRangeID[1]+1
	  get_moving_window(threadedEventsViz_ALL(),width,loc) })



	# Get data for the COMPARE tab mapping A
	threadedEventsComp_A <- reactive({
	  req(input$CompareMapInputID_A)
	  get_POV(input$CompareMapInputID_A )
	  })

	# Get data for the COMPARE tab mapping B.
	threadedEventsComp_B <- reactive({
	  req(input$CompareMapInputID_B)
	  get_POV( input$CompareMapInputID_B )
	  })

	# Get data for the Diachronic COMPARE tab.
	threadedEventsDiaComp <- reactive({
	  req(input$DiaCompareMapInputID)
	  get_POV(input$DiaCompareMapInputID )
	  })

	CF_levels <- reactive( get_CF_levels( threadedEventsDiaComp(),input$selectComparisonID) )

	# Get data for the Moving Window tab.
	threadedEventsMove <- reactive({
	  req(input$MovingWindowMapInputID)
	  get_POV(input$MovingWindowMapInputID )
	  })

	threadedEventsMove_A <- reactive({
	  req(input$MovingWindowSizeID, input$WindowLocation_A_ID)
		get_moving_window(
			threadedEventsMove() ,
			input$MovingWindowSizeID,
			input$WindowLocation_A_ID
		)
	})

	threadedEventsMove_B <- reactive({
	  req(input$MovingWindowSizeID, input$WindowLocation_B_ID)
		get_moving_window(
			threadedEventsMove(),
			input$MovingWindowSizeID,
			input$WindowLocation_B_ID
		)
	})

	# Source tab-specific Server output functions
	source(file.path("server", "readData.R"),          local = TRUE)$value
	source(file.path("server", "choosePOV.R"),         local = TRUE)$value
	source(file.path("server", "comparisons.R"),       local = TRUE)$value
	source(file.path("server", "visualize.R"),         local = TRUE)$value
	source(file.path("server", "subsets.R"),           local = TRUE)$value
	source(file.path("server", "movingWindow.R"),      local = TRUE)$value
	source(file.path("server", "parameterSettings.R"), local = TRUE)$value
})
