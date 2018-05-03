##########################################################################################################
# THREADNET:SHINY SERVER

# This software may be used according to the terms provided in the
# GNU General Public License (GPL-3.0) https://opensource.org/licenses/GPL-3.0?
# Absolutely no warranty!
##########################################################################################################

server <- shinyServer(function(input, output, session) {

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
	get_Zoom_VIZ <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$VisualizeEventMapInputID))==1 ,
												"ZM_1", paste0("ZM_",input$VisualizeTabZoomID))) })

	get_Zoom_COMP_A <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$CompareMapInputID_A))==1 ,
													"ZM_1", paste0("ZM_",input$CompareZoomID_A))) })

	get_Zoom_COMP_B <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$CompareMapInputID_B))==1 ,
													"ZM_1", paste0("ZM_",input$CompareZoomID_B))) })

	get_Zoom_DIA_COMP <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$DiaCompareMapInputID))==1 ,
													"ZM_1", paste0("ZM_",input$DiaCompareZoomID))) })

	get_Zoom_MOVE <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$MovingWindowMapInputID))==1 ,
												"ZM_1", paste0("ZM_",input$MovingWindowZoomID))) })

	get_Zoom_REGEX <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$RegExInputMapID))==1 ,
													"ZM_1", paste0("ZM_",input$regexZoomID))) })

	get_Zoom_freqNgram <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$freqNgramInputMapID))==1 ,
														"ZM_1", paste0("ZM_",input$freqNgramZoomID))) })

	get_Zoom_CHUNK <<- reactive({ return( ifelse (zoom_upper_limit(get_event_mapping_threads(input$ChunkInputMapID))==1 ,
											"ZM_1", paste0("ZM_",input$chunkZoomID))) })

	# add reactive value to force update
	get_event_mapping_names <- reactive({
		rv$newmap
		get_event_mapping_name_list()
	})

	#dataframe for occurrences that are read in from file1
	occ <- eventReactive(input$file1,read_occurrences(input$file1))

	# selected columns from the raw data
	selectOcc <- reactive(occ()[c("tStamp", input$CFcolumnsID)] )

	# select rows using the nice DT input
	selectOccFilter <- reactive(selectOcc()[input$Data_Tab_Output_2_rows_all,])

	# The POV tabs reconstruct the data into threads by sorting by tStamp and
	# adding columns for threadNum and seqNum for the selected POV in ThreadOccByPOV
	threadedOcc <- reactive({ ThreadOccByPOV( selectOccFilter(), input$THREAD_CF_ID, input$EVENT_CF_ID ) })

	# get the data that will be the input for this tab
	chunkInputEvents <- reactive({
		rv$newmap
		get_event_mapping_threads(input$ChunkInputMapID)
	})

	# this function runs when you push the button to create a new mapping based on chunks
	observeEvent( input$EventButton2,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			OccToEvents_By_Chunk(
				chunkInputEvents(),
				input$Chunks_method_Button, # which method?
				input$EventMapName2,
				input$fixed_chunk_size,
				input$chunk_time_gap_threshold,
				'mins',
				input$chunk_CFs,
				get_EVENT_CF(),
				get_COMPARISON_CF()
			)
		)
	}, ignoreInit = TRUE )

	# get the data that will be the input for this tab
	regexInputEvents <- reactive(get_event_mapping_threads(input$RegExInputMapID))

	# get the input values and return data frame with regex & label
	regexInput <- reactive({
		data.frame(
			pattern <- unlist(lapply(1:input$numRegexInputRows,function(i){input[[paste0('regex', i)]]})),
			label   <- unlist(lapply(1:input$numRegexInputRows,function(i){input[[paste0('regexLabel', i)]]})),
			stringsAsFactors = FALSE
		)
	})

	# this function runs when you push the button to create a new mapping
	observeEvent(input$EventButton3,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			OccToEvents3(
				regexInputEvents(),
				input$EventMapName3,
				get_EVENT_CF(),
				get_COMPARISON_CF(),
				'threadNum',
				get_Zoom_REGEX(),
				regexInput(),
				input$KeepIrregularEvents
			)
		)
	}, ignoreInit = TRUE )

	# get the data that will be the input for this tab
	freqNgramInputEvents <- reactive(get_event_mapping_threads( input$freqNgramInputMapID))

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
		s <- as.integer(input$freqnGramTable_rows_selected)
		data.frame(
			pattern <- unlist(lapply(1:length(s),function(i){ str_replace_all(fng_select()[i,'ngrams'],' ',',') })),
			label   <- unlist(lapply(1:length(s),function(i){paste0("<",str_replace_all(fng_select()[i,'ngrams'],' ','_'),">")})),
			stringsAsFactors=FALSE
		)
	})

	# this function runs when you push the button to create a new mapping
	observeEvent(input$EventButton4,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			OccToEvents3(
				freqNgramInputEvents(),
				input$EventMapName4,
				get_EVENT_CF(),
				get_COMPARISON_CF(),
				'threadNum',
				get_Zoom_freqNgram(),
				selected_ngrams(),
				input$KeepIrregularEvents_2
			)
		)
	}, ignoreInit = TRUE)

	# separate the cluster calculation from the dendrogram display
	cluster_result <- eventReactive(input$EventButton6,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		isolate(
			clusterEvents(
				get_event_mapping_threads(input$ClusterEventsInputID),
				input$EventMapName6,
				input$ClusterMethodID,
				get_EVENT_CF(),
				'cluster'
			)
		)
	}, ignoreInit = TRUE )

	# Get data for the Visualize tab.Need parallel functions for the other tabs.
	subsetEventsViz <- reactive({get_event_mapping_threads( input$SelectSubsetMapInputID ) })

	# reactive functions for the export and delete buttons
	observeEvent(input$DeleteMappingButton,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		delete_event_mapping(input$ManageEventMapInputID)
		output$delete_confirm <- renderText(paste(input$ManageEventMapInputID, " deleted."))
	}, ignoreInit = TRUE)

	observeEvent(input$ExportMappingRData,{
		export_event_mapping(input$ManageEventMapInputID )
		output$action_confirm <- renderText(paste(input$ManageEventMapInputID, " exported as .RData file"))
	})

	observeEvent(input$ExportMappingCsv,{
		export_event_mapping_csv( input$ManageEventMapInputID )
		output$action_confirm <- renderText(paste(input$ManageEventMapInputID, " exported as .csv file"))
	})

	observeEvent(input$SelectSubsetButton,{
		rv$newmap <- rv$newmap+1 # trigger reactive value
		store_event_mapping( input$SelectSubsetMapName, subsetEventsViz()[input$SelectSubsetDataTable_rows_all,] )
	}, ignoreInit = TRUE)

	# Get data for the Visualize tab.Need parallel functions for the other tabs.
#	threadedEventsViz <- reactive({get_event_mapping_threads( input$VisualizeEventMapInputID ) })


	# Get data for the Visualize tab.  Need parallel functions for the other tabs.
	threadedEventsViz_ALL <- reactive({  get_event_mapping_threads( input$VisualizeEventMapInputID ) })

	threadedEventsViz <- reactive({
	  loc = input$VisualizeRangeID[1]
	  width=input$VisualizeRangeID[2] - input$VisualizeRangeID[1]
	  get_moving_window(threadedEventsViz_ALL(),width,loc) })



	# Get data for the COMPARE tab mapping A
	threadedEventsComp_A <- reactive({get_event_mapping_threads(input$CompareMapInputID_A ) })

	# Get data for the COMPARE tab mapping B.
	threadedEventsComp_B <- reactive({get_event_mapping_threads( input$CompareMapInputID_B ) })

	# Get data for the Diachronic COMPARE tab.
	threadedEventsDiaComp <- reactive({get_event_mapping_threads(input$DiaCompareMapInputID ) })

	CF_levels <- reactive( get_CF_levels( threadedEventsDiaComp(),input$selectComparisonID) )

	# Get data for the Moving Window tab.
	threadedEventsMove <- reactive({get_event_mapping_threads(input$MovingWindowMapInputID )})

	threadedEventsMove_A <- reactive({
		get_moving_window(
			threadedEventsMove() ,
			input$MovingWindowSizeID,
			input$WindowLocation_A_ID
		)
	})

	threadedEventsMove_B <- reactive({
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
