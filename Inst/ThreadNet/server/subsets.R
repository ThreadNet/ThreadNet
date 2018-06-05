# Server Output Functions for Subsets and Mapping Tab

#### Contextual Chunks sub-tab ####

output$chunk_controls_0 <- renderUI({
	tags$div(
		align = "left",
		selectizeInput(
			"ChunkInputMapID",
			label = h4("Start with this POV:"),
			get_POV_names()
		)
	)
})

output$chunk_controls_1 <- renderUI({
	zoom_limit <- zoom_upper_limit( chunkInputEvents())
	if (zoom_limit == 1) {
		tags$h4("Zooming not available with this POV")
	} else {
		sliderInput(
			"chunkZoomID",
			label = h4("Zoom in and out by event similarity:"),
			1, zoom_limit,zoom_limit, step = 1, ticks = FALSE
		)
	}
})

# handoff controls
output$chunk_controls_2 <- renderUI({
	tags$div(
		checkboxGroupInput(
			"chunk_CFs",
			"Start new event when ALL of these change:",
			intersect(colnames(chunkInputEvents()), union(get_POV_EVENT_CF(input$ChunkInputMapID),get_COMPARISON_CF()))
		)
	)
})

# time gap controls
output$chunk_controls_3 <- renderUI({
	sliderInput(
		"chunk_time_gap_threshold",
		"Minimum time gap between chunks (mins):",
		threshold_slider_min(chunkInputEvents()),
		threshold_slider_max(chunkInputEvents()),
		threshold_slider_selected(chunkInputEvents()),
		step = 1, ticks = FALSE
	)
})

# fixed size controls
output$chunk_controls_4 <- renderUI({
	# input$fixed_chunk_size,
	tags$div(sliderInput("fixed_chunk_size", "Select fixed size for chunks:", 1,20,10, ticks = FALSE))
})

# mapping name and go button
output$chunk_controls_5 <- renderUI({
	tags$div(
		align = "left",
		textInput("EventMapName2", label = h4("Enter label to save result")),
		actionButton("EventButton2", "Create New POV"),
		span(textOutput("EventValidate2"), style="color:red")
	)
})

output$chunk_controls_6 <- renderUI({
	maxrows <- length(unique(chunkInputEvents()[['threadNum']]))
	sliderInput(
		"chunkVerbatimRows",
		label = h4("How many threads to view:"),
		min = 1,max = maxrows,c(1,min(maxrows,10)),step = 1,ticks = FALSE
	)
})

# show the results
output$chunk_controls_7 <- renderText({
  req(input$chunkVerbatimRows[1])
	paste(
		thread_text_vector(
			chunkInputEvents(),
			'threadNum',
			get_Zoom_CHUNK(),
			','
		)[input$chunkVerbatimRows[1]:input$chunkVerbatimRows[2]],
		'\n'
	)
})

output$chunk_controls_8 <- renderPlotly({
	threadMap(chunkInputEvents(), "threadNum", "seqNum", get_Zoom_CHUNK(), 15)
})

#### Input Your Pattern sub-tab (regular expressions) ####

output$Regular_Expression_controls_1 <- renderUI({
	tags$div(
		align = "left",
		selectizeInput(
			"RegExInputMapID",
			label = h4("Start with this POV:"),
			get_POV_names()
		)
	)
})

output$Regular_Expression_controls_2 <- renderUI({
	zoom_limit <- zoom_upper_limit(regexInputEvents())
	if (zoom_limit == 1) {
		tags$h4("Zooming not available with this view")
	} else {
		sliderInput(
			"regexZoomID",
			label = h4("Zoom in and out by event similarity:"),
			1,zoom_limit,zoom_limit,step = 1,ticks = FALSE
		)
	}
})

output$Regular_Expression_controls_3 <- renderUI({
	maxrows <- length(unique(regexInputEvents()[['threadNum']]))
	sliderInput(
		"regexVerbatimRows",
		label = h4("How many threads to view:"),
		min = 1,max = maxrows,c(1,min(maxrows,10)),step = 1,ticks = FALSE
	)
})

output$Regular_Expression_controls_4 <- renderText({
  req(input$regexVerbatimRows[1])
	paste(
		thread_text_vector(
			regexInputEvents(),
			'threadNum',
			get_Zoom_REGEX(),
			','
		)[input$regexVerbatimRows[1]:input$regexVerbatimRows[2]],
		'\n'
	)
})

output$Regular_Expression_controls_5 <- renderUI({
	maxrows <- length(unique(regexInputEvents()[['threadNum']]))
	sliderInput(
		"numRegexInputRows",
		label = h4("How many ngrams/labels to make:"),
		min = 1,max = 10,3,step = 1,ticks = FALSE
	)
})

# create several rows of inputs
output$Regular_Expression_controls_6 <- renderUI({
  req(input$numRegexInputRows)
	# create some select inputs
	lapply(1:input$numRegexInputRows, function(i) {
		fluidRow(
			column(2,textInput(paste0('regex', i), paste0('Ngram-', i) ), offset = 1),
			column(2,textInput(paste0('regexLabel', i), paste0('Label-', i)))
		)
	})
})

# need to add commas and probably add slider for upper/lower bound and threshold
# freqNgramSelections <- reactive({ selectize_frequent_ngrams(regexInputEvents() , 'threadNum', get_Zoom_REGEX(), 2, 5, 3)})

output$Regular_Expression_controls_7 <- renderUI({
	tags$div(
		align="left",
		textInput("EventMapName3", label = h4("Enter label to save result"), value = ""),
		radioButtons("KeepIrregularEvents",label = h4("Keep irregular events:"), choices = c('Keep', 'Drop'), inline = TRUE),
		actionButton("EventButton3", "Create New Mapping"),
		span(textOutput("EventValidate3"), style="color:red")
	)
})

#### Find/Replace Pattens (Frequen N-Gram) sub-tab ####

output$Frequent_Ngram_controls_1 <- renderUI({
	tags$div(
		align="left",
		selectizeInput("freqNgramInputMapID",label = h4("Start with this POV:"), get_POV_names())
	)
})

output$Frequent_Ngram_controls_2 <- renderUI({
	zoom_limit <- zoom_upper_limit(freqNgramInputEvents())
	if (zoom_limit == 1) {
		tags$h4("Zooming not available with this POV")
	} else {
		sliderInput(
			"freqNgramZoomID",
			label = h4("Zoom in and out by event similarity:"),
			1,zoom_limit,zoom_limit,step = 1,ticks=FALSE
		)
	}
})

output$Frequent_Ngram_controls_21 <- renderUI({
	tags$div(
		align = "left",
		sliderInput(
			"freqNgramRange",
			label = h4("Size of nGrams between:"),
			min = 2,max = 10,c(2,5),step = 1,ticks = FALSE
		)
	)
})

output$Frequent_Ngram_controls_3 <- renderUI({
	maxrows <- length(unique(freqNgramInputEvents()[['threadNum']]))
	sliderInput(
		"freqNgramVerbatimRows",
		label = h4("How many threads to view:"),
		min = 1,max = maxrows,c(1,min(maxrows,10)),step = 1,ticks = FALSE
	)
})

output$Frequent_Ngram_controls_4 <- renderText({
  req(freqNgramInputEvents(), input$freqNgramVerbatimRows[1])
	paste(
		thread_text_vector(
			freqNgramInputEvents(),
			'threadNum',
			get_Zoom_freqNgram(),
			', '
		)[input$freqNgramVerbatimRows[1]:input$freqNgramVerbatimRows[2]],
		'\n'
	)
})

output$Frequent_Ngram_controls_5 <- renderUI({
	maxrows <- length(unique(freqNgramInputEvents()[['threadNum']]))
	sliderInput(
		"numfreqNgramInputRows",
		label = h4("How many ngrams/labels to make:"),
		min = 1,max = 10,3,step = 1,ticks = FALSE
	)
})

output$freqnGramTable <- DT::renderDataTable(fng_select(), filter = "top")

output$Frequent_Ngram_controls_7 <- renderUI({
	tags$div(
		align="left",
		textInput("EventMapName4", label = h4("Enter label to save result"), value = "" ),
		radioButtons("KeepIrregularEvents_2",label = h4("Keep irregular events:"), choices = c('Keep', 'Drop'), inline = TRUE),
		actionButton("EventButton4", "Create New Mapping"),
		span(textOutput("EventValidate4"), style="color:red")
	)
})


#### Cluster For Zooming sub-tab ####

output$Cluster_Event_controls_1 <- renderUI({
	tags$div(
		align="left",
		tags$h4("Group similar events to together to allow zooming"),
		selectizeInput("ClusterEventsInputID",label = h4("Start with this POV:"), get_POV_names() ))
})

output$Cluster_Event_controls_2 <- renderUI({
	tags$div(
		align = "left",
		radioButtons(
			"ClusterMethodID",
			label = h4("Cluster based on:"),
			choices = c("Network Proximity","Contextual Similarity", "Sequential similarity"),
			selected="Network Proximity"
		)
	)
})

output$Cluster_Event_controls_3 <- renderUI({
	tags$div(
		align = "left",
		textInput("EventMapName6", label = h4("Enter label to save this view + new clustering"), value = ""),
		actionButton("EventButton6", "Cluster Events"),
		span(textOutput("EventValidate6"), style="color:red")
	)
})

output$dendroClusterResult <- renderDendroNetwork({
	dendroNetwork(
		cluster_result(),
		treeOrientation = "horizontal",
		textColour = "black"
	)
})

#### Select Subsets sub-tab ####

# Controls for the whole set of tabs
output$SelectSubsetControls_1 <- renderUI({
	selectizeInput("SelectSubsetMapInputID",label = h4("Start with this POV:"),get_POV_names())
})

output$SelectSubsetControls_2 <- renderUI({
	tags$div(
		align="left",
		textInput(
			"SelectSubsetMapName",
			label = h4(paste("Enter label for this subset of the", input$SelectSubsetMapInputID," mapping")),
			value = ""
		),
		actionButton("SelectSubsetButton", "Save Subset"),
		span(textOutput("SelectSubsetValidate"), style="color:red")
	)
})

output$SelectSubsetDataTable <- DT::renderDataTable({ subsetEventsViz() }, filter = "top")

#### Manage Event Maps sub-tab ####

output$Manage_Event_Map_controls <- renderUI({
	tags$div(
		align = "left",
		tags$h4("Select event mapping to export or delete"),
		selectizeInput("ManageEventMapInputID",label = h4("Choose POV:"), get_POV_names() ),
		actionButton("ExportMappingRData", "Export RData file"),
		actionButton("ExportMappingCsv", "Export to csv"),
		actionButton("DeleteMappingButton", "Delete")
	)
})
