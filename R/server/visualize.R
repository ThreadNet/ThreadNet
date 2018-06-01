# Server Output Functions for Visualize Tab

#### Main Tab Output Functions ####

# Controls for the whole set of tabs
output$Visualize_Tab_Controls_1 <- renderUI({
	selectizeInput(
		"VisualizeEventMapInputID",
		label = h4("Choose POV:"),
		get_POV_names()
	)
})

output$Visualize_Tab_Controls_2 <- renderUI({
  req(input$VisualizeEventMapInputID)
	zoom_limit = zoom_upper_limit(get_POV(input$VisualizeEventMapInputID))
	if(zoom_limit == 1) {
		tags$h4("Zooming not available for this POV")
	} else {
		sliderInput(
			"VisualizeTabZoomID",
			label = h4("Zoom in and out by event similarity:"),
			1, zoom_limit, zoom_limit, step = 1, ticks = FALSE
		)
	}
})

output$Visualize_Tab_Controls_3 = renderUI({
  nThreads = numThreads(threadedEventsViz_ALL(),'threadNum')
  sliderInput("VisualizeRangeID",
              label=h4("Range of threads to include:"),
              1, nThreads, c(1,nThreads),step = 1, ticks=FALSE)
})

#### N-Grams sub-tab ####

# controls for ngrams display
output$nGramControls <- renderUI({
	tagList(
		sliderInput("nGramLengthID","nGram Size", 1,10,2,step = 1,ticks = FALSE),
		sliderInput("nGramDisplayThresholdID","Display threshold", 1,50,1,step = 1,ticks = FALSE)
	)
})

# NGRAMdisplay
output$nGramBarchart <- renderPlotly({
  req(input$nGramLengthID)
	ng_bar_chart(
		threadedEventsViz(),
		"threadNum",
		get_Zoom_VIZ(),
		input$nGramLengthID,
		input$nGramDisplayThresholdID
	)
})

#### Whole Sequences sub-tab ####

# Whole sequence display -- allow alternatives
output$WholeSequenceThreadMap_Sequence     <- renderPlotly({ threadMap(threadedEventsViz(), "threadNum", "seqNum", get_Zoom_VIZ(), 15)})
output$WholeSequenceThreadMap_ActualTime   <- renderPlotly({ threadMap(threadedEventsViz(), "threadNum", "tStamp", get_Zoom_VIZ(), 15)})
output$WholeSequenceThreadMap_RelativeTime <- renderPlotly({ threadMap(threadedEventsViz(), "threadNum", "relativeTime", get_Zoom_VIZ(), 15)})

#### Event Network (circle) sub-tab ####

# use this to select how to color the nodes in force layout
output$Circle_Network_Tab_Controls <- renderUI({ sliderInput("circleEdgeTheshold","Display edges above", 0,1,0,step = 0.01,ticks = FALSE )})

output$circleVisNetwork <- renderVisNetwork({
  req(input$circleEdgeTheshold)
	# first convert the threads to the network
	n <- threads_to_network_original(threadedEventsViz(), "threadNum", get_Zoom_VIZ())
	n <- filter_network_edges(n,input$circleEdgeTheshold)
	circleVisNetwork(n, TRUE)
})

#### Other Networks sub-tab ####

# use this to select how to color the nodes in force layout
output$Other_Network_Tab_Controls <- renderUI({
	button_choices <- get_POV_EVENT_CF( input$VisualizeEventMapInputID )
	tags$div(
		radioButtons(
			"OtherNetworkCF",
			"Graph co-occurrence relation between:",
			choices = button_choices,
			selected = button_choices[1], # always start with the first one
			inline = TRUE
		),
		sliderInput("otherEdgeTheshold","Display edges above",0,1,0,step = 0.01,ticks = FALSE)
	)
})

output$otherVisNetwork <- renderVisNetwork({
  req(input$otherEdgeTheshold)
	# first convert the threads to the network
	n <- normalNetwork(threadedEventsViz(), selectOccFilter(), input$OtherNetworkCF)
	n <- filter_network_edges(n,input$otherEdgeTheshold)
	circleVisNetwork(n)
})


#### Event Network (force) sub-tab ####

# use this to select how to color the nodes in force layout
output$Force_Network_Tab_Controls <- renderUI({
	button_choices <- intersect(colnames(threadedEventsViz()), cfnames(selectOccFilter()))
	tags$div(
		radioButtons(
			"NetworkGroupID",
			"Select a dimension for coloring nodes:",
			choices = button_choices,
			selected = button_choices[1], # always start with the first one
			inline=TRUE
		),
		sliderInput("forceEdgeTheshold","Display edges above",0,1,0,step = 0.01,ticks = FALSE)
	)
})

output$forceNetworkD3 <- renderForceNetwork({
  req(input$forceEdgeTheshold)
	n <- threads_to_network_original(threadedEventsViz(), 'threadNum', get_Zoom_VIZ(), input$NetworkGroupID)
	n <- filter_network_edges(n,input$forceEdgeTheshold)
	forceNetworkD3(n)
})

output$networkPie <- renderPlotly({
	req(input$Group)
	get_group <- input$Group
	CF_multi_pie_event(threadedOcc(), threadedEventsViz(), get_POV_EVENT_CF( input$VisualizeEventMapInputID ), get_group, get_Zoom_VIZ())
})

#### View Events sub-tab ####

output$VisualizeCustomNetwork_Controls_0 <- renderUI({
	radioButtons("Timesplit2", "Time Measure:", choices = c('seqNum'='seqNum','timeGap'='timeGap'), selected = "seqNum", inline = TRUE)
})

output$VisualizeCustomNetwork_Controls_1 <- renderUI({
	selectizeInput("Event",label = "Choose Event:",get_POV_EVENT_CF( input$VisualizeEventMapInputID ) )
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

#### Role Maps sub-tab ####
output$Role_map_controls <- renderUI({checkboxGroupInput("Role_map_CFs","Pick Two:", get_POV_EVENT_CF( input$VisualizeEventMapInputID ) )})
output$Role_map_output   <- renderPlotly({role_map(threadedEventsViz(), selectOccFilter(), input$Role_map_CFs)})

#### Thread Trajectories sub-tab ####
output$ThreadTrajectoriesOutput <- renderPlotly({threadTrajectory(threadedEventsViz())})


#####  pie chart display #####
output$visualizePieCharts = renderPlotly({
  CF_multi_pie(threadedEventsViz(), get_POV_EVENT_CF( input$VisualizeEventMapInputID ) )
})

# provide a data table view, as well
output$visualizePOVData <-DT::renderDataTable(
  threadedEventsViz(),
  options = list(autoWidth = TRUE))
