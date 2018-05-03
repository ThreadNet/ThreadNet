# Server Output Functions for Choose POV Tab 

#### Define Threads sub-tab ####

output$POV_Tab_Controls_2 <- renderUI({
	checkboxGroupInput(
		"THREAD_CF_ID",
		"Select columns to define threads:",
		cfnames(selectOccFilter()),
		selected = get_THREAD_CF(),
		inline = TRUE
	)
})

output$ContextFlowers_2 <- renderPlotly({
	CF_multi_pie(
		selectOccFilter(),
		get_THREAD_CF()
	)
})

#### Define Events sub-tab ####

output$POV_Tab_Controls_3 <- renderUI({
	checkboxGroupInput(
		"EVENT_CF_ID",
		"Select columns to mark events:",
		cfnames(selectOccFilter()),
		selected = get_EVENT_CF(),
		inline = TRUE
	)
})

output$ContextFlowers_3 <- renderPlotly({
	CF_multi_pie(
		selectOccFilter(),
		get_EVENT_CF()
	)
})

#### Preview Threads sub-tab ####

output$previewThreadMap_1 <- renderPlotly({
	threadMap(
		threadedOcc(),
		"threadNum",
		"tStamp",
		newColName(get_EVENT_CF()),
		16
	)
})

output$Preview_Thread_Output_1 <- renderText({ paste(numThreads(threadedOcc(), "threadNum"),"threads in the selected data.")})

#### Preview Data sub-tab ####

output$Thread_Tab_Output_1 <- DT::renderDataTable({ threadedOcc()})
