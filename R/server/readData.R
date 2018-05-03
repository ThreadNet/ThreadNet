# Server Output Functions for Read Data Tab

output$Data_Tab_Controls_1 <- renderUI({
	tags$div(
		align = "center",
		fileInput(
			"file1",
			"Please select a .csv file",
			accept = c(
				"text/csv",
				"text/comma-separated-values,text/plain",
				".csv"
			)
		) 
	)
})

output$Data_Tab_Controls_2 <- renderUI({
	checkboxGroupInput(
		"CFcolumnsID",
		"Select columns to include in analysis:",
		cfnames(occ()),
		selected = cfnames(occ()),
		inline = TRUE
	)
})


output$Data_Tab_Output_2 <- DT::renderDataTable({
	selectOcc() },
	filter = "top",
	options = list(autoWidth = TRUE)
)
