# Server: Read Data functions
##############################
# Local Variable Definitions #
##############################

# limit what files to accept on input
fileTypes <- c("text/csv","text/comma-separated-values,text/plain",".csv")

##########################
# Tab Output Definitions #
##########################

# file selector dialog
output$fileSelector <- renderUI({
  tags$div(
    align = "center",
    fileInput("inputFile","Please select a .csv file",accept=fileTypes)
  )
})

# user selects time scale for graphics
output$timeSelector <- renderUI({
  tags$div(
    align = "center",
  radioButtons(
    "timeScaleID",
    "Select time scale for graphs:",
    c( 'mins', 'hours','days','weeks', 'auto'),
    selected = 'mins',
    inline = TRUE)
  )
})

# user selects columns to include
# [-1] drop first column (tStamp)
output$columnSelector <- renderUI({
  checkboxGroupInput(
    "CFcolumnsID",
    "Select columns to include in analysis:",
    names(occ())[-1],
    selected = names(occ())[-1],
    inline = TRUE
  )
})

# user filters data for review
output$dataFilter <- DT::renderDataTable(
  selectOcc(),
  filter  = "top",
  options = list(autoWidth = TRUE)
)

####################
# Helper Functions #
####################

# read in user supplied file
# return dataframe of occurences
parseInputData <- function(inputFile){

  withProgress(message = "Cleaning Data", value = 0,{

  # read in the table of occurrences
  fileRows <- read.csv(inputFile$datapath)

  incProgress(1/3)

  # validate expected columns (first col must be "tStamp" or "sequence")
  firstCol <- names(fileRows)[1]

  # if first col is tStamp, no changes, else
  # if first col is sequence, add a default timestamp, else
  # supply a default dataset
  if(firstCol != "tStamp"){
    if(firstCol != "sequence"){
      fileRows <- read.csv("sampleData.csv") # This could be handled in a config file instead of being hard coded
    } else {
      fileRows <- add_relative_timestamps(fileRows)
    }
  }
  incProgress(2/3)

  # clean the data
  cleanData <- cleanOcc(fileRows)

  incProgress(3/3)

  })


  shinyjs::show(selector = "#navbar li a[data-value=choosePOV]")
  shinyjs::hide(selector = "#navbar li a[data-value=visualize]")
  shinyjs::hide(selector = "#navbar li a[data-value=subsets]")
  shinyjs::hide(selector = "#navbar li a[data-value=comparisons]")
  shinyjs::hide(selector = "#navbar li a[data-value=movingWindow]")
  shinyjs::hide(selector = "#navbar li a[data-value=parameterSettings]")



  # return a valid dataframe of occurences
  return(cleanData)
}

# add initial "tStamp" column if missing in original input data
# Start time for all threads is the same: "2017-01-01 00:00:00"  Happy New Year!
add_relative_timestamps <- function(fileRows){

  startTime <- as.POSIXct("2017-01-01 00:00:00")

  # add the column at the beginning
  fileRows <- cbind(startTime + 60*as.numeric(as.character(fileRows[["sequence"]])), fileRows)

  # set the column name
  names(fileRows)[1] <- "tStamp"

  return(fileRows)

}

# clean up the raw occurrence data
# Remove blanks for n-gram functionality
cleanOcc <- function(fileRows){

  # extract tStamp
  tStamp <- fileRows$tStamp

  # confirm all spaces are converted to underscores in non tStamp columns; set as factors
  cleanedCF <- data.frame(lapply(fileRows[2:ncol(fileRows)], function(x){ gsub(" ","_",x)}))

  # bind tStamp back to cleaned data
  complete <- cbind(tStamp,cleanedCF)

  # force tStamp into a "YMD_HMS" format
  complete$tStamp <- as.character(complete$tStamp)
  complete$tStamp <- parse_date_time(complete$tStamp, c("dmy HMS", "dmY HMS", "ymd HMS","dmy HM", "dmY HM", "ymd HM"))

  # add weekday and month
  complete$weekday <- as.factor(weekdays(as.Date(complete$tStamp)))
  complete$month   <- as.factor(months(as.Date(complete$tStamp)))

  return(complete)
}



