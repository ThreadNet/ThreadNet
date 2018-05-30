#####################################################
# Global_POV is a global variable

#' Checks the name attempting to be create against the list of
#' map names in memory and forces the creation of a new name.
#'
#' @param mapname name of map attempting to be created

check_POV_name <- function(mapname){

    if (mapname %in% get_POV_name_list()){
      existingMap = TRUE
    } else {
      existingMap = FALSE
    }
    return(existingMap)
}

get_POV_name_list <- function(){

  # n <- unlist(lapply(1:length(Global_POV),function(i){
  #   unlist(Global_POV[[i]][["name"]]) }))

  # just return the global variable
  n <- unlist(Global_POV_Name)

  return(n)
}


store_POV <- function(EventMapName, e, event_CF, thread_CF, comparison_CF){

  # Add the mapping to the global list of mappings. Sort by threadNum and seqNum
  # em = list(name = paste(EventMapName), threads = e[order(e[['threadNum']],e[['seqNum']]),])

  Global_POV <<- append( list(e[order(e[['threadNum']],e[['seqNum']]),]), Global_POV )
  Global_POV_Name <<- append( list(paste(EventMapName)), Global_POV_Name )
  Global_POV_Event_CF <<- append( list(event_CF), Global_POV_Event_CF )
  Global_POV_Thread_CF <<- append( list(thread_CF), Global_POV_Thread_CF )
  Global_POV_Comparison_CF <<- append( list(comparison_CF), Global_POV_Comparison_CF )

  return(em)

}

get_POV <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
  	return( Global_POV[[idx]] )
  }
}

get_POV_THREAD_CF <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
    return( Global_POV_Thread_CF[[idx]] )
  }
}

get_POV_EVENT_CF <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
    return( Global_POV_Event_CF[[idx]] )
  }
}

get_POV_COMPARISON_CF <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
    return( Global_POV_Comparison_CF[[idx]] )
  }
}

delete_POV <- function(mapname){

  # get the index for the mapname
  idx <- which(mapname==get_POV_name_list())

  # delete all traces of it
  Global_POV[[idx]] <<- NULL
  Global_POV_Name[[idx]] <<- NULL
  Global_POV_Event_CF[[idx]] <<- NULL
  Global_POV_Thread_CF[[idx]] <<- NULL
  Global_POV_Comparison_CF[[idx]] <<- NULL


}

export_POV <- function(mapname){

  # get the nice variable names
  nicename = paste0("POV_",mapname)
  nicename_TM = paste0("TM_",mapname)

  # assign the data to the variables
  assign(nicename, get_POV(mapname))
  assign(nicename_TM, convert_TN_to_TramineR(get_POV(mapname)) )

  # save the data
  save(list=c(nicename,nicename_TM), file = paste0(nicename,".Rdata"))

}

export_POV_csv <- function(mapname){

  output = as.data.frame( get_POV(mapname) )

  output[grep('V_',colnames(output))]<-NULL

  write.csv(output, file=file.choose(), quote = TRUE, row.names = FALSE)

}
