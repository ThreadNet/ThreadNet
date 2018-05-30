#####################################################
# GlobalEventMappings is a global variable

#' Checks the name attempting to be create against the list of
#' map names in memory and forces the creation of a new name.
#'
#' @param mapname name of map attempting to be created

check_map_name <- function(mapname){

    if (mapname %in% get_event_mapping_name_list()){
      existingMap = TRUE
    } else {
      existingMap = FALSE
    }
    return(existingMap)
}

get_event_mapping_name_list <- function(){

  # n <- unlist(lapply(1:length(GlobalEventMappings),function(i){
  #   unlist(GlobalEventMappings[[i]][["name"]]) }))

  # just return the global variable
  n <- GlobalEventMappings_Name

  return(n)
}


store_event_mapping <- function(EventMapName, e, event_CF, thread_CF, comparison_CF){

  # Add the mapping to the global list of mappings. Sort by threadNum and seqNum
  # em = list(name = paste(EventMapName), threads = e[order(e[['threadNum']],e[['seqNum']]),])

  GlobalEventMappings <<- append( list(e[order(e[['threadNum']],e[['seqNum']]),]), GlobalEventMappings )
  GlobalEventMappings_Name <<- append( list(paste(EventMapName)), GlobalEventMappings_Name )
  GlobalEventMappings_Event_CF <<- append( list(event_CF), GlobalEventMappings_Event_CF )
  GlobalEventMappings_Thread_CF <<- append( list(thread_CF), GlobalEventMappings_Thread_CF )
  GlobalEventMappings_Comparison_CF <<- append( list(comparison_CF), GlobalEventMappings_Comparison_CF )

  return(em)

}

get_event_mapping <- function(mapname){

  idx <- which(mapname==get_event_mapping_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
  	return( GlobalEventMappings[[idx]] )
  }
}

get_event_mapping_THREAD_CF <- function(mapname){

  idx <- which(mapname==get_event_mapping_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
    return( GlobalEventMappings_Thread_CF[[idx]] )
  }
}

get_event_mapping_EVENT_CF <- function(mapname){

  idx <- which(mapname==get_event_mapping_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
    return( GlobalEventMappings_Event_CF[[idx]] )
  }
}

get_event_mapping_COMPARISON_CF <- function(mapname){

  idx <- which(mapname==get_event_mapping_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
    return( GlobalEventMappings_Comparison_CF[[idx]] )
  }
}

delete_event_mapping <- function(mapname){

  # get the index for the mapname
  idx <- which(mapname==get_event_mapping_name_list())

  # delete all traces of it
  GlobalEventMappings[[idx]] <<- NULL
  GlobalEventMappings_Name[[idx]] <<- NULL
  GlobalEventMappings_Event_CF[[idx]] <<- NULL
  GlobalEventMappings_Thread_CF[[idx]] <<- NULL
  GlobalEventMappings_Comparison_CF[[idx]] <<- NULL


}

export_event_mapping <- function(mapname){

  # get the nice variable names
  nicename = paste0("EventMap_",mapname)
  nicename_TM = paste0("TM_",mapname)

  # assign the data to the variables
  assign(nicename, get_event_mapping(mapname))
  assign(nicename_TM, convert_TN_to_TramineR(get_event_mapping(mapname)) )

  # save the data
  save(list=c(nicename,nicename_TM), file = paste0(nicename,".Rdata"))

}

export_event_mapping_csv <- function(mapname){

  output = as.data.frame( get_event_mapping(mapname) )

  output[grep('V_',colnames(output))]<-NULL

  write.csv(output, file=file.choose(), quote = TRUE, row.names = FALSE)

}
