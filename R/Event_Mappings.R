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

  n <- unlist(lapply(1:length(GlobalEventMappings),function(i){
    unlist(GlobalEventMappings[[i]][["name"]]) }))

  return(n)
}


store_event_mapping <- function(EventMapName, e){

  # Add the mapping to the global list of mappings. Sort by threadNum and seqNum
  em = list(name = paste(EventMapName), threads = e[order(e[['threadNum']],e[['seqNum']]),])

  GlobalEventMappings <<- append(list(em), GlobalEventMappings )

  return(em)

}

get_event_mapping_threads <- function(mapname){

  idx <- which(mapname==get_event_mapping_name_list() )

  if (idx==0) {
    return(NULL)
  } else {
  	return(GlobalEventMappings[[idx]][["threads"]])
  }
}

delete_event_mapping <- function(mapname){

  # get the index for the mapname
  idx <- which(mapname==get_event_mapping_name_list())

  GlobalEventMappings[[idx]] <<- NULL

}

export_event_mapping <- function(mapname){

  nicename = paste0("EventMap_",mapname)

  assign(nicename, get_event_mapping_threads(mapname))

  save(list=nicename, file = paste0(nicename,".Rdata"))

}

export_event_mapping_csv <- function(mapname){

  output = as.data.frame(get_event_mapping_threads(mapname))

  output[grep('V_',colnames(output))]<-NULL

  write.csv(output, file=file.choose(), quote = TRUE, row.names = FALSE)

}
