#####################################################
# Global_POV is a global variable

#' @title check_POV_name
#' @description Checks the name attempting to be create against the list of
#' map names in memory and forces the creation of a new name.
#' @name check_POV_name
#' @param mapname name of map attempting to be created
#' @return TRUE or FALSE
#' @export
check_POV_name <- function(mapname){

    if (mapname %in% get_POV_name_list()){
      existingMap = TRUE
    } else {
      existingMap = FALSE
    }
    return(existingMap)
}

#' @title get_POV_name_list
#' @description Get list of POV names for all of the dropdown boxes on the UI
#' @name get_POV_name_list
#' @return List of POV names
#' @export get_POV_name_list
get_POV_name_list <- function(){

  # just return the global variable
  n <- unlist(Global_POV_Name)

  return(n)
}

#' @title store_POV
#' @description Stores the POV and context factors
#' @name store_POV
#' @param EventMapName name of map attempting to be created
#' @param e data frame with POV to be stored
#' @param thread_CF List of CFs to be stored
#' @param event_CF List of CFs to be stored
#' @return None, updates global variables
#' @export
store_POV <- function(EventMapName, e,  thread_CF, event_CF){

  # print(paste('in store_POV, EventMapName=',EventMapName))
  # print(paste('in store_POV, thread_CF=',thread_CF))
  # print(paste('in store_POV, event_CF=',event_CF))

  # make sure the name does not already exist
  if (check_POV_name(EventMapName))
    { print(paste(EventMapName, ' already exists. Cannot use the same name.')) }
  else {
        # Add the mapping to the global list of mappings.
        Global_POV <<- append( list(e), Global_POV )
        Global_POV_Name <<- append( list(paste(EventMapName)), Global_POV_Name )
        Global_POV_Event_CF <<- append( list(event_CF), Global_POV_Event_CF )
        Global_POV_Thread_CF <<- append( list(thread_CF), Global_POV_Thread_CF )
  #     Global_POV_Comparison_CF <<- append( list(comparison_CF), Global_POV_Comparison_CF )
  }
}

#' @title get_POV
#' @description Gets the data frame for the POV
#' @name get_POV
#' @param mapname name of POV map
#' @return data frame with POV
#' @export
get_POV <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )
  if (length(idx)==0)  return(NULL)

  if (idx==0) {
    return(NULL)
  } else {
  	return( Global_POV[[idx]] )
  }
}

#' @title get_POV_THREAD_CF
#' @description Gets the CFs that define threads in this POV
#' @name get_POV_THREAD_CF
#' @param mapname name of POV map
#' @return thread CFs for that POV
#' @export
get_POV_THREAD_CF <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )
  if (length(idx)==0)  return(NULL)

  if (idx==0) {
    return(NULL)
  } else {
    return( Global_POV_Thread_CF[[idx]] )
  }
}

#' @title get_POV_EVENT_CF
#' @description Gets the CFs that define events in this POV
#' @name get_POV_EVENT_CF
#' @param mapname name of POV map
#' @return event CFs for that POV
#' @export
get_POV_EVENT_CF <- function(mapname){

  idx <- which(mapname==get_POV_name_list() )
  if (length(idx)==0)  return(NULL)

  if (idx==0) {
    return(NULL)
  } else {
    return( Global_POV_Event_CF[[idx]] )
  }
}

# this one compute the list based on the other two
#' @title get_POV_COMPARISON_CF
#' @description Gets the CFs that can be used for comparisons in this POV
#' @name get_POV_COMPARISON_CF
#' @param mapname name of POV map
#' @param CF_list list of other column names
#' @return comparison CFs for that POV
#' @export
get_POV_COMPARISON_CF <- function(mapname, CF_list){

  idx <- which(mapname==get_POV_name_list() )
  if (length(idx)==0)  return(NULL)

  if (idx==0) {
    return(NULL)
  } else {
    return(  setdiff(CF_list, union(get_POV_THREAD_CF(mapname),get_POV_EVENT_CF(mapname) )) )
  }
}

#' @title delete_POV
#' @description Deletes all the data assocated with this POV
#' @name delete_POV
#' @param mapname name of POV map
#' @return  None, updates global variables
#' @export
delete_POV <- function(mapname){

  # get the index for the mapname
  idx <- which(mapname==get_POV_name_list())

  # delete all traces of it
  Global_POV[[idx]] <<- NULL
  Global_POV_Name[[idx]] <<- NULL
  Global_POV_Event_CF[[idx]] <<- NULL
  Global_POV_Thread_CF[[idx]] <<- NULL
  # Global_POV_Comparison_CF[[idx]] <<- NULL


}

#' @title export_POV
#' @description Exports the data assocated with this POV as Rdata
#' @name export_POV
#' @param mapname name of POV map
#' @return (writes Rdata file)
#' @export
export_POV <- function(mapname){

  # get the nice variable names
  nicename = paste0("POV_",mapname)
  nicename_TM = paste0("TM_",mapname)

  # assign the data to the variables
  # ??? may want to make CF for traminer output a user input.
  assign(nicename, get_POV(mapname))
  assign(nicename_TM, convert_TN_to_TramineR(get_POV(mapname),'label') )

  # save the data
  save(list=c(nicename,nicename_TM), file = paste0(nicename,".Rdata"))

}

#' @title export_POV_csv
#' @description Exports the data assocated with this POV as CSV
#' @name export_POV_csv
#' @param mapname name of POV map
#' @return (writes CSV file)
#' @export
export_POV_csv <- function(mapname){

  output = as.data.frame( get_POV(mapname) )

  output[grep('V_',colnames(output))]<-NULL

  write.csv(output, file=file.choose(), quote = TRUE, row.names = FALSE)

}

#' @title export_network
#' @description Exports the edge list for the graph that is displayed
#' @name export_network
#' @param mapname name of POV map to use as name of the file
#' @param n
#' @return (saves network into file)
#' @export
export_network <- function(mapname, CurrentNetwork ){

  # get the nice variable names
  nicename = paste0("CurrentNetwork_from_",mapname)
  nicename = "CurrentNetwork_POV"

  # get the edge list for the network
  # edge_list <- n.edgeDF

  print('saving network: CurrentNetwork_POV.Rdata')
  # save the data
  save( CurrentNetwork , file = paste0(nicename,".Rdata"))
  print(' network saved')

}

