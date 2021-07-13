#' Fetch data from EJSCREEN
#'
#' This function looks for data from EJSCREEN. First checks if from EJSCREEN is
#' in working directory. If not, it creates a directory and downloads most recent
#' data.
#'
#' @param state_filter Users may restrict screening to a particular state in the
#' contiguous US. If so, users can specify a state. Default is to conduct
#' screening for the entire contiguous US.
#'
#' @return
#' @export
#'
#' @examples
 fetch_data_ej <- function(state_filter){
  #first check if data folder exists
  ifelse(!dir.exists("EJSCREEN data"), dir.create("EJSCREEN data"), FALSE)

  #edited function to download gdb
  options(download.file.method="libcurl")

  # NOTE: REMOVE HI, AK, and islands for projection purposes
  #if files do not exist, go get most recent. If files do exist, open most recent.
  calendaryear <- as.numeric(format(Sys.time(), "%Y"))

  # Option to filter states--only used if user specifies this
  filter_state <- function(data, state_filter){
    if(!is.null(state_filter)){
      if(state_filter %in% unique(data$ST_ABBREV)){
        data <- data %>%
          filter(!(ST_ABBREV %in% state_filter))
      }
    } else {
      data <- data %>%
        filter(!(ST_ABBREV %in% c("AK","HI","GU","MP","VI","AS")))
    }
  }

  #Block group level data and state percentiles
  if(identical(list.files(path=paste0("EJSCREEN data"), pattern="StatePctile.gdb"), character(0)) ){
    #If data not downloaded, download most recent data
    gdb_stpctile <- ejscreen.download.local(folder=paste0("EJSCREEN data"), file="StatePctile", state=state_filter)
  } else {
    #if data exist in local directory, load data for the latest year available
    #if user does not want to use data already in directory and wants to re-download
    ##newer data, user should remove existing data from local directory.
    calendar_year <- max(as.numeric(gsub("[^0-9]", "", list.files(path=paste0("EJSCREEN data/"), pattern="StatePctile.gdb"))))
    gdb_stpctile <- sf::st_read(dsn = paste0("EJSCREEN data/EJSCREEN_",calendar_year,"_StatePctile.gdb"), layer =st_layers(dsn = paste0("EJSCREEN data/EJSCREEN_",calendar_year,"_StatePctile.gdb"))[[1]]) %>%
      filter_state(state_filter=state_filter) %>%
      st_transform("ESRI:102005") %>%
      mutate(area_bg = st_area(Shape)) %>%
      rename_at(vars(starts_with("P_")), ~ paste0(., '_state'))

  }
  st_crs(gdb_stpctile)$units


  #national percentiles
  if(identical(list.files(path=paste0("EJSCREEN data"), pattern="USPR.csv"), character(0)) ){
    #If data not downloaded, download most recent data
    csv_uspr <- ejscreen.download.local(folder=paste0("EJSCREEN data"), file="USPR")
  } else {
    #if data exist in local directory, load data for the latest year available
    #if user does not want to use data already in directory and wants to re-download
    ##newer data, user should remove existing data from local directory.
    calendar_year <- max(as.numeric(gsub("[^0-9]", "", list.files(path=paste0("EJSCREEN data/"), pattern="USPR.csv"))))
    csv_uspr <- read_csv(paste0("EJSCREEN data/EJSCREEN_",calendar_year,"_USPR.csv"), col_types=cols(.default = "c")) %>%
      dplyr::select(ID, starts_with("P_")) %>%
      rename_at(vars(-ID), ~ paste0(., '_US')) %>%
      na_if("None") %>%
      mutate_at(vars(-ID), as.numeric)
  }


  data.state.uspr <- gdb_stpctile %>%
    left_join(csv_uspr, by=c("ID"="ID")) %>%
    filter(!(ACSTOTPOP==0))

  return(data.state.uspr)

}
