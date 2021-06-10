#' Fetch data from EJSCREEN
#'
#' This function checks if the most recent data from EJSCREEN is already
#' part of the package.
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
  ifelse(!dir.exists("data"), dir.create("data"), FALSE)
  ifelse(!dir.exists("data/2020 dataset EJSCREEN/"), dir.create("data/2020 dataset EJSCREEN/"), FALSE)

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
  if(is.null(list.files(path=paste0("data/",calendaryear," dataset EJSCREEN"), pattern="StatePctile.gdb"))){
    gdb_stpctile <- ejscreen.download.local(folder=paste0("data/",calendaryear," dataset EJSCREEN"), file="StatePctile")
  } else {
    gdb_stpctile <- sf::st_read(dsn = paste0("data/EJSCREEN_2020_StatePctile.gdb"), layer = paste0("EJSCREEN_2020_StatePct")) %>%
      filter_state(state=state_filter) %>%
      st_transform("ESRI:102005") %>%
      mutate(area_bg = st_area(Shape)) %>%
      rename_at(vars(starts_with("P_")), ~ paste0(., '_state'))

  }
  st_crs(gdb_stpctile)$units



  #national percentiles
  if(is.null(list.files(path=paste0("data/", calendaryear," dataset EJSCREEN"), pattern="USPR.csv"))){
    csv_uspr <- ejscreen.download.local(folder=paste0("data/", calendaryear," dataset EJSCREEN"), file="USPR")
  } else {
    csv_uspr <- read_csv("data/EJSCREEN_2020_USPR.csv", col_types=cols(.default = "c")) %>%
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
