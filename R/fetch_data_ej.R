#' Fetch data from EJSCREEN
#'
#' This function looks for data from EJSCREEN. First checks if from EJSCREEN is
#' in working directory. If not, it creates a directory and downloads most recent
#' data.
#' @param working_dir
#' @param state_filter Users may restrict screening to a particular state in the
#' contiguous US. If so, users can specify a state. Default is to conduct
#' screening for the entire contiguous US.
#'
#' @return
#' @export
#'
#' @examples
fetch_data_ej <- function(working_dir, state_filter){
  #first check if data folder exists
  ifelse(!dir.exists(paste0(working_dir, "/EJSCREEN data")), dir.create(paste0(working_dir, "/EJSCREEN data")), FALSE)

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
          filter((ST_ABBREV %in% state_filter)) %>%
          dplyr::filter(!(ST_ABBREV %in% c("AK","HI","GU","MP","VI","AS")))
      }
    } else {
      data <- data %>%
        dplyr::filter(!(ST_ABBREV %in% c("AK","HI","GU","MP","VI","AS")))
    }
  }

  #Block group level data and state percentiles
  #*# ICF: The if statement needed to be adjusted to accommodate different file names across different EJSCREEN dataset vintages.
  if(identical(list.files(path=paste0(working_dir, "/EJSCREEN data"), pattern="StatePct"), character(0)) ){
    #If data not downloaded, download most recent data
    gdb_stpctile <- ejscreen.download.local(folder=paste0(working_dir, "/EJSCREEN data"), file="StatePctile", state=state_filter)
  } else {
    #if data exist in local directory, load data for the latest year available
    #if user does not want to use data already in directory and wants to re-download
    ##newer data, user should remove existing data from local directory.
    calendar_year <- max(as.numeric(gsub("[^0-9]", "", list.files(path=paste0(working_dir,"/EJSCREEN data/"), pattern="StatePct"))))
    if (calendar_year > 2019 & calendar_year < 2022) {
      gdbname <- paste0("EJSCREEN_", calendar_year, "_StatePctile.gdb")
    } else if (calendar_year == 2022) {
      gdbname <- paste0("EJSCREEN_", calendar_year, "_StatePct_with_AS_CNMI_GU_VI.gdb")
    }
    #*# ICF: Added missing state filter
    gdb_stpctile <- sf::st_read(dsn = paste0(working_dir,"/EJSCREEN data/",gdbname), 
                                layer =st_layers(dsn = paste0(working_dir,"/EJSCREEN data/",gdbname))[[1]]) %>%
      filter_state(state_filter) %>%
      st_transform("ESRI:102005") %>%
      mutate(area_bg = st_area(Shape)) %>%
      rename_at(vars(starts_with("P_")), ~ paste0(., '_state'))

  }
  #*# ICF: Is this just a QA check? Suggest removing.
  st_crs(gdb_stpctile)$units

  #national percentiles
  #*# ICF: The if statement needed to be adjusted to accommodate different file names across different EJSCREEN dataset vintages.
  if(identical(list.files(path=paste0(working_dir,"/EJSCREEN data"), pattern="USPR.csv|Full_with_AS_CNMI_GU_VI.csv"), character(0)) ){
    #If data not downloaded, download most recent data
    #*# ICF: Made the function call more generic to accommodate different file names across different EJSCREEN dataset vintages.
    csv_uspr <- ejscreen.download.local(folder=paste0(working_dir,"/EJSCREEN data"), file="CBG_Data")
  } else {
    #if data exist in local directory, load data for the latest year available
    #if user does not want to use data already in directory and wants to re-download
    ##newer data, user should remove existing data from local directory.
    calendar_year <- max(as.numeric(gsub("[^0-9]", "", list.files(path=paste0(working_dir,"/EJSCREEN data/"), 
                                                                  pattern="USPR.csv|Full_with_AS_CNMI_GU_VI.csv"))))
    if (calendar_year > 2019 & calendar_year < 2022) {
      csvname <- paste0("EJSCREEN_", calendar_year, "_USPR.csv")
    } else if (calendar_year == 2022) {
      csvname <- paste0("EJSCREEN_", calendar_year, "_Full_with_AS_CNMI_GU_VI.csv")
    }
    csv_cbg <- read_csv(paste0(working_dir,"/EJSCREEN data/",csvname), col_types=cols(.default = "c")) %>%
      dplyr::select(ID, starts_with("P_")) %>%
      rename_at(vars(-ID), ~ paste0(., '_US'))  %>%
      na_if("None") %>%
      mutate_at(vars(-ID), as.numeric)
  }


  data.state.cbg <- gdb_stpctile %>%
    left_join(csv_cbg, by=c("ID"="ID")) %>%
    filter_state(state_filter=state_filter) %>%
    dplyr::filter(!(ACSTOTPOP==0))

  return(data.state.cbg)

}
