#' Fetch data from EJSCREEN
#'
#' This function looks for data from EJSCREEN. First checks if from EJSCREEN is
#' in package directory. If not, it creates a directory and downloads most recent
#' data.
#'
#' @param year Users may select EJSCREEN data vintage from any year in 2020-2023.
#' @param state_filter User-selected state abbreviation (e.g. "AR") can restrict screening to a single state. Default is to screen entire US.
#'
#' @return sf data.frame containing EJSCREEN block group data
#' @export
#'
#' @examples
fetch_data_ej <- function(year = NULL, state_filter = NULL){
  #first check if data folder exists
  ifelse(!dir.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                            "/EJSCREEN data")),
         dir.create(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                           "/EJSCREEN data")), FALSE)


  #edited function to download gdb
  options(download.file.method="libcurl")

  # NOTE: REMOVE HI, AK, and islands for projection purposes
  #if files do not exist, go get most recent. If files do exist, open most recent.

  # Option to filter states--only used if user specifies this
  filter_state <- function(data, state_filter){
    if(!is.null(state_filter)){
      if(state_filter %in% unique(data$ST_ABBREV)){
        data <- data %>%
          dplyr::filter((ST_ABBREV %in% state_filter)) %>%
          dplyr::filter(!(ST_ABBREV %in% c("GU","MP","VI","AS")))
      }
    } else {
      data <- data %>%
        dplyr::filter(!(ST_ABBREV %in% c("GU","MP","VI","AS")))
    }
  }

  # If year is set to null, check most FTP for up-to-date year.
  # Then see if that current file exists in package directory.
  # If so, below use current file rather than re-downloading
  if(is.null(year)){
    latestavailableyear <- function(mypath){
      calendaryear <- as.numeric(format(Sys.time(), "%Y"))
      yrschecked <- 2015:calendaryear
      temp1 <-  lapply(paste0("https://gaftp.epa.gov/EJSCREEN/",
                              yrschecked, "/", sep = ""), httr::GET,
                       config = httr::config(connecttimeout = 20))
      temp2 <- sapply(temp1, "[[", 2)
      exists.fun <- function(x){
        ifelse(x>200, FALSE, TRUE)
      }
      return(yrschecked[max(which(sapply(temp2, exists.fun)))])
    }
    year <- latestavailableyear(ftpurlbase)
  }


  #Block group level data and state percentiles
  #*# ICF: The if statement needed to be adjusted to accommodate different file names across different EJSCREEN dataset vintages.
  if(!(file.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                        "/EJSCREEN data/EJSCREEN_",year,"_StatePctile.gdb")))){
    #If data not downloaded, download most recent data
    gdb_stpctile <- ejscreen_download(
      folder = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                      "/EJSCREEN data"),
      yr = year,
      file="StatePctile", state=state_filter)
  } else {
    #if data exist in local directory, load data for the latest year available
    #if user does not want to use data already in directory and wants to re-download
    ##newer data, user should remove existing data from local directory.
    gdbname <- paste0("EJSCREEN_", year, "_StatePctile.gdb")
    gdb_stpctile <- sf::st_read(dsn = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                             "/EJSCREEN data/",gdbname),
                                layer = sf::st_layers(dsn = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                                              "/EJSCREEN data/",gdbname))[[1]]) %>%
      filter_state(state_filter) %>%
      dplyr::rename_at(dplyr::vars(dplyr::starts_with("P_")), ~ paste0(., '_state'))
  }

  #national percentiles
  if(!(file.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                          "/EJSCREEN data/EJSCREEN_",year,"_USPR.csv")))){
    csv_cbg <- ejscreen_download(folder=paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                                      "/EJSCREEN data"),
                                 yr = year,
                                 file="CBG_Data")
  } else {
    csvname <- paste0("EJSCREEN_", year, "_USPR.csv")
    csv_cbg <- data.table::fread(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                        "/EJSCREEN data/",csvname),
                                 colClasses = 'character') %>%
      dplyr::select(ID, dplyr::starts_with("P_")) %>%
      dplyr::rename_at(dplyr::vars(-ID), ~ paste0(., '_US'))  %>%
      dplyr::mutate_at(dplyr::vars(-ID), as.numeric)
  }

  data.state.cbg <- gdb_stpctile %>%
    dplyr::left_join(csv_cbg, by=c("ID"="ID")) %>%
    filter_state(state_filter=state_filter) %>%
    dplyr::filter(!(ACSTOTPOP==0))

  return(data.state.cbg)
}
