#' Support function for EJSCREEN data download
#'
#' Downloads most data from EJSCREEN ftp. Code for this function was adapted
#' from Mark Corrales' ejscreen function on github at ejanalysis/ejscreen.
#'
#' @param folder
#' @param file
#' @param yr
#' @param ftpurlbase
#' @param justreadname
#' @param addflag
#' @param cutoff
#' @param state
#'
#' @return EJSCREEN in data.frame/sf format.
#' @export
#'
#' @examples
ejscreen_download <- function (folder = "EJSCREEN data", file, yr = NULL, ftpurlbase = "https://gaftp.epa.gov/EJSCREEN/",
                                     state=NULL, justreadname = NULL, addflag = FALSE, cutoff = 80)
{

  #*# ICF: Made the code more generic to accommodate different file names across different EJSCREEN dataset vintages.
  `%notin%` = Negate(`%in%`)
  if(file %notin% c("StatePctile","CBG_Data")){
    stop("File must be either State percentile data or National Census Block Group percentile data")
  }

    latestavailableyear <- function(mypath) {
      calendaryear <- as.numeric(format(Sys.time(), "%Y"))
      yrschecked <- 2015:calendaryear

      temp1 <-  lapply(paste0(ftpurlbase,
                              yrschecked, "/", sep = ""), httr::GET,
                       config = httr::config(connecttimeout = 30))

      temp2 <- sapply(temp1, "[[", 2)
      exists.fun <- function(x){
        ifelse(x>200, FALSE, TRUE)
      }
     return(yrschecked[max(which(sapply(temp2, exists.fun)))])
    }

    if (is.null(yr)) {
      yr <- latestavailableyear(ftpurlbase)
    } else {
      yr <- as.numeric(yr)
      if (httr::GET(paste0(ftpurlbase, yr, "/"))$status_code>200) {
        calendaryear <- as.numeric(format(Sys.time(),
                                          "%Y"))
        if (yr == calendaryear)
          warning("This version might not be available yet. The 20xx version of EJSCREEN has typically been released around Sept/Oct of that year 20xx.")
        stop("folder for that year not found on FTP site")
      }
    }

    if (yr == 2023) {
      ftpurl <- paste(ftpurlbase, yr, '/2.22_September_UseMe', sep = "")
    } else {
      ftpurl <- paste(ftpurlbase, yr, sep = "")
    }

    zipunzippednames <- function(yr) {
      if (yr >= 2019 & yr < 2022) {
        if(file == "StatePctile"){
        zipname <- paste("EJSCREEN_", yr, "_StatePctile.gdb.zip",
                         sep = "")
        unzippedname <- paste("EJSCREEN_", yr, "_StatePctile.gdb",
                         sep = "")
        } else {
          zipname <- paste("EJSCREEN_", yr, "_USPR.csv.zip",
                           sep = "")
          unzippedname <- paste("EJSCREEN_", yr, "_USPR.csv",
                           sep = "")
        }
      } else if (yr == 2022) {
        if(file == "StatePctile"){
          zipname <- paste("EJSCREEN_", yr, "_StatePct_with_AS_CNMI_GU_VI.gdb.zip",
                           sep = "")
          unzippedname <- paste("EJSCREEN_", yr, "_StatePctile.gdb",
                                sep = "")
        } else {
          zipname <- paste("EJSCREEN_", yr, "_with_AS_CNMI_GU_VI.csv.zip",
                           sep = "")
          unzippedname <- paste("EJSCREEN_", yr, "_USPR.csv",
                                sep = "")
        }
      } else if (yr == 2023) {
        if(file == "StatePctile"){
          zipname <- paste("EJSCREEN_", yr, "_BG_StatePct_with_AS_CNMI_GU_VI.gdb.zip",
                           sep = "")
          unzippedname <- paste("EJSCREEN_", yr, "_StatePctile.gdb",
                                sep = "")
        } else {
          zipname <- paste("EJSCREEN_", yr, "_BG_with_AS_CNMI_GU_VI.csv.zip",
                           sep = "")
          unzippedname <- paste("EJSCREEN_", yr, "_USPR.csv",
                                sep = "")
        }
      }
      return(c(zipname, unzippedname))
    }
    x <- zipunzippednames(yr)
    zipname <- x[1]
    unzippedname <- x[2]

    justdownload <- function(mypathfileRemote, mypathfileLocal) {
      cat("Attempting to download dataset from ", mypathfileRemote,
          "and saving as", mypathfileLocal, " \n")
      cat("This normally takes a few minutes. \n")

      getOption('timeout')
      options(timeout=600) #10 min max

      x <- utils::download.file(url = mypathfileRemote,
                                destfile = mypathfileLocal)
      if (x != 0) {
        stop("Download failed.")
      }
      if (!(file.exists(mypathfileLocal))) {
        stop("Download attempted but saved zip file not found locally")
      }
      return(NULL)
    }

    justdownload(mypathfileRemote = file.path(ftpurl, zipname),
                 mypathfileLocal = file.path(folder, paste0(unzippedname,'.zip')))

    if (zipname == "") {
      cat("\n")
    } else {
      if (zipname == paste("EJSCREEN_", yr, "_with_AS_CNMI_GU_VI.csv.zip",
                           sep = "")) {
        zipname <- paste("EJSCREEN_", yr, "_Full_with_AS_CNMI_GU_VI.csv.zip",
                         sep = "")
      }
      cat("Attempting to unzip dataset \n")
      utils::unzip(file.path(folder, paste0(unzippedname,'.zip')),
                   exdir=folder)
      file.rename(from = gsub('.zip','',file.path(folder,zipname)),
                  to = file.path(folder, unzippedname))
    }

    # Option to filter states--only used if user specifies this
    filter_state <- function(data, state_filter){
      if(!is.null(state_filter)){
        if(state_filter %in% unique(data$ST_ABBREV)){
          data <- data %>%
            dplyr::filter(!(ST_ABBREV %in% state_filter))
        }
      } else {
        data <- data %>%
          dplyr::filter(!(ST_ABBREV %in% c("GU","MP","VI","AS")))
      }
    }

  if(file=="StatePctile"){
    db <- sf::st_read(dsn = paste0(folder,"/",unzippedname),
                      layer = sf::st_layers(dsn = paste0(folder,"/",unzippedname))[[1]]) %>%
      filter_state(state_filter=state) %>%
      dplyr::rename_at(dplyr::vars(starts_with("P_")), ~ paste0(., '_state'))
  } else {
    db <- data.table::fread(paste0(folder,"/",unzippedname), colClasses = 'character') %>%
      dplyr::select(ID, dplyr::starts_with("P_")) %>%
      dplyr::rename_at(dplyr::vars(-ID), ~ paste0(., '_US')) %>%
      dplyr::mutate_at(dplyr::vars(-ID), as.numeric)
  }

  return(db)
}
