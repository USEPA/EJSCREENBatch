#' EJSCREEN data download
#'
#' Downloads most recent data from EJSCREEN. Code for this function was borrowed
#' from Mark Corrales' ejscreen function on github at ejanalysis/ejscreen.
#'
#' @param folder
#' @param file
#' @param yr
#' @param ftpurlbase
#' @param justreadname
#' @param addflag
#' @param cutoff
#' @param or.tied
#'
#' @return
#' @export
#'
#' @examples
ejscreen.download.local <- function (folder = "EJSCREEN data", file, yr = NULL, ftpurlbase = "https://gaftp.epa.gov/EJSCREEN/",
          justreadname = NULL, addflag = FALSE, cutoff = 80, or.tied = TRUE)
{

  `%notin%` = Negate(`%in%`)
  if(file %notin% c("StatePctile","USPR")){
    stop("File must be either StatePctile or USPR")
  }

    latestavailableyear <- function(mypath) {
      calendaryear <- as.numeric(format(Sys.time(), "%Y"))
      yrschecked <- 2015:calendaryear

      temp1 <-  lapply(paste0(ftpurlbase,
                              yrschecked, "/", sep = ""), httr::GET)

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
    justftpurl <- function(yr) {
      return(paste(ftpurlbase, yr, sep = ""))
    }
    ftpurl <- justftpurl(yr)
    zipunzippednames <- function(yr) {

      if (yr > 2019) {
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
      }
      return(c(zipname, unzippedname))
    }
    x <- zipunzippednames(yr)
    zipname <- x[1]
    unzippedname <- x[2]
    if (zipname == "") {
      myfilename <- unzippedname
    } else {
      myfilename <- zipname
    }
    justdownload <- function(mypathfileRemote, mypathfileLocal) {
      cat("Attempting to download dataset from ", mypathfileRemote,
          "and saving as", mypathfileLocal, " \n")
      cat("This normally takes a few minutes. \n")

      getOption('timeout')
      options(timeout=3600) #set timeout at 1hour for slower connections

      x <- utils::download.file(url = mypathfileRemote,
                                destfile = mypathfileLocal)
      if (x != 0) {
        stop("Download failed.")
      }
      if (!(file.exists(mypathfileLocal))) {
        stop("download attempted but saved zip file not found locally")
      }
      return(NULL)
    }
    justdownload(mypathfileRemote = file.path(ftpurl, myfilename),
                 mypathfileLocal = file.path(folder, myfilename))
    if (zipname == "") {
      cat("\n")
    } else {
      cat("Attempting to unzip dataset \n")
      utils::unzip(file.path(folder, myfilename), exdir=folder)
    }



  if(file=="StatePctile"){
    db <- sf::st_read(dsn = paste0(folder,"/EJSCREEN_",yr,"_StatePctile.gdb"), layer = paste0("EJSCREEN_",yr,"_StatePct")) %>%
      filter_state(state_filter=state) %>%
      st_transform("ESRI:102005") %>%
      mutate(area_bg = st_area(Shape)) %>%
      rename_at(vars(starts_with("P_")), ~ paste0(., '_state'))
  } else{
    db <- read_csv(paste0(folder,"/EJSCREEN_",yr,"_USPR.csv"), col_types=cols(.default = "c")) %>%
      select(ID, starts_with("P_")) %>%
      rename_at(vars(-ID), ~ paste0(., '_US')) %>%
      na_if("None") %>%
      mutate_at(vars(-ID), as.numeric)
  }

  return(db)
}
