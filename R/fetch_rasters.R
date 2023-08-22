#' Support function to download and extract pop count raster data for analysis
#'
#' @param year User-specified year of data vintage for analysis.
#'
#' @return A raster object
#' @export
#'
#' @examples
fetch_rasters <- function(year = NULL){

  # Create folder if it doesn't exist
  silent <- ifelse(!dir.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                            "/Pop Rasters")),
         dir.create(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                           "/Pop Rasters")), FALSE)

  # If both population raster files aren't there, download them.
  if (length(list.files(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                               "/Pop Rasters"))) < 2){
    utils::download.file(url ='https://ejscreenbatch-data.s3.amazonaws.com/batch_rasters.zip',
                         destfile = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                           "/Pop Rasters/",
                                           "batch_rasters.zip"))

    unzip(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                 "/Pop Rasters/",
                 "batch_rasters.zip"),
          exdir = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                         "/Pop Rasters"),
          junkpaths = T
    )

    silent <- file.remove(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                       "/Pop Rasters/",
                       "batch_rasters.zip"))
  }

  # Open the file appropriate to EJSCREEN vintage in use.
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

  if (year > 2021){
    raster <- terra::rast(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                      "/Pop Rasters/",
                                      "uspop2020.tif"))
  } else {
    raster <- terra::rast(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                      "/Pop Rasters/",
                                      "uspop2010.tif"))
  }
  return(raster)
}
