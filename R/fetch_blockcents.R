#' Support function to download and import vintage-appropriate census block centroids
#'
#' @param year Year of data vintage
#'
#' @return Data.frame containing coordinates of census block centroids.
#' @export
#'
#' @examples
fetch_blockcents <- function(year = NULL){
  
  # Create folder if it doesn't exist
  ifelse(!dir.exists(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                            "/Census block data")), 
         dir.create(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                           "/Census block data")), FALSE)
  
  # If both block group centroid files aren't there, download them.
  if (length(list.files(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                    "/Census block data"))) < 2){
    utils::download.file(url ='https://ejscreenbatch-data.s3.amazonaws.com/2020_block_centroids_natl.csv.gz',
                         destfile = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                           "/Census block data/",
                                           "2020_block_centroids_natl.csv.gz"))
    utils::download.file(url ='https://ejscreenbatch-data.s3.amazonaws.com/2010_block_centroids_natl.csv.gz',
                         destfile = paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                           "/Census block data/",
                                           "2010_block_centroids_natl.csv.gz"))
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
    block <- data.table::fread(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                      "/Census block data/",
                                      "2020_block_centroids_natl.csv.gz"))
  } else {
    block <- data.table::fread(paste0(paste0(.libPaths(),'/EJSCREENbatch')[1],
                                      "/Census block data/",
                                      "2010_block_centroids_natl.csv.gz"))
  }
  return(block)
}
