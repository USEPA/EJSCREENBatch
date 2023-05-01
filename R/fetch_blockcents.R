

fetch_blockcents <- function(year){
  
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
