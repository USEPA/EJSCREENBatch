#' EJ Demographics
#'
#' Creates boxplots that display distribution of demographic characteristics of
#' potential concern in areas of interest.
#'
#' @param data
#' @param gis_method User specified method of creating buffers around areas of interest (intersect, centroid, intersection).
#' @param buffer Distance(s) used to create buffers.
#' @param threshold User specified threshold to represent potential concern. Default is 80%
#'
#' @return
#' @export
#'
#' @examples
EJdemographics <- function(data, gis_method, buffer, threshold, working_dir){
  ifelse(!dir.exists(file.path(working_dir,Sys.time(),"plots")), dir.create(file.path(working_dir,Sys.time(),"plots")), FALSE)

  data.list <- list()
  i=1
  for(percentile in c('state','US')){
    jpeg(file=paste0(Sys.time(),"/plots/demographics_boxplot_gis_",gis_method,"_radius",buffer,"_",percentile,".jpeg"))
    demo.df <- data %>%
      as.data.frame() %>%
      dplyr::select(as.character(sprintf('P_MINORPCT_%s', percentile)),
             as.character(sprintf('P_LWINCPCT_%s', percentile)),
             as.character(sprintf('P_LESHSPCT_%s', percentile)),
             as.character(sprintf('P_LNGISPCT_%s', percentile)),
             as.character(sprintf('P_UNDR5PCT_%s', percentile)),
             as.character(sprintf('P_OVR64PCT_%s', percentile))) %>%
      rename(Minority             = as.character(sprintf('P_MINORPCT_%s', percentile)),
             Low_Income           = as.character(sprintf('P_LWINCPCT_%s', percentile)),
             Less_than_HS_Edu     = as.character(sprintf('P_LESHSPCT_%s', percentile)),
             Linguistic_Isolation = as.character(sprintf('P_LNGISPCT_%s', percentile)),
             Age_Under_5          = as.character(sprintf('P_UNDR5PCT_%s', percentile)),
             Age_Over_64          = as.character(sprintf('P_OVR64PCT_%s', percentile))) %>%
      mutate_all(as.numeric) %>%
      boxplot(.,
              names = c("% Minority","Low Income", "< HS Edu.","Ling. Isolation",  "< Age 5", "> Age 64"),
              ylab="State Percentile",
              main=paste0("Demographics relative to ", percentile," \n GIS Method: ",gis_method," \n Buffer: ",buffer," mile Distance"), las=2, cex.axis=.75)
    abline(h=threshold, col="Red", lty=5)
    dev.off()

    assign(paste0("demographics.",percentile),demo.df)
  }

  data.list <- sapply(objects(pattern="^demographics.", envir = environment()),get, envir = environment(),simplify=F, USE.NAMES=T)
  return(data.list)
}
