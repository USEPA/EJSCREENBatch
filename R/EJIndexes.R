#' EJ Indexes
#'
#' Creates boxplots that display distribution of EJ indexes of
#' potential concern in areas of interest.
#'
#' @param data
#' @param gis_method User specified method of creating buffers around areas of interest (intersect, centroid, intersection).
#' @param buffer  Distance(s) used to create buffers.
#' @param threshold User specified threshold to represent potential concern. Default is 80%.
#'
#' @return
#' @export
#'
#' @examples
EJIndexes <- function(data, gis_method, buffer,threshold, working_dir){
  ifelse(!dir.exists(file.path(working_dir,Sys.time(),"plots")), dir.create(file.path(working_dir,Sys.time(),"plots")), FALSE)

  data.list <- list()
  i=1
  for(percentile in c('state','US')){
    jpeg(file=paste0(Sys.time(),"/plots/Indexes_boxplot_gis_",gis_method,"_radius",buffer,"_",percentile,".jpeg"))
    demo.df <- data %>%
      as.data.frame() %>%
      dplyr::select(as.character(sprintf('P_LDPNT_%s', percentile)),
             as.character(sprintf('P_DSLPM_%s', percentile)),
             as.character(sprintf('P_CANCR_%s', percentile)),
             as.character(sprintf('P_RESP_%s', percentile)),
             as.character(sprintf('P_PTRAF_%s', percentile)),
             as.character(sprintf('P_PWDIS_%s', percentile)),
             as.character(sprintf('P_PNPL_%s', percentile)),
             as.character(sprintf('P_PRMP_%s', percentile)),
             as.character(sprintf('P_PTSDF_%s', percentile)),
             as.character(sprintf('P_OZONE_%s', percentile)),
             as.character(sprintf('P_PM25_%s', percentile)),
             as.character(sprintf('P_VULEOPCT_%s', percentile))) %>%
      rename(Lead_Paint                            = as.character(sprintf('P_LDPNT_%s', percentile)),
             Diesel_PM                             = as.character(sprintf('P_DSLPM_%s', percentile)),
             Air_Toxics_Cancer_Risk                = as.character(sprintf('P_CANCR_%s', percentile)),
             Air_Toxics_Respiratory_Hazard         = as.character(sprintf('P_RESP_%s', percentile)),
             Traffic_Proximity                     = as.character(sprintf('P_PTRAF_%s', percentile)),
             Major_WW_Dischargers                  = as.character(sprintf('P_PWDIS_%s', percentile)),
             Nation_Priorities_List                = as.character(sprintf('P_PNPL_%s', percentile)),
             Risk_Mgmt_Plan_Facilities             = as.character(sprintf('P_PRMP_%s', percentile)),
             Treatment_Storage_Disposal_Facilities = as.character(sprintf('P_PTSDF_%s', percentile)),
             Ozone_Level                           = as.character(sprintf('P_OZONE_%s', percentile)),
             PM                                    = as.character(sprintf('P_PM25_%s', percentile)),
             Demographic_Index                     = as.character(sprintf('P_VULEOPCT_%s', percentile))) %>%
      mutate_all(as.numeric) %>%
      boxplot(.,
              names = c("Lead", "Diesel PM", "Air, Cancer", "Resp. Hazard", "Traffic", "WW Discharge", "NPL", "RMP Facility", "TSD Facility", "Ozone", "PM", "Demo. Index"),
              ylab="State Percentile",
              main=paste0("EJ Indexes relative to ", percentile," \n GIS Method: ",gis_method," \n Buffer: ", buffer," mile Distance"), las=2, cex.axis=.75)
    abline(h=threshold, col="Red", lty=5)
    dev.off()

    assign(paste0("indexes.",percentile),demo.df, envir = environment())
  }

  data.list <- sapply(objects(pattern="^indexes.", envir = environment()),get,  envir = environment(), simplify=F, USE.NAMES=T)
  return(data.list)
}
