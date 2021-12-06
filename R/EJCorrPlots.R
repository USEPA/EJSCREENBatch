#' EJ Correlation Plots
#'
#' Creates a correlation plot that quantifies two-way correlations between
#' EJ and demographic indicators.
#'
#' @param data
#' @param gis_method User specified method of creating buffers around areas of interest (intersect, centroid, intersection).
#' @param buffer Distance(s) used to create buffers.
#' @param threshold User specified threshold to represent potential concern. Default is 80%.
#' @param directory
#'
#' @return
#' @export
#'
#' @examples
EJCorrPlots <- function(data, gis_method, buffer, threshold, directory){
  ifelse(!dir.exists(file.path(directory,"plots")), dir.create(file.path(directory,"plots")), FALSE)

  exceed.threshold <- function(x) {
    ifelse(as.numeric(x)>threshold, 1, 0)
  }

  replace.zeros <- function(x) {
    ifelse(x==0, NA, x)
  }

  demo_indexes <- data %>%
    as.data.frame() %>%
    dplyr::select(ID, STATE_NAME, ST_ABBREV, starts_with("P_MINORPCT"),
                  starts_with("P_LWINCPCT"), starts_with("P_LESHSPCT"),
                  starts_with("P_LNGISPCT"), starts_with("P_UNDR5PCT"),
                  starts_with("P_OVR64PCT")) %>%
    rename(
      Minority_S=P_MINORPCT_state,
      Low_Income_S=P_LWINCPCT_state,
      Less_than_HS_Edu_S=P_LESHSPCT_state,
      Linguistic_Isolation_S=P_LNGISPCT_state,
      Age_Under_5_S=P_UNDR5PCT_state,
      Age_Over_64_S=P_OVR64PCT_state,

      Minority_US=P_MINORPCT_US,
      Low_Income_US=P_LWINCPCT_US,
      Less_than_HS_Edu_US=P_LESHSPCT_US,
      Linguistic_Isolation_US=P_LNGISPCT_US,
      Age_Under_5_US=P_UNDR5PCT_US,
      Age_Over_64_US=P_OVR64PCT_US) %>%
    # mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),as.numeric) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),exceed.threshold) %>%
    mutate(potential_issues_count_S = rowSums(dplyr::select(., -c("ID","STATE_NAME","ST_ABBREV", ends_with("_US")))),
           potential_issues_count_US = rowSums(dplyr::select(., -c("ID","STATE_NAME","ST_ABBREV", ends_with("_S"))))) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),replace.zeros)


  ej_indexes <- data %>%
    as.data.frame() %>%
    dplyr::select(ID, STATE_NAME, ST_ABBREV, starts_with("P_LDPNT"),
                  starts_with("P_DSLPM"), starts_with("P_CANCR"),
                  starts_with("P_RESP"), starts_with("P_PTRAF"), starts_with("P_PWDIS"),
                  starts_with("P_PNPL"), starts_with("P_PRMP"), starts_with("P_PTSDF"),
                  starts_with("P_OZONE"), starts_with("P_PM25"), starts_with("P_VULEOPCT")) %>%
    dplyr::select(-c(contains("D2"))) %>%
    rename(
      Lead_Paint_S=P_LDPNT_state,
      Diesel_PM_S=P_DSLPM_state,
      Air_Toxics_Cancer_Risk_S=P_CANCR_state,
      Air_Toxics_Respiratory_Hazard_S=P_RESP_state,
      Traffic_Proximity_S=P_PTRAF_state,
      Major_WW_Dischargers_S=P_PWDIS_state,
      Nation_Priorities_List_S=P_PNPL_state,
      Risk_Mgmt_Plan_Facilities_S=P_PRMP_state,
      Treatment_Storage_Disposal_Facilities_S=P_PTSDF_state,
      Ozone_Level_S=P_OZONE_state,
      PM_S=P_PM25_state,
      Demographic_Index_S=P_VULEOPCT_state,

      Lead_Paint_US=P_LDPNT_US,
      Diesel_PM_US=P_DSLPM_US,
      Air_Toxics_Cancer_Risk_US=P_CANCR_US,
      Air_Toxics_Respiratory_Hazard_US=P_RESP_US,
      Traffic_Proximity_US=P_PTRAF_US,
      Major_WW_Dischargers_US=P_PWDIS_US,
      Nation_Priorities_List_US=P_PNPL_US,
      Risk_Mgmt_Plan_Facilities_US=P_PRMP_US,
      Treatment_Storage_Disposal_Facilities_US=P_PTSDF_US,
      Ozone_Level_US=P_OZONE_US,
      PM_US=P_PM25_US,
      Demographic_Index_US=P_VULEOPCT_US,
      ) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),exceed.threshold) %>%
    mutate(potential_issues_count_S = rowSums(dplyr::select(., -c("ID","STATE_NAME","ST_ABBREV", ends_with("_US")))),
           potential_issues_count_US = rowSums(dplyr::select(., -c("ID","STATE_NAME","ST_ABBREV", ends_with("_S"))))) %>%
    mutate_at(vars(-c("ID","STATE_NAME","ST_ABBREV")),replace.zeros)




  geo_levels <- c("state","US")
  datasets <- c("demo_indexes", "ej_indexes")
  for(geo_level in geo_levels){
    for(dataset in datasets){

      if(geo_level=="state"){
        print("state")
        step1 <- get(dataset) %>%
          select(-c(tidyselect::ends_with("_US")))
      } else{
        print("US")
        step1 <- get(dataset) %>%
          select(-c(tidyselect::ends_with("_S")))
      }

      step1 <- step1 %>%
        rename_all(
          funs(
            stringr::str_replace_all(.,'_S','') %>%
            stringr::str_replace_all(.,'_US','')
          )
        )


      w <- which(step1==1,arr.ind=TRUE)
      step1[w] <- names(step1)[w[,"col"]]

      `%notin%` = Negate(`%in%`)
      colsNOT2paste <- c("ID","STATE_NAME","ST_ABBREV", "potential_issues_count","potential_issues_count", "overlap")
      step1$overlap <- do.call(paste, c(step1[, which(names(step1) %notin% colsNOT2paste)], sep=","))

      step1.1 <- step1[, which(names(step1) %notin% colsNOT2paste)]
      step1$overlap <- apply(step1.1, 1, function(x) toString(na.omit(x)))

      step2 <- step1 %>%
        dplyr::select(ID,STATE_NAME,ST_ABBREV,potential_issues_count, overlap) %>%
        mutate(potential_issues_count = ifelse(is.na(potential_issues_count),0, potential_issues_count),
               potential_issues_count = ifelse(potential_issues_count=="potential_issues_count",1,potential_issues_count)) %>%
        filter(!is.na(ID))

      assign(paste0("step2_",dataset),step2)

      tryCatch(
        {
          step3 <- step2 %>%
            dplyr::select(ID, overlap) %>%
            distinct() %>%
            filter(!is.na(ID)) %>%
            separate_rows(overlap,sep=", ") %>%
            mutate(value=1)  %>%
            filter(overlap!="")  %>%
            pivot_wider(values_from=value, names_from=overlap,  values_fill=0)  %>%
            dplyr::select(-ID) %>%
            cor()

          if(dataset=="demo_indexes"){
             txt.size=480
           } else {
             txt.size=900
           }

          jpeg(file=paste0(directory,"/plots/correlations_",dataset,"_gis_",gis_method,"_radius",buffer,"_",geo_level,".jpeg"), width = txt.size, height = txt.size)
          col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
          corrplot::corrplot(step3, method="color",
                   type="upper", order="hclust",
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col="black", tl.srt=45, #Text label color and rotation
                   diag=FALSE, # hide correlation coefficient on the principal diagonal,
                   tl.cex=1.25
          )
          dev.off()

        },
        warning=function(cond){
          message(paste0("Only ",length(unique(step2$ID))," block groups for ",length(unique(step2$shape_ID))," locations.
                         No correlation plots will be outputted"))
        })

    }
  }

  step2 <- step2_demo_indexes %>%
    rename(potential_issues_count_demo = potential_issues_count,
           overlap_demo = overlap) %>%
    left_join(step2_ej_indexes) %>%
    rename(potential_issues_count_ej = potential_issues_count,
           overlap_ej = overlap)


  return(step2)
}
