#' EJ Population Areal Apportionment
#'
#' Function to apportion census block group population using dasymetric raster data
#' when using intersection approach to creating buffers around areas of interest.
#' This function currently uses 1kmX1km raster data from NASA's Socioeconomic
#' Data and Applications Center (SEDAC)
#'
#'
#' @param ejscreen.bgs.data EJSCREEN data
#' @param facility_buff Polygons representing buffered areas of interest
#'
#' @return
#' @export
#'
#' @examples
areal_apportionment <- function(ejscreen.bgs.data, facility_buff, raster.pop.data){





  layers <- list.files(path="data/US Census Grid_SF2010_TIFF/", pattern= "((pop)).*\\.tif$", full.names = TRUE )
  raster_extract <- raster(layers) %>%
    projectRaster(crs="ESRI:102005")

  #Decennial Census Data from NASA's SEDAC
  #get pop for block group (intersect) and area covered by buffer (intersection)
  #Used to compute fraction necessary to weight EJ Indices
  methods=c("intersect", "intersection")
  for(method in methods){
    intermediate <- data.state.uspr %>%
        dplyr::select(ID, Shape) %>%
        {if(method=="intersection"){
            st_intersection(.,facility_buff) %>%
            group_by(Facility.Name) %>%
            mutate(count_bgs_radius = n_distinct(ID)) %>%
            group_by(ID) %>%
            mutate(count_fac_radius = n_distinct(Facility.Name))
        } else if(method=="intersect"){
            st_join(.,facility_buff, join=st_intersects) %>%
            filter(!is.na(shape_ID))
        }} %>%
        cbind(exact_extract(raster_extract, .,
                            c('sum'),
                            include_xy=F,
                            stack_apply=T,
                            full_colnames=T)) %>%
      rename(sum.uspop10.tif=starts_with('exact_'))
      assign(paste0("bgs.",method), intermediate)
  }

  #Summarizes EJ Indices for buffer, computes state and national averages, and national percentiles
  facility_level <- bgs.intersect %>%
    as.data.frame() %>%
    dplyr::select(shape_ID, ID, NPDES.Permit.Number, Facility.Name, starts_with("sum")) %>%
    left_join(bgs.intersection %>%
                as.data.frame() %>%
                dplyr::select(-c(starts_with("Shape"))) %>%
                dplyr::select(ID, NPDES.Permit.Number, Facility.Name, starts_with("sum"),starts_with("count_")) %>%
                rename(sum.uspop10.intersection = sum.uspop10.tif)) %>%
    mutate(fraction = as.numeric(sum.uspop10.intersection/sum.uspop10.tif*100, options(scipen=999))) %>%
    right_join(data.state.uspr, by=c("ID"="ID")) %>%
    group_by(shape_ID, Facility.Name)  %>%
    mutate(across(c(PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PNPL, PRMP, PRE1960PCT,
                    PTSDF, PWDIS, VULEOPCT),
                  list(~ifelse(!is.na(ID) & is.na(Facility.Name), ., (sum(fraction*ACSTOTPOP*., na.rm=T)/sum(fraction*ACSTOTPOP,na.rm=T)))  )  ,
                  .names="raw_E_{.col}"),
           across(c(MINORPCT, LOWINCPCT, UNDER5PCT,
                    LESSHSPCT, OVER64PCT, LINGISOPCT),
                  list(~ifelse(!is.na(ID) & is.na(Facility.Name), ., (sum(fraction*ACSTOTPOP*., na.rm=T)/sum(fraction*ACSTOTPOP,na.rm=T)))  )  ,
                  .names="raw_D_{.col}")) %>%
    group_by(STATE_NAME) %>%
    mutate(across(c(PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PNPL, PRMP, PRE1960PCT,
                       PTSDF, PWDIS, VULEOPCT),
                     list(~mean(., na.rm=T)),
                     .names="S_E_{.col}"),

           across(c(MINORPCT, LOWINCPCT, UNDER5PCT,
                    LESSHSPCT, OVER64PCT, LINGISOPCT),
                  list(~mean(., na.rm=T)),
                  .names="S_D_{.col}")) %>%
    ungroup() %>%
    mutate(across(c(PM25, OZONE, DSLPM, CANCER, RESP, PTRAF, PNPL, PRMP, PRE1960PCT,
                       PTSDF, PWDIS, VULEOPCT),
                     list(~(mean(., na.rm=T))),
                     .names="N_E_{.col}"),


           across(c(MINORPCT, LOWINCPCT, UNDER5PCT,
                    LESSHSPCT, OVER64PCT, LINGISOPCT),
                  list(~(mean(., na.rm=T))),
                  .names="N_D_{.col}"))  %>%
    dplyr::select(shape_ID, ID, STATE_NAME,Facility.Name, starts_with("raw_"), starts_with("S_E_"), starts_with("N_E_")) %>%
    mutate(ID = ifelse(!is.na(Facility.Name),"1",ID)) %>%
    distinct() %>%
    ungroup() %>%
    mutate(across(c(starts_with("raw_")),
                  list(~round(ecdf(data.state.uspr %>%
                                     as.data.frame() %>%
                                     dplyr::select(as.name(str_replace(cur_column(), c("raw_E_|raw_D_") ,""))) %>%
                                     unlist() %>%
                                     as.numeric())(.)*100
                              ,0)),
                  # list(~ntile(., 100)),
                  .names="N_{.col}_per"))  %>%
    rename_with(~ sub("N_raw_", "N_", .x), everything()) %>%
    filter(!is.na(Facility.Name))



  states <- facility_level %>%
    dplyr::select(STATE_NAME) %>%
    filter(!(STATE_NAME=="Puerto Rico")) %>%
    unique() %>%
    unlist() %>%
    as.list()


  #computes state percentiles--looping to make sure distribution used to get percentile is by state
  #Can parallelize for speed
  facility_level_estimates <- do.call(rbind,lapply(states, function(x){
    iterm <- facility_level %>%
      filter(STATE_NAME==x) %>%
      filter(!is.na(Facility.Name))  %>%
      mutate(across(c(starts_with("raw_")),
                    list(~round(ecdf(na.omit(data.state.uspr %>%
                                       as.data.frame() %>%
                                       filter(STATE_NAME==x) %>%
                                       dplyr::select(as.name(str_replace(cur_column(), c("raw_E_|raw_D_"),""))) %>%
                                       unlist() %>%
                                       as.numeric()))(.)*100
                                ,0)),
                    .names="S_{.col}_per"))  %>%
      rename_with(~ sub("S_raw_", "S_", .x), everything())
  }))

}
