% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EJfunction.R
\name{EJfunction}
\alias{EJfunction}
\title{EJ tool}
\usage{
EJfunction(
  data_type,
  facility_data,
  input_type = NULL,
  gis_option = NULL,
  buff_dist = NULL,
  threshold = NULL,
  state = NULL,
  ds_mode = NULL,
  ds_dist = NULL,
  produce_ancillary_tables = NULL,
  heat_table_type = NULL,
  heat_table_geog_lvl = NULL,
  heat_table_keepid = NULL,
  heat_table_topN = NULL,
  rank_type = NULL,
  rank_geography_type = NULL,
  rank_count = NULL,
  maps_perc_geog = "US",
  input_name = NULL,
  attains = NULL,
  raster_data = "data/US Census Grid_SF2010_TIFF"
)
}
\arguments{
\item{data_type}{Required. Either "landbased" or "waterbased"}

\item{facility_data}{Required.}

\item{input_type}{Required if data_type == "waterbased". Input must be "sf" object or list of catchments (ComIDs)}

\item{gis_option}{User specified method of creating buffers around areas of interest (intersect, centroid, intersection). Default is intersection.}

\item{buff_dist}{Distance(s) used to create buffers (miles). Default is 1, 3, and 5 miles.}

\item{threshold}{User specified threshold to represent potential concern. Default is 80\%.}

\item{state}{User can restrict screening to particular states. Default is to screen for entire contiguous US.}

\item{ds_mode}{Set Upstream/downstream option for water-based screening. Default is downstream.}

\item{ds_dist}{Set distance to examine areas upstream/downstream for water-based screening. Default is 50 miles.}

\item{produce_ancillary_tables}{Option to return secondary tables/figures. Default is FALSE.}

\item{heat_table_type}{Locations to include in Heat Table. Options include "all", "single", or "topn". If "topn", user must also provide a value for parameter heat_table_topN.}

\item{heat_table_geog_lvl}{State or US.}

\item{heat_table_keepid}{shape_ID to keep if type = 'single'}

\item{heat_table_topN}{Number of locations to include in Heat table.}

\item{rank_type}{Ranking table type--"location" or "cbg".}

\item{rank_geography_type}{State or US.}

\item{rank_count}{Number of locations to include in ranking table.}

\item{input_name}{Vector of names for facilities}

\item{attains}{Option to pull data from the attains database. Default is FALSE.}

\item{raster_data}{Path to dasymetric raster data. Recommend using 1kmX1km raster
data from NASA's Socioeconomic Data and Applications Center (SEDAC)}
}
\value{

}
\description{
Main function that performs screening (land and water-based).
Input must be an SF object! User must make this transformation.
}
\examples{

#===============================================================================
#=====================FEATURE 1: LAND-BASED ANALYSIS============================
#===============================================================================
# Demonstration using meat and poultry plant facility information
# How it works:
# Provide lat lons to tool and specify data type (facility_latlons)
# options to consider
# 1) gis_option. Three options available: intersect, centroid, intersection.
#    Instersection is default.
# 2) buff_dist. Radius to use around facilities
# 3) Threshold for EJ consideration. EJScreen uses 80 as default.
# 4) states. Can restrict analysis to specific states.
# bring in data for contiguous US
a1 <- EJfunction(data_type="landbased", facility_data = facilities, gis_option="centroid",
                buff_dist = 5)

#===============================================================================
#=======================FEATURE 2: WATER-BASED ANALYSIS=========================
#===============================================================================
# Demonstration using random set of catchments
# How it works:
# Provide COMIDs to tool and specify data type (water_catchments)
# options to consider
# 1) ds.us.mode. Upstream/downstream option
#    mode DD is ds w/ diversions, DM is ds mainstem, UT is us w/ tributaries, UM is us mainstem.
#    Mode DD is default.
# 2) ds.us.dist. Upstream/downstream distance. 50miles is default.
# 3) buff.dist. Buffer distance around catchments in miles. 1 mile is default.
# 4) Attains. Call attains API for data? (T/F). Default is False

c <- EJfunction(data_type="waterbased", facility_data=facilities,
                input_type = 'sf', attains = F)

}
