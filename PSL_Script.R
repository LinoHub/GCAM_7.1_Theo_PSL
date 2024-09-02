# Demeter land use outputs added together 

# Set up work space ----

# Clear Workspace
# rm(list = ls())

# Clear Console
# cat("\014") 

# RGCAM
# install.packages("devtools")
# devtools::install_github('JGCRI/rgcam', build_vignettes=TRUE)

# The IUCN Redist of Threatened Species Database (https://www.iucnredlist.org/)
# install.packages("rredlist", repos = "https://dev.ropensci.org")
# devtools::install_github("ropensci/rnaturalearth")
# install.packages("rnaturalearth", dependencies = TRUE)
# install.packages("class", dependencies = TRUE)
# install.packages("reticulate")

# Load packages ----

library(rgcam)
library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(s2)
# library(mapview)
library(rredlist)
library(tibble)
library(ggplot2)
library(mapdata)
library(mapproj)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringi)
library(purrr)
library(raster)
library(ncdf4)
library(shapefiles)
library(xlsx)
library(openxlsx)
library(terra)
library(reticulate)

# Create and load project ----
setwd("C:/GCAM/GCAM_7.0_Theo_2/")
db_path <- "C:/GCAM/GCAM_7.0_Theo_2/output"

# Update folder name (protected or unprotected)
db_name <- "database_basexdb"
query_name <- "DetailedLU_query.xml"

# Update scenario name
prj_name <- "Forest_Unprotected.dat"
scen_name <- c("Reference", "Reference_0.9", "Reference_Driver")

# Upload basin mapping
basin.id <- read_excel("basin_to_country_mapping.xlsx",col_names = TRUE)

# Extract scenario data from database
conn <- localDBConn(db_path, db_name)

# Extract information specified by XML query for each scenario
proj <- addScenario(conn, prj_name, scen_name, query_name)


# Format GCAM land use outputs to fit as Demeter inputs ----
det.LU <- getQuery(proj, "detailed land allocation") %>% separate(landleaf, into = c("landclass", "GLU_name", "irrtype", "hiORlo"), sep = "_") %>% 
          mutate(landclass = case_when (!is.na(irrtype) ~ paste0(landclass,irrtype, hiORlo),TRUE ~ landclass)) %>%
          merge(basin.id,by="GLU_name") %>% dplyr::select(region, landclass, GCAM_basin_ID, year, value, scenario)  %>% 
          rename("metric_id"="GCAM_basin_ID") %>% spread(year, value) %>% dplyr::select(-"1975")

# Get unique scenario names
scenario_names <- unique(det.LU$scenario)

# Loop to create one file per scenario in "input/projected" demeter folder and configuration files
for (scenario_name in scenario_names) {
  # Filter the dataframe for the current scenario
  filtered_df <- det.LU[det.LU$scenario == scenario_name, ]
  
  # Define the file paths for saving the filtered dataframe and the configuration file
  file_path <- paste0("C:/GCAM/GCAM_7.0_Theo_2/demeter/demeter/GCAM_Demeter_protection_scenario/inputs/projected/Scenario_", scenario_name, ".csv")
  config_path <- paste0("C:/GCAM/GCAM_7.0_Theo_2/demeter/demeter/GCAM_Demeter_protection_scenario/config_files/Scenario_", scenario_name, ".ini")
  
  # Config file parameters
  projected_file <- paste0("Scenario_", scenario_name, ".csv")
  
  # Create the content for the config file
  config_file <- paste0(
    "[STRUCTURE]\n",
    "run_dir =                       C:/GCAM/GCAM_7.0_Theo_2/demeter/demeter/GCAM_Demeter_protection_scenario\n",
    "in_dir =                        inputs\n",
    "out_dir =                       outputs\n\n",
    "[INPUTS]\n",
    "allocation_dir =                allocation\n",
    "observed_dir =                  observed\n",
    "constraints_dir =               constraints\n",
    "projected_dir =                 projected\n\n",
    "[[ALLOCATION]]\n",
    "spatial_allocation_file =       gcam_regbasin_moirai_v3_type5_5arcmin_observed_alloc.csv\n",
    "gcam_allocation_file =          gcam_regbasin_moirai_v3_type5_5arcmin_projected_alloc.csv\n",
    "kernel_allocation_file =        gcam_regbasin_moirai_v3_type5_5arcmin_kernel_weighting.csv\n",
    "transition_order_file =         gcam_regbasin_moirai_v3_type5_5arcmin_transition_alloc.csv\n",
    "treatment_order_file =          gcam_regbasin_moirai_v3_type5_5arcmin_order_alloc.csv\n",
    "constraints_file =              gcam_regbasin_moirai_v3_type5_5arcmin_constraint_alloc.csv\n\n",
    "[[OBSERVED]]\n",
    "observed_lu_file =              baselayer_GCAM6_WGS84_5arcmin_2022_HighProt.zip\n\n",
    "[[PROJECTED]]\n",
    "projected_lu_file =             ", projected_file, "\n\n",
    "[[MAPPING]]\n",
    "region_mapping_file =           gcam_regions_32.csv\n",
    "basin_mapping_file =            gcam_basin_lookup.csv\n\n",
    "[PARAMS]\n",
    "# scenario name\n",
    "scenario =                      ", scenario_name, "\n\n",
    "# run description\n",
    "run_desc =                      run_description_null\n\n",
    "# spatial base layer id field name\n",
    "observed_id_field =             fid\n\n",
    "# first year to process\n",
    "start_year =                    2010\n\n",
    "# last year to process\n",
    "end_year =                      2020\n\n",
    "# enter 1 to use non-kernel density constraints, 0 to ignore non-kernel density constraints\n",
    "use_constraints =               1\n\n",
    "# the spatial resolution of the observed spatial data layer in decimal degrees\n",
    "spatial_resolution =            0.0833333\n\n",
    "# error tolerance in km2 for PFT area change not completed\n",
    "errortol =                      0.001\n\n",
    "# time step in years\n",
    "timestep =                      5\n\n",
    "# factor to multiply the projected land allocation by\n",
    "proj_factor =                   1000\n\n",
    "# from 0 to 1; ideal fraction of LUC that will occur during intensification, the remainder will be expansion\n",
    "intensification_ratio =         0.8\n\n",
    "# activates the stochastic selection of grid cells for expansion of any PFT\n",
    "stochastic_expansion =          1\n\n",
    "# threshold above which grid cells are selected to receive a given land type expansion; between 0 and 1, where 0 is all\n",
    "#     land cells can receive expansion and set to 1 only the grid cell with the maximum likelihood will expand.  For\n",
    "#     a 0.75 setting, only grid cells with a likelihood >= 0.75 x max_likelihood are selected.\n",
    "selection_threshold =           0.75\n\n",
    "# radius in grid cells to use when computing the kernel density; larger is smoother but will increase run-time\n",
    "kernel_distance =               30\n\n",
    "# create kernel density maps; 1 is True\n",
    "map_kernels =                   1\n\n",
    "# create land change maps per time step per land class\n",
    "map_luc_pft =                   0\n\n",
    "# create land change maps for each intensification and expansion step\n",
    "map_luc_steps =                 0\n\n",
    "# creates maps of land transitions for each time step\n",
    "map_transitions =               0\n\n",
    "# years to save data for, default is all; otherwise a semicolon delimited string e.g, 2005;2050\n",
    "target_years_output =           all\n\n",
    "# save tabular spatial landcover as CSV; define tabular_units below (default sqkm)\n",
    "save_tabular =                  0\n\n",
    "# untis to output tabular data in (sqkm or fraction)\n",
    "tabular_units =                 fraction\n\n",
    "# exports CSV files of land transitions for each time step in km2\n",
    "save_transitions =              0\n\n",
    "# create a NetCDF file of land cover percent for each year by grid cell containing each land class\n",
    "save_netcdf_yr =                1\n"
  )
  
  # Save the filtered dataframe to CSV as "projected" Demeter input 
  write.csv(filtered_df, file = file_path, row.names = FALSE)
  write(config_file, file = config_path)

# Import Python module and Run Demeter (approx. 50 min per scenario) ----
demeter <- reticulate::import("demeter")
sys <- reticulate::import("sys")
config_path = "C:\\GCAM\\GCAM_7.0_Theo_2\\demeter\\demeter\\GCAM_demeter_protection_scenario\\config_files\\"
config_name = paste0("Scenario_", scenario_name, ".ini")
config_file = paste0(config_path, config_name)
demeter$run_model(config_file=config_file, write_outputs=TRUE)

}

# Extract surfaces by land use type from the netCDF files -----
year <- c('2020','2050')
output_year <- data.frame(year)
areas_land_types <- read.csv("C:/GCAM/GCAM_7.0_Theo_2/demeter/demeter/GCAM_demeter_protection_scenario/Coordinates.csv")

# for (scen_name in scenario_names) { 
# scen_name = "gcam6"

# PENDING, problem of telling the file with the hour and date, not just scenario name ----
for (scenario_name in scenario_names) {
  # Create path for specific scenario
  dir_demeter <- "C:/GCAM/GCAM_7.0_Theo_2/demeter/demeter/GCAM_demeter_protection_scenario/outputs"
  # Update output scenario name
  output_scenario <- "gcam6_2024-04-17_12h11m36s"
  netcdffolder <- "spatial_landcover_netcdf"
  nc_file <- paste0("_demeter_gcam7_",scen_name,"_",output_year[i,1],".nc")
  
  NetCDFfiles_path <- file.path(dir_demeter,output_scenario,netcdffolder,nc_file)

  # To incorporate NetCDF files. Include raster and  convert to dataframe
  area_shrubland <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT0")))
  area_shrubland<- area_shrubland %>% rename ("shr"="shrubland")
  area_protected.shrubland <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT50")))
  area_protected.shrubland<- area_protected.shrubland %>% rename ("ptshrub"="protected_shrubland")
  area_grassland <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT1")))
  area_grassland<- area_grassland %>% rename ("grass"="grassland")
  area_protected.grassland <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT51")))
  area_protected.grassland<- area_protected.grassland %>% rename ("ptgrass"="protected_grassland")
  area_tundra <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT6")))
  area_forest <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT2")))
  area_protected.forest <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT52")))
  area_protected.forest<- area_protected.forest %>% rename ("ptforest"="protected_forest")
  area_pasture <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT4")))
  area_protected.pasture <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT53")))
  area_protected.pasture<- area_protected.pasture %>% rename ("ptpast"="protected_pasture")
  area_rockicedesert <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT5")))
  area_rockicedesert<- area_rockicedesert %>% rename ("rid"="rockicedesert")
  area_urban <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT7")))
  area_urban<- area_urban %>% rename ("urban"="urbanland")
  
  # Crops
  area_cornc4_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT8")))
  area_cornc4_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT9")))
  area_fibercrop_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT11")))
  area_fruit_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT12")))
  area_fruit_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT13")))
  area_legumes_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT14")))
  area_legumes_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT15")))
  area_misc_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT16")))
  area_misc_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT17")))
  area_misctree_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT18")))
  area_misctree_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT19")))
  area_nuts_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT20")))
  area_nuts_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT21")))
  area_oil_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT22")))
  area_oil_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT23")))
  area_oiltree_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT24")))
  area_oiltree_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT25")))
  area_oilpalmtree_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT26")))
  area_oilpalmtree_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT27")))
  area_othergrain_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT28")))
  area_othergrain_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT29")))
  area_othergrainc4_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT30")))
  area_othergrainc4_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT31")))
  area_rice_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT32")))
  area_rice_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT33")))
  area_roottuber_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT34")))
  area_roottuber_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT35")))
  area_soybean_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT36")))
  area_soybean_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT37")))
  area_sugar_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT38")))
  area_sugar_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT39")))
  area_sugarc4_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT40")))
  area_sugarc4_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT41")))
  area_veg_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT42")))
  area_veg_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT43")))
  area_wheat_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT44")))
  area_wheat_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT45")))
  area_biomassgrass_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT46")))
  area_biomassgrass_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT47")))
  area_biomassgrasstree_irr <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT48")))
  area_biomassgrasstree_rf <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT49")))
  area_otherarableland <- as.data.frame(rasterToPoints(raster(NetCDFfiles_path, varname = "PFT3")))
  
  # Merge all the crops separating the irrigated from the rainfed ones
  area_crops_irr <- merge (area_cornc4_irr,area_fibercrop_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_fruit_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_legumes_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_misc_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_misctree_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_nuts_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_oil_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_oiltree_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_oilpalmtree_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_othergrain_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_othergrainc4_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_rice_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_roottuber_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_soybean_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_sugar_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_sugarc4_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_veg_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_wheat_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_biomassgrass_irr,by=c("x","y"))
  area_crops_irr <- merge (area_crops_irr,area_biomassgrasstree_irr,by=c("x","y"))
  area_crops_irr <- area_crops_irr %>% mutate(Row_Sums = rowSums(across(everything()))) %>% 
                                       mutate(irrcrop= Row_Sums - x - y) %>% dplyr::select(x,y,irrcrop)
  
  
  area_crops_rf <- merge (area_cornc4_rf,area_misc_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_misctree_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_nuts_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_oil_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_oiltree_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_oilpalmtree_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_othergrain_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_othergrainc4_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_rice_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_roottuber_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_soybean_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_sugar_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_sugarc4_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_veg_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_wheat_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_biomassgrass_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_biomassgrasstree_rf,by=c("x","y"))
  area_crops_rf <- merge (area_crops_rf,area_otherarableland,by=c("x","y"))
  area_crops_rf <- area_crops_rf %>% mutate(Row_Sums = rowSums(across(everything()))) %>% 
                                     mutate(rfcrop= Row_Sums - x - y) %>% dplyr::select(x,y,rfcrop)
  
  # Merge all the land type areas together
  areas_land_types <- merge(areas_land_types,area_shrubland,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_protected.shrubland,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_grassland,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_protected.grassland,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_tundra,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_forest,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_protected.forest,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_pasture,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_protected.pasture,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_crops_irr,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_crops_rf,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_rockicedesert,by=c("x","y"))
  areas_land_types <- merge(areas_land_types,area_urban,by=c("x","y"))
  
  
# Geography parameters ----
areas_land_types <- areas_land_types %>% rename ("longitude"="x") %>% rename ("latitude"="y")
areas_land_types_shapefile <- areas_land_types
coordinates(areas_land_types_shapefile)=~longitude+latitude
proj4string(areas_land_types_shapefile)<- CRS("+proj=longlat +datum=WGS84")

# Save processed Demeter outputs csv and shp ----
shapefile(areas_land_types_shapefile, paste0("C:/GCAM/GCAM_7.0_Theo_2/QGIS input files/",scenario_name,".shp"))
write.csv(areas_land_types,paste0("C:/GCAM/GCAM_7.0_Theo_2/Processed Demeter outputs/",scenario_name,".csv"),row.names=TRUE)

  }
# }


# # (C) Calculate changes between 2050 and 2020 ----
# LUChange <- areas_land_types %>%
#                 mutate (shrub.diff=(shr.y+ptshrub.y)-(shr.x+ptshrub.x),
#                         grass.diff=(grass.y+ptgrass.y)-(grass.x+ptgrass.x),
#                         forest.diff=(forest.y+ptforest.y)-(forest.x+ptforest.x),
#                         pasture.diff=(pasture.y+ptpast.y)-(pasture.x+ptpast.x),
#                         irrcrop.diff=rfcrop.y-rfcrop.x,
#                         rfcrop.diff=rfcrop.y-rfcrop.x)
# 
# # Transform to simple feature collections
# LUChange <- st_as_sf(LUChange,coords = c('longitude','latitude'), crs=4326)
# 
# ## Land use changes spatial distribution plot
# 
# # (C) Plots changes 2020-2050 ----
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # # Forest Change
# ForestChange <- LUChange %>% filter(forest.diff < -1 | forest.diff > 1)
# ggplot(data = world) + geom_sf(data = world, color = "gray80", fill = "gray80") + geom_sf(data = ForestChange, aes(color = forest.diff), lwd=0) +
#    scale_colour_gradient2 (low = "darkred", mid = "gray80", high = "darkgreen", midpoint = 0) + coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#    labs(color='Forest area \nchange [km2]') + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
#     legend.title = element_text(family = "Calibri Light", face = "bold", size = 10), legend.text = element_text(family = "Calibri Light", size = 8),
#     legend.position = c(0.9, 0.8)) + guides(colour = guide_colourbar(barwidth = 0.9, barheight = 5, ticks = FALSE, title.vjust = 4))
# 
# 
# # # Shrub area Change
# ShrubChange <- LUChange %>% filter(shrub.diff < -2 | shrub.diff > 2)
# ggplot(data = world) + geom_sf(data = world, color = "gray80", fill = "gray80") + geom_sf(data = ShrubChange, aes(color = forest.diff), lwd=0) +
#   scale_colour_gradient2 (low = "darkred", mid = "gray80", high = "darkgreen", midpoint = 0) + coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   labs(color='Shrub area \nchange [km2]') + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
#   legend.title = element_text(family = "Calibri Light", face = "bold", size = 10), legend.text = element_text(family = "Calibri Light", size = 8),
#   legend.position = c(0.9, 0.8)) + guides(colour = guide_colourbar(barwidth = 0.9, barheight = 5, ticks = FALSE, title.vjust = 4))
# 
# # # Grass area Change
# GrassChange <- LUChange %>% filter(grass.diff < -2 | grass.diff > 2)
# ggplot(data = world) + geom_sf(data = world, color = "gray80", fill = "gray80") + geom_sf(data = GrassChange, aes(color = forest.diff), lwd=0) +
#   scale_colour_gradient2 (low = "darkred", mid = "gray80", high = "darkgreen", midpoint = 0) + coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   labs(color='Grass area \nchange [km2]') + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
#   legend.title = element_text(family = "Calibri Light", face = "bold", size = 10), legend.text = element_text(family = "Calibri Light", size = 8),
#   legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 5, ticks = FALSE, title.vjust = 4))
# 
# # # Pasture area Change
# PastureChange <- LUChange %>% filter(pasture.diff < -2 | pasture.diff > 2)
# ggplot(data = world) + geom_sf(data = world, color = "gray80", fill = "gray80") + geom_sf(data = PastureChange, aes(color = pasture.diff), lwd=0) +
#   scale_colour_gradient2 (low = "darkred", mid = "gray80", high = "darkgreen", midpoint = 0) + coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   labs(color='Pasture area \nchange [km2]')  + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
#   legend.title = element_text(family = "Calibri Light", face = "bold", size = 10), legend.text = element_text(family = "Calibri Light", size = 8),
#   legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 5, ticks = FALSE, title.vjust = 4))
# 
# # # Irrigated Crop area Change
# IrrCropChange <- LUChange %>% filter(irrcrop.diff < -2 | irrcrop.diff > 2)
# ggplot(data = world) + geom_sf(data = world, color = "gray80", fill = "gray80") + geom_sf(data = IrrCropChange, aes(color = irrcrop.diff), lwd=0) +
#   scale_colour_gradient2 (low = "darkred", mid = "gray80", high = "darkgreen", midpoint = 0) + coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   labs(color='Irrigated crop area \nchange [km2]') + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
#   legend.title = element_text(family = "Calibri Light", face = "bold", size = 10), legend.text = element_text(family = "Calibri Light", size = 8),
#   legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 5, ticks = FALSE, title.vjust = 4))
# 
# # # Rainfed Crop area Change
# RfCropChange <- LUChange %>% filter(rfcrop.diff < -2 | rfcrop.diff > 2)
# ggplot(data = world) + geom_sf(data = world, color = "gray80", fill = "gray80") + geom_sf(data = RfCropChange, aes(color = rfcrop.diff), lwd=0) +
#   scale_colour_gradient2 (low = "darkred", mid = "gray80", high = "darkgreen", midpoint = 0) + coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   labs(color='Rainfed crop area \nchange [km2]') + theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"),
#   legend.title = element_text(family = "Calibri Light", face = "bold", size = 10), legend.text = element_text(family = "Calibri Light", size = 8),
#   legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 5, ticks = FALSE, title.vjust = 4))

# Ecoregions aggregation processing into R -----

# Load Ecoregions shp 
setwd("C:/GCAM/GCAM_7.0_Theo_2/Ecoregions")
ecoregions_shp <- st_read("wwf_terr_ecos.shp")

# Load the Demeter outputs (if necessary), otherwise it is here as "areas_land_types_shapefile" 
setwd("C:/GCAM/GCAM_7.0_Theo_2/QGIS input files")

# HERE MAKE LOOP PER SCENARIO ----
areas_land_types_shp <- st_read("gcam6.shp")

unique(st_geometry_type(areas_land_types_shp$geometry))

# Check the classes
class(areas_land_types_shp)
crs(areas_land_types_shp)
areas_land_types_shp = st_transform(areas_land_types_shp, crs=4326)

class(ecoregions_shp)
crs(ecoregions_shp)
# Transform an object of SP class into SF (if necessary)
# areas_land_types_sf = as(areas_land_types_shapefile, "sf")
# class(areas_land_types_sf)

# Check the geometries
ecoregions_valid <- st_is_valid(ecoregions_shp)
areas_land_types_valid <- st_is_valid(areas_land_types_shp)

# Identify invalid geometries
invalid_ecoregions <- ecoregions_shp[!ecoregions_valid, ]
invalid_areas_land_types <- areas_land_types_shp[!areas_land_types_valid, ]

# Fix the geometries 
ecoregions_shp = st_make_valid(ecoregions_shp)

# Simplify ecoregions
ecoregions_id <- ecoregions_shp %>% 
  dplyr::select(all_of(c("OBJECTID", "eco_code")))
length(unique(ecoregions_id$OBJECTID))

# Perform test on subset with 100k lines
# joined_shp_id = st_join(areas_land_types_shp[1:100000,], ecoregions_id, join = st_intersects)

# Join attributes by location 
joined_shp_id = st_join(areas_land_types_shp, ecoregions_id, join = st_intersects) # way too long, need to simply both with index 
length(unique(joined_shp_id_3$OBJECTID))

# Aggregate per land use, dropping geometry
joined_shp_agg <- joined_shp_id %>%
  st_drop_geometry() %>%
  group_by(OBJECTID) %>% 
  summarise(shr.x = sum(shr.x),
            ptshrub.x = sum(ptshrub.x),
            grass.x = sum(grass.x),
            ptgrass.x = sum(ptgrass.x),
            tundra.x = sum(tundra.x),
            forest.x = sum(forest.x),
            ptforest.x = sum(ptforest.x),
            pasture.x = sum(pasture.x),
            ptpast.x = sum(ptpast.x),
            irrcrop.x = sum(irrcrop.x),
            rfcrop.x = sum(rfcrop.x),
            rid.x = sum(rid.x),
            urban.x = sum(urban.x),
            shr.y = sum(shr.y),
            ptshrub.y = sum(ptshrub.y),
            grass.y = sum(grass.y),
            ptgrass.y = sum(ptgrass.y),
            tundra.y = sum(tundra.y),
            forest.y = sum(forest.y),
            ptforest.y = sum(ptforest.y),
            pasture.y = sum(pasture.y),
            ptpast.y = sum(ptpast.y),
            irrcrop.y = sum(irrcrop.y),
            rfcrop.y = sum(rfcrop.y),
            rid.y = sum(rid.y),
            urban.y = sum(urban.y)) %>% 
  ungroup() %>% 
  full_join(ecoregions_shp[,c(1,18)]) %>%
  rename(ECOREGION_CODE = eco_code) %>%
  mutate_at(vars(-ECOREGION_CODE), ~replace(., is.na(.), 0))

joined_shp_agg

# Check the final sum -- Problem: There are many points without polygons
joined_shp_full <- joined_shp_agg %>%
  summarise(shr.x = sum(shr.x),
            ptshrub.x = sum(ptshrub.x),
            grass.x = sum(grass.x),
            ptgrass.x = sum(ptgrass.x),
            tundra.x = sum(tundra.x),
            forest.x = sum(forest.x),
            ptforest.x = sum(ptforest.x),
            pasture.x = sum(pasture.x),
            ptpast.x = sum(ptpast.x),
            irrcrop.x = sum(irrcrop.x),
            rfcrop.x = sum(rfcrop.x),
            rid.x = sum(rid.x),
            urban.x = sum(urban.x),
            shr.y = sum(shr.y),
            ptshrub.y = sum(ptshrub.y),
            grass.y = sum(grass.y),
            ptgrass.y = sum(ptgrass.y),
            tundra.y = sum(tundra.y),
            forest.y = sum(forest.y),
            ptforest.y = sum(ptforest.y),
            pasture.y = sum(pasture.y),
            ptpast.y = sum(ptpast.y),
            irrcrop.y = sum(irrcrop.y),
            rfcrop.y = sum(rfcrop.y),
            rid.y = sum(rid.y),
            urban.y = sum(urban.y))

# Export final files as csv ----
# Read file with the Ecoregion names from Chaudhary and Brookes (2018)
setwd("C:/GCAM/GCAM_7.0_Theo_2/QGIS output files")
ecoregions_ID <- read.xlsx("Ecoregion ID.xlsx",1)

# Incorporate the ecoregions names from Chaudhary and Brookes (2018) to the Results matrix obtained from QGIS with data on land use type area changes over time
# Conversion function from square km to square meters
sqm <- function(x, na.rm = FALSE) (x*1000000)

joined_shp_extended <- joined_shp_agg %>% 
  merge(ecoregions_ID,by="ECOREGION_CODE") %>%  
  relocate(ECO_NAME, .after=ECOREGION_CODE) %>%  
  group_by(ECOREGION_CODE,ECO_NAME) %>%
  summarise(across(where(is.numeric), sum)) %>% 
  dplyr::select(-OBJECTID) %>% 
  mutate_if(is.numeric, sqm, na.rm = FALSE) %>% 
  ungroup() %>% 
  as.data.frame()

scen_name_shp = "scenario_name"

# joined_shp_extended[is.na(joined_shp_extended)] <- 0
# scen_outputfile_extended <- as.data.frame(scen_outputfile_extended) %>% dplyr::select(1:28)

write.xlsx(joined_shp_extended,paste0("C:/GCAM/GCAM_7.0_Theo_2/Output data by ecoregion/",scen_name_shp,"_R.xlsx") ,
           overwrite = TRUE, rowNames=TRUE, colNames=TRUE)



# JORGE PREVIOUS ----

# QGIS processing to aggregate species habitat data inside ecoregion perimeters /QGIS ----
# 1. To import shape file: Layer > Add Layer > Add Vector Layer. Search and select the Ecoregions shape file (.shp). Click on "Add", then "Close" and rename to "Ecoregions".
# 2. Fix the geometry of the species habitat layer by looking for the "Fix geometries" option in the "Processing Toolbox" search bar and selecting 
#    the "Ecoregions" layer as "Input layer"
# 3. Create a spatial index for the Ecoregion layer using the "Create Spatial Index" tool (it can be found in the "Processing Toolbox" search box on the right).
# 4. Go to Settings > Options > Processing and under "General" make sure "Skip (ignore) features with invalid geometries" is chosen 
#    as Value for "Invalid features filtering".
# 5. To import shape file: Layer > Add Layer > Add Vector Layer. Select the specific scenario shape file (e.g. Baseline_2c_protected.shp).
# 6. Create a spatial index for the specific scenario layer (e.g. Baseline_2c_protected") using the "Create Spatial Index" tool (in the "Processing Toolbox" search box on the right).
# 7. Do a "Join attribute by location (summary)" with the Ecoregion layer as "Input layer" and the specific scenario layer as "Join layer" with "intersects" as 
#    "Geometric predicate", checking all the corresponding ..._x and ..._y in the "Fields to summarise" option and "sum" in the "Summaries to calculate" option.
# 8. To export as shape file: Right click on the layer: Export > "Save Feature As..." with Format -> ESRI Shapefile


# Import treated files from QGIS into a Results Matrix -----
setwd("C:/GCAM/GCAM_7.0_Theo_2/QGIS output files")

# Read file with the Ecoregion names from Chaudhary and Brookes (2018)
ecoregions_ID <- read.xlsx("Ecoregion ID.xlsx",1)

# Read the QGIS output shape file of the specific scenario + Update with the specific scenario file
scen_name_shp <- "Joined_Ecoregions_Demeter"
scen_outputfile_shp <- paste0(scen_name_shp,".shp")
scen_outputfile <- st_read(scen_outputfile_shp) %>% rename(ECOREGION_CODE = eco_code)

# Incorporate the ecoregions names from Chaudhary and Brookes (2018) to the Results matrix obtained from QGIS with data on land use type area changes over time
# Conversion function from square km to square meters
sqm <- function(x, na.rm = FALSE) (x*1000000)
# ORIGINAL
# scen_outputfile_extended <- scen_outputfile %>% merge(ecoregions_ID,by="ECOREGION_CODE") %>% dplyr::select(1,22:48) %>%
#                             rename(ECO_NAME = ECO_NAME.y) %>% relocate(ECO_NAME, .after=ECOREGION_CODE) %>%  group_by(ECOREGION_CODE,ECO_NAME) %>%
#                             summarise(across(where(is.numeric), sum)) %>% mutate_if(is.numeric, sqm, na.rm = FALSE) %>% ungroup ()

scen_outputfile_extended <- scen_outputfile %>% merge(ecoregions_ID,by="ECOREGION_CODE") %>% dplyr::select(1,23:49) %>%
                            rename(ECO_NAME = ECO_NAME.y) %>% relocate(ECO_NAME, .after=ECOREGION_CODE) %>%  group_by(ECOREGION_CODE,ECO_NAME) %>%
                            summarise(across(where(is.numeric), sum)) %>% mutate_if(is.numeric, sqm, na.rm = TRUE) %>% ungroup()

scen_outputfile_extended_2 <- scen_outputfile %>% merge(ecoregions_ID,by="ECOREGION_CODE") %>% dplyr::select(1,23:49) %>%
  rename(ECO_NAME = ECO_NAME.y) %>% relocate(ECO_NAME, .after=ECOREGION_CODE) %>%   mutate_at(vars(-c(ECOREGION_CODE,ECO_NAME)), ~replace(., is.na(.), 0)) %>% 
  group_by(ECOREGION_CODE,ECO_NAME) %>% summarise(across(where(is.numeric), sum)) %>% mutate_if(is.numeric, sqm, na.rm = FALSE) %>% ungroup()

scen_outputfile_extended[is.na(scen_outputfile_extended)] <- 0
scen_outputfile_extended <- as.data.frame(scen_outputfile_extended) %>% dplyr::select(1:28)
write.xlsx(scen_outputfile_extended,paste0("C:/GCAM/GCAM_7.0_Theo_2/Output data by ecoregion/",scen_name_shp,".xlsx") ,
          sheetName = scen_name_shp, overwrite = TRUE, startRow = 2, rowNames=TRUE,colNames=FALSE)


# Risk spatial distribution plots
# ggplot(data = world) + geom_sf(data = world, color = "gray80", lwd=0, fill = "gray80") + geom_sf(data = Species_forest_ref, aes(fill = risk, color=risk)) + 
#   scale_fill_gradient (name = "Risk in forest \nhabitats", low = "gray80", high ="forestgreen", breaks = c(0,0.5,1), limits=c(0,1), labels=c("0","0.5","1")) +  
#   scale_color_gradient (low = "gray80", high = "forestgreen", guide = 'none') +  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") + 
#   theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_text(family = "Calibri Light", face = "bold", size = 10),
#   legend.text = element_text(family = "Calibri Light", size = 8), legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 3, ticks = FALSE, title.vjust = 4))
#
# ggplot(data = world) + geom_sf(data = world, color = "gray80", lwd=0, fill = "gray80") + geom_sf(data = Species_shrubland_ref, aes(fill = risk, color=risk)) +
#   scale_fill_gradient (name = "Risk in shrubland \nhabitats", low = "gray80", high ="saddlebrown", breaks = c(0,0.5,1), limits=c(0,1), labels=c("0","0.5","1")) +
#   scale_color_gradient (low = "gray80", high = "saddlebrown", guide = 'none') +  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_text(family = "Calibri Light", face = "bold", size = 10),
#         legend.text = element_text(family = "Calibri Light", size = 8), legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 3, ticks = FALSE, title.vjust = 4))
# 
# ggplot(data = world) + geom_sf(data = world, color = "gray80", lwd=0, fill = "gray80") + geom_sf(data = Species_grassland_ref, aes(fill = risk, color=risk)) +
#  scale_fill_gradient (name = "Risk in grassland \nhabitats", low = "gray80", high ="yellow", breaks = c(0,0.5,1), limits=c(0,1), labels=c("0","0.5","1")) +
#  scale_color_gradient (low = "gray80", high = "yellow", guide = 'none') +  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#  theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_text(family = "Calibri Light", face = "bold", size = 10),
#        legend.text = element_text(family = "Calibri Light", size = 8), legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 3, ticks = FALSE, title.vjust = 4))
# 
# ggplot(data = world) + geom_sf(data = world, color = "gray80", lwd=0, fill = "gray80") + geom_sf(data = Species_sparse_ref, aes(fill = risk, color=risk)) + 
#   scale_fill_gradient (name = "Risk in sparse \nhabitats", low = "gray80", high ="darkblue", breaks = c(0,0.5,1), limits=c(0,1), labels=c("0","0.5","1")) +  
#   scale_color_gradient (low = "gray80", high = "darkblue", guide = 'none') +  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") + 
#   theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_text(family = "Calibri Light", face = "bold", size = 10),
#         legend.text = element_text(family = "Calibri Light", size = 8), legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 3, ticks = FALSE, title.vjust = 4))
# 
# ggplot(data = world) + geom_sf(data = world, color = "gray80", lwd=0, fill = "gray80") + geom_sf(data = Species_urban_ref, aes(fill = risk, color=risk)) + 
#   scale_fill_gradient (name = "Risk in urban \nhabitats", low = "gray80", high ="darkviolet", breaks = c(0,0.5,1), limits=c(0,1), labels=c("0","0.5","1")) +  
#   scale_color_gradient (low = "gray80", high = "darkviolet", guide = 'none') +  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") + 
#   theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_text(family = "Calibri Light", face = "bold", size = 10),
#         legend.text = element_text(family = "Calibri Light", size = 8), legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 3, ticks = FALSE, title.vjust = 4))
# 
# ggplot(data = world) + geom_sf(data = world, color = "gray80", lwd=0, fill = "gray80") + geom_sf(data = Species_crops_ref, aes(fill = risk, color=risk)) +
#   scale_fill_gradient (name = "Risk in crop \nhabitats", low = "gray80", high ="darkred", breaks = c(0,0.5,1), limits=c(0,1), labels=c("0","0.5","1")) +
#   scale_color_gradient (low = "gray80", high = "darkred", guide = 'none') +  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0") +
#   theme(panel.border = element_blank(), panel.grid = element_blank(), panel.background = element_rect(fill = "white"), legend.title = element_text(family = "Calibri Light", face = "bold", size = 10),
#         legend.text = element_text(family = "Calibri Light", size = 8), legend.position = c(0.9, 0.8)) + guides(fill = guide_colourbar(barwidth = 0.9, barheight = 3, ticks = FALSE, title.vjust = 4))
# 


# -------------------- Instructions to incorporate Species habitats into Ecoregions ------------------------------------------------------------------------------------------------------------------------ 


# ## Data from IUCN Red List of Threatened Species
# # 
# setwd("C:/GCAM/Jorge/Biodiversity/IUCN Red List/MAMMALS")
# RedList_polyg <- st_read('MAMMALS.shp')
# RedList_polyg <- st_make_valid(RedList_polyg)
# 
# # Keep only terrestrial mammals with categories of near threatened, vulnerable, endangered and critically endangered
# RedList_polyg_terr <- RedList_polyg %>% filter(terrestial == "true") %>% filter(category == "NT" | category == "VU" | category == "EN" | category == "CR" )
# 
# # Incorporate habitat data for those species
# RedList_habitat <- read.csv("habitats.csv") %>% rename("id_no"="internalTaxonId")  %>% rename("binomial"="scientificName")
# # names(RedList_habitat)[2] <- 'id_no'
# # names(RedList_habitat)[3] <- 'binomial'
# RedList_polyg_terr_hab <- merge(RedList_polyg_terr,RedList_habitat,by= c("id_no","binomial")) %>% separate(name, into = c("habitat", "subhabitat"), sep = " - ")
# 
# # Keep habitat whose change can be assessed by GCAM/Demeter and translate habitat names into GCAM/Demeter land use outputs
# RedList_polyg_terr_hab <- RedList_polyg_terr_hab %>% filter(habitat == "Forest" | habitat == "Grassland" | habitat == "Rocky areas (eg. inland cliffs, mountain peaks)" |
#                                                             habitat == "Shrubland" | habitat == "Desert" | habitat == "Savanna" | subhabitat == "Plantations" |
#                                                             subhabitat == "Arable Land" | subhabitat == "Urban Areas" | subhabitat == "Pastureland" |
#                                                             subhabitat == "Subtropical/Tropical Heavily Degraded Former Forest" | subhabitat == "Rural Gardens")  %>%
#                                                      mutate(habitat = case_when(habitat == "Rocky areas (eg. inland cliffs, mountain peaks)" ~ "Sparse",
#                                                                                 habitat == "Desert" ~ "Sparse",
#                                                                                 habitat == "Savanna" ~ "Grassland",
#                                                                                 subhabitat == "Plantations" ~ "Crop",
#                                                                                 subhabitat == "Arable Land" ~ "Crop",
#                                                                                 subhabitat == "Urban Areas" ~ "Urban",
#                                                                                 subhabitat == "Pastureland" ~ "Grassland",
#                                                                                 subhabitat == "Subtropical/Tropical Heavily Degraded Former Forest" ~ "Forest",
#                                                                                 subhabitat == "Rural Gardens" ~ "Urban",
#                                                                                 TRUE ~ habitat))
# 
# # Export files to be treated with QGIS (Change folder name for each scenario)
# st_write(RedList_polyg_terr_hab,"C:/GCAM/Jorge/Biodiversity/QGIS working files/Reference/Species_habitat.shp",append = FALSE)

# Update folder name for each scenario
# st_write(LUChange2100,"C:/GCAM/Jorge/Biodiversity/QGIS working files/Reference/LUChange2100.shp",append = FALSE)


# QGIS processing to aggregate species habitat data inside ecoregion perimeters
# 1. To import shape file: Layer > Add Layer > Add Vector Layer. Select one .shp at a time (both the Species habitat and the Ecoregions shape files).
# 2. Fix the geometry of the species habitat layer by looking for the "Fix geometries" option in the "Processing Toolbox" search bar and selecting 
#    the "Species_habitat" layer as "Input layer"
# 3. Create a spatial index for the Species habitat layer using the "Create Spatial Index" tool (it can be found in the "Processing Toolbox" search box on the right).
# 4. Go to Settings > Options > Processing and under "General" make sure "Skip (ignore) features with invalid geometries" is chosen 
#    as Value for "Invalid features filtering".
# 5. Perform a "Join attribute by location" (NOT THE SUMMARY) with the Ecoregions layer as the "Base layer" and the fixed Species habitat layer as "Join layer" with "contains" 
#    as "Geometric predicate", check "binomil" and "habitat" in the "Fields to add (leave empty to use all fields) [optional]", and select "Create separate feature for each 
#    matching feature (one-to-many)" as "Join type". Run.
# 6. Duplicate the layer and keep in the newly created layer only the observations with non-empty attribute values for "binomil" by doing a "Query builder" (double click on 
#    the Species habitat layer in the "Layers" menu, then in the "Source" tab where the "Query builder" is available in the the bottom right corner). Then type 
#    "binomil" IS NOT NULL in the "Provider Specific Filter Expression". Click "OK".

# # Extract data from taxa classification and merge it with the Results matrix
# Species_habitat <- as.data.frame(st_read("C:/GCAM/Jorge/Biodiversity/QGIS working files/Reference/Species_habitat.shp")) %>%
#   select(binomil, kingdom, phylum, class, order_, family, genus, categry) %>% distinct()
# 
# 
# 
# Mammals_extinctions_extended <- Mammals_extinctions_extended %>% merge(Species_habitat,by="binomil") %>%
#   select(binomil, ECOREGION_CODE, habitat, forst_x_su, shrub_x_su, grass_x_su, urban_x_su, snow_x_sum, spars_x_su, crops_x_su,
#          forst_y_su, shrub_y_su, grass_y_su, urban_y_su, snow_y_sum, spars_y_su, crops_y_su, Habitat.type, z, Sorg_mammals,
#          Sorg_birds, Sorg_amphibians, Sorg_reptiles, Mammals_TER, Birds_TER, Amphibians_TER, Reptiles_TER)
# 





