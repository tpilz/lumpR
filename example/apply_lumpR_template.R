# Copyright (C) 2017 Tobias Pilz
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.





# This is an example script showing how to apply lumpR.

# ATTENTION: You will need to use your own data as we so far have not generated an example dataset! Do the adjustments within the SETTINGS section of this script
#
# BY JUST RUNNING THIS SCRIPT WILL PRODUCE ERRORS! TAKE YOUR TIME TO UNDERSTAND WHAT IS GOING ON!
# Have a look at produced outputs within you working directory and within GRASS after every function call!
#
# Script only demonstrates the steps of landscape discretisation and database processing.
# Additional functionalities (reservoir processing and seasonality analysis for the pre-processing of WASA's vegetation dynamics parametrisation) are not included!
#
# PLEASE READ THE DOCUMENTATION:
#  a) R help of lumpR functions
#  b) https://github.com/tpilz/lumpR/wiki
#  c) the comments of this script
#
# REFERENCES:
# lumpR Paper (in preparation!): https://github.com/tpilz/lumpr_paper
#
# landscape discretisation algorithm:
# Francke et al. (2008), Int. J. Geogr. Inf. Sci., 22, 111–132, doi:10.1080/13658810701300873



### INSTALLATION ###

# you will need devtools to install lumpR from github!
if("lumpR" %in% rownames(installed.packages())) {
  library(lumpR)
} else if("devtools" %in% rownames(installed.packages())) {
  library(devtools)
  install_github("tpilz/lumpR")
  library(lumpR)
} else {
  install.packages("devtools")
  library(devtools)
  install_github("tpilz/lumpR")
  library(lumpR)
}




### SETTINGS ###

# switch to specified working directory
setwd("~/lumpR_test/") #use "/" instead of "\" in Windows


# DATABASE #
# you will need to set up an ODBC database
# for Windows consider https://github.com/tpilz/lumpR/wiki/04-Databases-and-ODBC
dbname <- "" # DSN registered at ODBC

# LINUX ONLY:
# register database (write into .odbc.ini in your $HOME)
str_odbc <- c(paste0("[", dbname, "]"), # adjust as you like
              "Description = lumpR analysis database",
              "Driver = ", # INSTALL AND FILL IN HERE YOUR ODBC DRIVER!
              "ServerName = localhost", # as needed
              paste0("Database = ", getwd(), "/dbase.db"), # adjust as you like
              "")
write(str_odbc, file="~/.odbc.ini", ncolumns=1, append=T, sep="\n")



# INPUT #
# inputs marked MANDATORY have to be given, the rest can be 'NULL' if not available

# watershed outlet (coordinates in projection of GRASS location!) 
drain_p <- data.frame(utm_x_m=, utm_y_m=)
# specifiy columns containing coordinates
coordinates(drain_p) <- c("utm_x_m", "utm_y_m")

# DEM raster in GRASS location
dem <- ""

# land / vegetation cover raster map in GRASS location - MANDATORY
lcov <- ""

# soil raster map in GRASS location - MANDATORY
soil <- ""

# soil depth raster map
soil_depth <- ""

# water mask raster map in GRASS location (1=water, 0=no water)
watermask <- ""

# impervious surface areas raster map in GRASS location (1=impervious, 0=permeable)
imperviousmask <- NULL

# river vector map
river <- NULL

# path to prepared vegetation parameter table 'vegetation.dat' in WASA format - MANDATORY
veg_path <- ""

# path to prepared soil parameter tables 'horizons.dat, 'particle_classes.dat', 'r_soil_contains_particles.dat', and 'soil.dat' in WASA format - MANDATORY
soil_path <- ""


# OUTPUT #
# outputs marked MANDATORY have to be given, the rest can be 'NULL'
# some outputs are inputs for other functions (e.g. flow accumulation)

# subbasin raster map - MANDATORY
subbas <- "subbas"

# prefix of calculated stream segments raster and vector maps
stream_pref <- "stream_accum"

# prefix of drainage point vector files (given points snapped to river and internally calculated points, respectively) - MANDATORY
drainp_processed <- "drain_points"

# elementary hillslope areas raster map - MANDATORY
eha <- "eha"

# flow direction raster map - MANDATORY
flowdir <- "flowdir"

# flow accumulation raster map - MANDATORY
flowacc <- "flowacc"

# stream segments raster map (based on eha calculation; much finer than 'stream_pref' to delineate hillslopes) - MANDATORY
stream <- "stream"

# Horton stream order raster map (based on 'stream' above) - MANDATORY
stream_horton <- "stream_horton"

# elevation relative to next river segment raster map - MANDATORY
elevriv <- "elevriv"

# distance to next river segment raster map - MANDATORY
distriv <- "distriv"

# soil vegetation components raster map - MANDATORY
svc <- "svc"

# landscape units raster map - MANDATORY
lu <- "lu"

# name of file containing svc parameters - MANDATORY
svc_file <- "soil_vegetation_components.dat"

# Name of output file containing mean catena information as input for prof_class - MANDATORY
catena_out <- "rstats.txt"

# Name of output header file containing meta-information as input for prof_class - MANDATORY
catena_head_out <- "rstats_head.txt"

# Name of subbasin statistics file containing subbasin parameters
sub_ofile <- "sub_stats.txt"

# Name of file containing subbasins and the corresponding LUs with their fraction of area in the subbasin
lu_ofile <- "lu_stats.txt"

# Name of file containing LUs and related parameters
lupar_ofile <- "lu_pars.txt"

# Name for the vector reservoir map created in GRASS location containing information on reservoir size classes in the attribute table
# If NULL it will not be created
res_vect_class <- "res_vect_class"



# PARAMETERS #
# STRONGLY CASE-STUDY SPECIFIC! Try out what suits your needs / data

# MISCELLANEOUS PARAMETERS #
# parameters influecing some outputs but not directly discretisation complexity

# Raster cells in accumulation map with values greater than thresh_stream are considered as streams. Needs to be set only if river is not set - MANDATORY
thresh_stream <- 1000

# maximum distance for snapping of drain_points to stream segments in units of your GRASS location - MANDATORY
snap_dist <- 500

# shall small spurious subbasins created within calc_subbas() be deleted? (automatically set to FALSE if drain_use_reservoirs=TRUE)
rm_spurious <- T

# minimum size of EHAs (in map units) not to be removed, smaller EHAs (artefacts) are removed; parameter for GRASS function r.reclass.area - MANDATORY
sizefilter <- 30 

# growing radius (in raster cells) to remove artefacts in EHA data; parameter for GRASS function r.grow - MANDATORY
growrad <- 50

# minimum number of cells a hillslope area must have, all smaller ones are skipped
min_cell_in_slope <- 10

# minimum number of sampling points (cells) a catena should have. If there are less, the catena is not saved
min_catena_length <- 3

# maximum distance to river [in cells]: if the closest cell of an EHA is farther than max_riv_dist, the EHA is skipped, otherwise all distances within the EHA are redurced by the distance of the closest cell to river
max_riv_dist <- 15


# RUN TIME PARAMETERS #

# shall temporary files be kept in the GRASS location, e.g. for debugging or further analyses?
keep_temp <- F

# Shall output of previous calls of this function be deleted? If FALSE the function returns an error if output already exists
overwrite <- T

# Shall the function be silent (also suppressing warnings of internally used GRASS functions, etc.)?
silent <- F

# produce plots (scatter, mean catena, etc.) for each area / class (written into sub-directory plots_area2catena)
plot_catena <- T

# produce plots of classification of catenas to landscape units and terrain components
plot_profclass <- T

# produce GRASS reclassification files for qualitative raster data
grass_files <- F

# number of cores that should be used for parallel computation (where possible)
ncores <- 1

# LANDSCAPE DISCRETISATION PARAMETERS #

# Parameter for GRASS function r.watershed defining the minimum size of an exterior watershed basin in number of grid cells. If NULL only the given drainage points are used for subbasin delineation
thresh_sub <- 5000
  
# parameter for GRASS function r.watershed. This is a crucial parameter affecting the size of delineated hillslopes - MANDATORY
eha_thres <- 200
  
# vector with GRASS file names of quantitative supplemental information for LU deviation (adjust no_LUs accordingly)
supp_quant <- NULL

# vector with GRASS file names of qualitative supplemental information for LU deviation (adjust no_LUs accordingly)
supp_qual <- c("svc") # svc has to be defined to generate SVC parameters needed for WASA! Map is generated by lump_grass_prep()

# number of LUs to be produced per attribute in the order:
# c( <shape>, <extent>, <weighting vertical vs. horizontal extent>, <quant. suppl. information>, <qual. suppl. information>, <slope width> )
no_LUs <- c(3, 3, 10, 3, 3)

# number of TCs that are created by algorithm (numTC)
no_TCs <- 3


# GRASS #
# ATTENTION: GRASS 6.4 is needed, not compatible to GRASS 7!
# Best is to use GRASS 6.4.6 as GRASS 6.4.5 by autumn 2016 suddenly started producing segmentation faults!
addon_path="/home/tobias/.grass6/addons/" # path to your locally installed GRASS add-ons. Must only be given if necessary, see ?lump_grass_prep
# initialisation of session
initGRASS(gisBase="", # path to GRASS installation (use / instead of \ under windows, e.g. "d:/programme/GRASS6.4.3" )
          home=getwd(), # The directory in which to create the .gisrc file
          location="", # GRASS location
          mapset="",    # corresp. mapset
          gisDbase="",  # path to 'grassdata' directory containing the location specified above and all corresp. data
          override=TRUE)
  
  
  
  
  
### CALCULATIONS ####
# no adjustments necessary
# run line-by-line to understand what is going on!


      
# SUBBASIN DELINEATION #
# define projection of drainage point(s) (use projection of GRASS location)
projection(drain_p) <- getLocationProj()

# calculate subbasins; one subbasin for each drainage point
?calc_subbas # read the documentation!
calc_subbas(
  # INPUT #
  dem=dem,
  drain_points=drain_p,
  river=river,
  # OUTPUT #
  basin_out=subbas,
  stream=stream_pref,
  points_processed=drainp_processed,
  # PARAMETERS #
  outlet=1,
  thresh_stream=thresh_stream,
  thresh_sub=thresh_sub,
  snap_dist=snap_dist,
  rm_spurious=rm_spurious,
  keep_temp=keep_temp,
  overwrite=overwrite,
  silent=silent
)
      
      
      
# PREPROCESSING AND HILLSLOPE DEVIATION #
?lump_grass_prep # read the documentation!
lump_grass_prep(
  # INPUT #
  mask = subbas,
  dem = dem,
  lcov = lcov,
  soil = soil,
  watermask = watermask,
  imperviousmask = imperviousmask,
  # OUTPUT #
  eha=eha,
  flowdir = flowdir,
  flowacc = flowacc,
  stream = stream,
  stream_horton = stream_horton,
  elevriv = elevriv,
  distriv = distriv,
  mask_corr = "MASK_corr",
  svc = svc,
  dir_out = getwd(),
  svc_ofile = svc_file,
  # PARAMETERS #
  eha_thres = eha_thres,
  sizefilter = sizefilter,
  growrad = growrad,
  keep_temp=keep_temp,
  overwrite=overwrite,
  silent=silent,
  addon_path=addon_path
)
      
      
      
# CALCULATE MEAN CATENA FOR HILLSLOPES #
# Part of algorithm described by Francke et al. (2008)
?area2catena # read the documentation!
area2catena(
  # INPUT #
  mask="MASK_corr",
  flowacc=flowacc,
  eha=eha,
  distriv=distriv,
  elevriv=elevriv,
  supp_quant=supp_quant,
  supp_qual=supp_qual,
  # OUTPUT #
  dir_out=getwd(),
  catena_out=catena_out,
  catena_head_out=catena_head_out,
  # PARAMETERS #
  ridge_thresh=1,
  min_cell_in_slope=min_cell_in_slope,
  min_catena_length=min_catena_length,
  max_riv_dist=max_riv_dist,
  plot_catena=plot_catena,
  grass_files=grass_files,
  ncores=ncores,
  eha_subset=NULL,
  overwrite=overwrite,
  silent=silent
)

# change header file according to rstats_header
header_dat <- readLines(paste(getwd(), catena_head_out, sep="/"))
no_LUs[1] <- no_LUs[1] * -1
header_dat[8] <- paste(no_LUs, "\t", sep="", collapse="")
header_dat[9] <- paste(c(no_TCs, rep(0, length(no_LUs)-1)), "\t", sep="", collapse="")
writeLines(header_dat,paste(getwd(), catena_head_out, sep="/"))



# CATENA CLASSIFICATION INTO LANDSCAPE UNITS AND TERRAIN COMPONENTS #
# Part of algorithm described by Francke et al. (2008)
# get resolution (mean between x and y resolution)
res <- execGRASS("r.info", map=dem, flags=c("s"), intern=TRUE)
res <- sum(as.numeric(gsub("[a-z]*=", "", res))) / 2

?prof_class # read the documentation!
prof_class(
  # INPUT #
  catena_file=catena_out,
  catena_head_file=catena_head_out,
  svc_column="svc",
  # OUTPUT #
  dir_out=getwd(),
  luoutfile="lu.dat",
  tcoutfile="tc.dat",
  lucontainstcoutfile="lucontainstc.dat",
  tccontainssvcoutfile="tc_contains_svc.dat",
  terraincomponentsoutfile="terraincomponents.dat",
  recl_lu="reclass_lu.txt",
  saved_clusters=NULL,
  # PARAMETERS #
  seed=1312,
  resolution=res,
  classify_type=' ',
  max_com_length=50,
  com_length=NULL,
  make_plots=plot_profclass,
  eha_subset=NULL,
  overwrite=overwrite,
  silent=silent
)



# POST PROCESSING #
?lump_grass_post # read the documentation!
lump_grass_post(
# INPUT #
mask = "MASK_corr",
dem = dem,
recl_lu = "reclass_lu.txt",
lu = lu,
subbasin = subbas,
eha = eha,
flowacc = flowacc,
flowdir = flowdir,
stream_horton = stream_horton,
soil_depth = NULL,
sdr=NULL,
# OUTPUT #
dir_out = getwd(),
sub_ofile = sub_ofile,
lu_ofile = lu_ofile,
lupar_ofile = lupar_ofile,
# PARAMETER #
fill_holes=T,
groundwater=0,
keep_temp = keep_temp,
overwrite = overwrite,
silent = silent
)



# DATABASE #
# rainy_season not included within this example

# create database
?db_create
db_create(dbname)

# update database (if necessary)
?db_update
db_update(dbname)

# create information file for filling landscape_units into database (SIMPLEST POSSIBLE PARAMETER VALUES USED HEREIN)
luout <- read.table("lu.dat", header=T)
lupar <- read.table(lupar_ofile, header=T)
lupar$slopelength <- luout$x_length
lupar$soil_depth <- -1 # groundwater option I.1.1 (WASA documentation) 
lupar$allu_depth <- -1 # groundwater option I.1.1 (WASA documentation) 
lupar$riverbed_depth <- 2000 # riverbed in any case below soil (no information whether this is reasonable or not)
lupar$kf_bedrock <- -9999
lupar$gw_dist <- -9999
lupar$frgw_delay <- -9999
write.table(lupar, "lu_db.dat", quote = F, row.names = F, sep="\t")

# copy soil and vegetation parameter files into output_dir
file.copy(paste(veg_path, "vegetation.dat", sep="/"), "vegetation.dat", overwrite=T)
file.copy(paste(soil_path, "soil.dat", sep="/"), "soil.dat", overwrite=T)
file.copy(paste(soil_path, "horizons.dat", sep="/"), "horizons.dat", overwrite=T)
file.copy(paste(soil_path, "particle_classes.dat", sep="/"), "particle_classes.dat", overwrite=T)
file.copy(paste(soil_path, "r_soil_contains_particles.dat", sep="/"), "r_soil_contains_particles.dat", overwrite=T)

# lumpR output and manually prepared information (e.g. soil parameters) to database
?db_fill
db_fill(dbname=dbname,
        tables = c("r_subbas_contains_lu", "subbasins",
                   "landscape_units", "r_lu_contains_tc", "terrain_components", "r_tc_contains_svc",
                   "vegetation", "soils", "horizons", "soil_veg_components",
                   "particle_classes", "r_soil_contains_particles"),
        dat_files=c("lu_stats.txt", "sub_stats.txt",
                    "lu_db.dat", "lucontainstc.dat", "terraincomponents.dat", "tc_contains_svc.dat",
                    "vegetation.dat", "soil.dat", "horizons.dat", "soil_vegetation_components.dat",
                    "particle_classes.dat", "r_soil_contains_particles.dat"), 
        dat_dir=getwd(),
        overwrite=T, verbose=T)

# Please process these cleaning actions step-by-step according to your needs.
?db_check

db_check(dbname, 
         check=c("filter_small_areas"), 
         option=list(area_thresh=0.01),
         fix=T,
         verbose=T)

db_check(dbname, 
         check=c("tc_slope"), 
         option=list(treat_slope=c(3,0.01,0.1)),
         fix=T,
         verbose=T)

# db_check(dbname, 
#          check=c("special_areas"), 
#          option=list(special_area = data.frame(reference_tbl=c("vegetation", "vegetation", "soils"), ref_id=c(3,4,10), special_id=c(1,1,2))),
#          fix=F,
#          verbose=F)

db_check(dbname, 
         check=c("remove_water_svc"),
         fix=T,
         verbose=T)

db_check(dbname, 
         check=c("compute_rocky_frac"),
         fix=T,
         verbose=T)

db_check(dbname, 
         check=c("remove_impervious_svc"),
         fix=T,
         verbose=T)

# db_check(dbname, 
#          check=c("proxy_frgw_delay"), 
#          option=list(total_mean_delay=50),
#          fix=T,
#          verbose=T)

db_check(dbname,
         check=c("delete_obsolete"),
         fix=T,
         verbose=T)

db_check(dbname,
         check=c("completeness"),
         fix=T,
         verbose=T)

db_check(dbname,
         check=c("subbasin_order"),
         fix=T,
         verbose=T)

# generate input files for WASA
?db_wasa_input
db_wasa_input(dbname = dbname,
              dest_dir = paste(getwd(), "WASA_input", sep="/"),
              files=c("info.dat", "River/routing.dat", "River/response.dat", "Hillslope/hymo.dat",
                      "Hillslope/soter.dat", "Hillslope/terrain.dat", "Hillslope/soil_vegetation.dat",
                      "Hillslope/soil.dat", "Hillslope/vegetation.dat", "Hillslope/svc_in_tc.dat",
                      "do.dat", "maxdim.dat", "part_class.dat", "Hillslope/soil_particles.dat", "Hillslope/svc.dat"),
              overwrite = overwrite, verbose=T)
      
# adjust model input data to your needs ...

# Generate WASA input data outside the scope of lumpR (meteo data etc.) ...
 