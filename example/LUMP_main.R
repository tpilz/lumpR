# R Script for pre-processing of WASA model using LUMP package and GRASS GIS
#
# Install LUMP from github:
# library(devtools)
# install_github("https://github.com/tpilz/LUMP")
#
# based on SHELL and MATLAB scripts of Till Francke
# theoretical basis see: Francke et al. (2008): Automated catena-based discretization of landscapes for the derivation of hydrological modelling units
#
# by Tobias Pilz <tpilz@uni-potsdam.de>, JAN 2015
#
# NOTES: GUIs such as RStudio might not produce runtime messages within parallel ('foreach') loops
#        In case of problems with area2catena within the foreach-loop set '.errorhandling="pass"' and execute 'str(logdata)' after the loop to show error messages

# load LUMP after installation
library(LUMP)
library(rgdal)

# switch to specified working directory
setwd("/home/tobias/Promotion/Modellierung/WASA/LUMP/scripts/")

# start GRASS session
  initGRASS(gisBase="/opt/grass",                                     # path to GRASS installation (use / instead of \ under windows)
           home=tempdir(),                                            # The directory in which to create the .gisrc file; can usually be set to tempdir()
           location="Ceara",                                          # GRASS location
           mapset="LUMP",                                             # corresp. mapset
           gisDbase="/home/tobias/Promotion/Modellierung/grassdata",  # path to grass data containing the location specified above and all corresp. data
           override=TRUE)

# output (sub-)directory
output_dir <- "../test"



# clean up GRASS location if necessary (remove outputs from previous call of lump_grass_prep())
#execGRASS("g.mremove", rast="*fill,stream,stream_horton,eha,flow_dir,flow_accum,elev_riv,dist_riv,MASK*,svc*", flags=c("f"))




### SUBBASIN DELINEATION ###
# read gage file (containing at least lon/lat or x/y coordinates for each gage)
# for each gage a subbasin will be created containing gage at its outlet
gages_file <- "/home/tobias/Promotion/Modellierung/Data/Hidroweb/gages_list.csv"
dat <- read.table(gages_file, header=T, sep=",", dec=".", quote="")

# needs to be adapted:
dat <- dat[which(dat[,"data_qual"] == "acceptable" & dat[,"type"] != "reservoir"),c("lon", "lat")]

# if necessary:
# function for converting lonlat coords from 'degree:min:sec' to decimal format
lonlat_convert <- function(x) {
  val <- as.numeric(x)
  if (val[1] < 0) {
    return(val[1] - val[2]/60 - val[3]/3600)
  } else {
    return(val[1] + val[2]/60 + val[3]/3600)
  }
}

# apply function
lon <- gsub(":", "_", dat$lon)
lon <- strsplit(lon, "_")
lon <- sapply(lon, lonlat_convert)

lat <- gsub(":", "_", dat$lat)
lat <- strsplit(lat, "_")
lat <- sapply(lat, lonlat_convert)

dat$lon <- lon
dat$lat <- lat

# transform lonlat coordinates of dat to CRS of GRASS location
coordinates(dat) <- c("lon", "lat")
projection(dat) <- "+proj=longlat +ellps=WGS84"
# derive from grass location with g.proj -j
dat <- spTransform(dat, crs(getLocationProj()))

# determine reservoir outlet coordinates from reservoir vector file and flowwacc or dem raster in GRASS location
res_coords <- reservoir_outlet(dem="dem_srtm", res_vct="res_crop")

# merge with gage points
sub_outs <- rbind(dat, res_coords)

# calculate subbasins, one subbasin for each point defined above (gages, reservoir outlets)
calc_subbas(
  # INPUT #
  dem="dem_srtm",
  drain_points=sub_outs,
  # OUTPUT #
  basin_out="subbas",
  stream="stream_accum",
  # PARAMETERS #
  outlet=10,
  thresh_stream=1000,
  snap_dist=2000,
  keep_temp=T
)



### GRASS PREPROCESSING ###
# check if you have the GRASS command r.stream.distance available and install it if it is missing
# in my case (Linux) addons to GRASS can be added to a local addon directory in $HOME
# I don't know into which env. variable to include the directory path
# expanding $PATH works within shell scripts but not within R -> specify full path to command
r_stream_distance <- "/home/tobias/.grass6/addons/r.stream.distance"
r_stream_order <- "/home/tobias/.grass6/addons/r.stream.order"

# call pre-processing function
lump_grass_prep(
  # INPUT #
  mask = "subbas",
  dem = "dem_srtm_filled",
  lcov = "lcov_m_grow",
  soil = "HWSD_soil",
  # OUTPUT #
  eha="eha",
  flowdir = "flow_dir",
  flowacc = "flow_accum",
  stream = "stream",
  stream_horton = "stream_horton",
  elevriv = "elev_riv",
  distriv = "dist_riv",
  mask_corr = "MASK_corr",
  svc = "svc",
  dir_out = output_dir,
  svc_ofile = "soil_vegetation_components.csv",
  # PARAMETERS #
  eha_thres = 3000,
  sizefilter = 30,
  growrad = 50,
  r_stream_distance = r_stream_distance,
  r_stream_order = r_stream_order
)


### LANDSCAPE ATTRIBUTES ###
# specify auxiliary attribute and their weights/class no. in the classification
# alternatively, execute the main script line-by-line and adjust rstats_head.txt manually after it was created
# name:      name of raster map in GRASS
# lu_class:  weighting factor / number of LU-classes to generate for this attribute
# tc_class:  weighting factors (relative to slope) for the respective attribute used  in TC-partitioning
# type:      "quant" for numerical data, "qual" for categorical data
# for details see LUMP_manual
LANDSCAPE_ATTRIBUTES=rbind(
  cbind(name="HWSD_soil",          lu_class=2,  tc_class=0, type="qual"),
  cbind(name="lcov_m_grow",      lu_class=1,  tc_class=0, type="qual"), 
  cbind(name="geoenv_grow2",         lu_class=1,  tc_class=0, type="qual"),
  cbind(name="svc",            lu_class=1,  tc_class=0, type="qual"),
  cbind(name="urban_m_zero",            lu_class=1,  tc_class=0, type="qual"),
  
  #do not remove this line, only adjust lu_class and tc_class 
  cbind(name="slope_width", lu_class=1,  tc_class=0, type="") #weighting factor / number of classes to generate for slope_width
)

CLASSIFICATION_METHOD=-1  # 1: Specifying the overall number of classes and weighting factor for each attribute
                          #-1: Specifying the number of classes applied to each single attribute. The final classification results from the intersection of the classification for each single attribute.

cLU1 = 3   #number of LUs that are to be created by algorithm / number of classes to be generated based on shape
cLU2 = 3   #weighting factor of horizontal extension / number of classes to be generated based on horizontal AND vertical extension
cLU3 = 2   #weighting factor of vertical extension, relative to shape-weighting [=1]) / weighting factor of vertical extension, relative to horizontal-weighting 

cTC1 = 3   #number of TCs that are created by algorithm (numTC)

LANDSCAPE_ATTRIBUTES=rbind(
  cbind(name="c1", lu_class=CLASSIFICATION_METHOD*cLU1, tc_class=cTC1, type=""), 
  cbind(name="c2", lu_class=cLU2,                       tc_class=0,   type=""),   
  cbind(name="c3", lu_class=cLU3,                       tc_class=0,   type=""), 
  LANDSCAPE_ATTRIBUTES
)

SUPP_QUAL  = LANDSCAPE_ATTRIBUTES[LANDSCAPE_ATTRIBUTES[,"type"]=="qual" , "name"]
SUPP_QUANT = LANDSCAPE_ATTRIBUTES[LANDSCAPE_ATTRIBUTES[,"type"]=="quant", "name"]

#assemble header for rstats_header
rstats_header =  list(lu_class=LANDSCAPE_ATTRIBUTES[,"lu_class"],
                      tc_class=LANDSCAPE_ATTRIBUTES[,"tc_class"])



### area2catena: calculate hillslpe properties ###
area2catena(
  # INPUT #
  flowacc="flow_accum",
  eha="eha",
  distriv="dist_riv",
  elevriv="elev_riv",
  supp_quant=SUPP_QUANT,
  supp_qual=SUPP_QUAL,
  # OUTPUT #
  dir_out=output_dir,
  catena_out="rstats.txt",
  catena_head_out="rstats_head.txt",
  # PARAMETERS #
  ridge_thresh=1,
  min_cell_in_slope=30,
  min_catena_length=3,
  max_riv_dist=15,
  plot_catena=T,
  grass_files=T,
  ncores=8,
  eha_subset=NULL
)

# change header file according to rstats_header (see above)
header_dat <- readLines(paste(output_dir, "rstats_head.txt", sep="/"))
header_dat[8] <- paste(rstats_header$lu_class, "\t", sep="", collapse="")
header_dat[9] <- paste(rstats_header$tc_class, "\t", sep="", collapse="")
writeLines(header_dat,paste(output_dir, "rstats_head.txt", sep="/"))



### prof_class: classification of landscape units (LU) and terrain components (TC) ###
# get resolution (mean between x and y resolution)
res <- execGRASS("r.info", map="dem_srtm_filled", flags=c("s"), intern=TRUE)
res <- sum(as.numeric(gsub("[a-z]*=", "", res))) / 2

prof_class(
  # INPUT #
  catena_file=paste(output_dir,"rstats.txt",sep="/"),
  catena_head_file=paste(output_dir,"rstats_head.txt",sep="/"),
  # OUTPUT #
  dir_out=output_dir,
  luoutfile="lu.dat",
  tcoutfile="tc.dat",
  lucontainstcoutfile="lucontainstc.dat",
  tccontainssvcoutfile="tc_contains_svc.dat",
  terraincomponentsoutfile="terraincomponents.dat",
  recl_lu="reclass_lu.txt",
  saved_clusters="cluster_centers.Rdat",
  # PARAMETERS #
  seed=931317,
  resolution=res,
  classify_type='',
  max_com_length=50,
  make_plots=T,
  eha_subset=NULL
)



### POST PROCESSING ###
lump_grass_post(
  # INPUT #
  dem = "dem_srtm_filled",
  recl_lu = paste(output_dir, "reclass_lu.txt", sep="/"),
  eha = "eha",
  subbasin = "subbas",
  mask = "MASK_corr",
  flowacc = "flow_accum",
  flowdir = "flow_dir",
  stream_horton = "stream_horton",
  soil_depth = "HWSD_depth",
  # OUTPUT #
  lu = "lu",
  dir_out = output_dir,
  sub_ofile = "sub_stats.txt",
  lu_ofile = "lu_stats.txt",
  lupar_ofile = "lu_pars.txt"
)
