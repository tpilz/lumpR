# lumpR/db_wasa_input.R
# Copyright (C) 2015-2017 Tobias Pilz
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


#' Create WASA input files
#' 
#' Function retrieves parameters from a parameter database and generates ASCII files
#' as input to the  hydrological model WASA.
#' 
#' @param dbname Name of the data source (DSN) registered as ODBC source.
#' 
#' @param dest_dir The directory in which the output files (= WASA input files) will be
#' written. Will be created if it does not exist. Default: \code{./}. Will include the
#' sub-directories 'Hillslope' and 'River'.
#' 
#' @param files Character vector specifying WASA input files that should be created. 
#' See \code{Details}. By default, all files will be created.
#' 
#' @param overwrite \code{logical}. Should existing files in \code{dest_dir} be
#' overwritten? Default: \code{FALSE}.
#' 
#' @param verbose \code{logical}. Should detailed information during execution be
#' printed? Default: \code{TRUE}.
#' 
#' 
#' @details
#' Consider function \code{\link[lumpR]{db_check}} before running this function to ensure consistency
#' and completeness of data in the database. Otherwise, the input files might contain
#' errors, which might lead to errors or unexpected results during model application.
#' 
#' Note differences in variable notations between parameter database and WASA's
#' input files!
#' 
#'  Sediment transport specific variables are not yet included, please configure \code{erosion.ctl} manually.
#'  
#'  The following files will be created:
#'  
#'  \bold{info.dat}\cr
#'  General information (actual date, parameter database version etc.).
#'  
#'  \bold{River/routing.dat}\cr
#'  File contains routing order (i.e. flow direction) at subbasin level.
#'  
#'  \bold{River/response.dat}\cr
#'  File contains routing parameters (lag time and retention in \emph{days})
#'  at subbasin level.
#'  
#'  
#'  \bold{Hillslope/hymo.dat}\cr
#'  File contains relations between subbasins and landscape units. Only subbasins
#'  present in table 'subbasins' AND 'r_subbas_contains_lu' will be considered.
#'  Columns:
#'  
#'  \emph{Subbasin-ID}\cr
#'  ID of subbasin.
#'  
#'  \emph{Area}\cr
#'  Area of each subbasin in \emph{km^2} (including reservoir areas).
#'  
#'  \emph{nbr}\cr
#'  Number of landscape units in the respective subbasin.
#'  
#'  \emph{LU-IDs}\cr
#'  IDs of landscape units in the respective subbasin
#'  
#'  \emph{areal_fraction_of_LU}\cr
#'  Areal fraction of each LU in the respective subbasin.
#'  
#'  
#'  \bold{Hillslope/soter.dat}\cr
#'  File contains landscape unit parameters and associated terrain components. Only LUs
#'  present in table 'landscape_units' AND 'r_lu_contains_tc' will be considered.
#'  
#'  \emph{LU-ID}\cr
#'  ID of landscape unit.
#'  
#'  \emph{No._of_TC}\cr
#'  Number of TCs in the corresponding LU.
#'  
#'  \emph{TC1}\cr
#'  ID of first terrain component.
#'  
#'  \emph{TC2}\cr
#'  ID of second terrain component.
#'  
#'  \emph{TC3}\cr
#'  ID of third terrain component.
#'  
#'  \emph{TCx}\cr
#'  IDs of the other TCs as specified in field \emph{No._of_TC} (if available).
#'  
#'  \emph{kfsu[mm/d]}\cr
#'  Hydraulic conductivity of bedrock in \emph{mm/d}.
#'  
#'  \emph{length[m]}\cr
#'  Mean slope length in LU in \emph{m}.
#'  
#'  \emph{meandep[mm]}\cr
#'  Mean maximum depth of soil zone in \emph{mm}.
#'  
#'  \emph{maxdep[mm]}\cr
#'  Maximum depth of alluvial soil zone in \emph{mm}.
#'  
#'  \emph{Riverbed[mm]}\cr
#'  Depth of River bed below terrain component in \emph{mm}.
#'  
#'  \emph{gwflag[0/1]}\cr
#'  Groundwater flag for LU. 0: no groundwater, 1: with groundwater. 
#'  
#'  \emph{gw_dist[mm]}\cr
#'  Initial depth of groundwater below surface in \emph{mm}.
#'  
#'  \emph{frgw_delay[day]}\cr
#'  Storage coefficient for groundwater outflow in \emph{days}.
#'  
#'  
#'  \bold{Hillslope/terrain.dat}\cr
#'  File contains specification of terrain components. Only TCs
#'  present in tables 'terrain_components' AND 'r_lu_contains_tc' will be considered.
#'  
#'  \emph{TC-ID}\cr
#'  ID of terrain component.
#'  
#'  \emph{fraction}\cr
#'  Areal fraction of TC in corresponding LU.
#'  
#'  \emph{slope[\%]}\cr
#'  Slope of TC in \emph{\%}.
#'  
#'  \emph{position}\cr
#'  Number indicating the relative position of TC along the hillslope. 1: highland,
#'  2: middle, ..., [highest number]: foot slope.
#'  ATTENTION: The order in WASA input file is reversed in comparison to order within
#'  database!
#'  
#'  
#'  \bold{Hillslope/svc_in_tc.dat}\cr
#'  SVC relations to TC.
#'  
#'  
#'  \bold{Hillslope/soil_vegetation.dat}\cr
#'  Definition of soil vegetation components. Only SVCs occurring in table 'r_tc_contains_svc'
#'  will be considered.
#'  
#'  
#'  \bold{Hillslope/soil.dat}\cr
#'  Horizon specific soil parameters. See file header and \code{\link[lumpR]{db_fill}}
#'  for more information. Only soil types occurring in tables 'soil_veg_components'
#'  and 'r_tc_contains_svc' will be considered.
#'  
#'  \bold{Hillslope/vegetation.dat}\cr
#'  Vegetation parameters. See file header and \code{\link[lumpR]{db_fill}}
#'  for more information. Only vegetation types occurring in table 'soil_veg_components'
#'  and 'r_tc_contains_svc' will be considered.
#'  
#'  \bold{do.dat}\cr
#'  File contains general parameters and control flags for WASA. See file's comments for
#'  more information. Manual investigation and adjustment after creation is necessary
#'  (e.g. for input/output directories, start/stop year of simulation etc.). Note that,
#'  depending on your choices, the manual creation of additional input files will
#'  be necessary. Consult the WASA documentation.
#'  
#'  \bold{maxdim.dat}\cr
#'  \emph{Optional} file that contains maximum dimensions of spatial units to
#'  optimise memory management and improve computational performance.
#'  
#'  \bold{part_class.dat}\cr
#'  \emph{Optional} file for sediment modelling of multiple particle size classes.
#'   File defines the number and the properties of the particle sizes that will be
#'   modelled. Please note that class numbering has to be continuous, starting with
#'   1. The particle size classes must be ordered from fine to coarse.
#'   
#'  \bold{gauges_catchment_area.txt}\cr
#'  \emph{Optional, auxiliary file not needed by WASA} File relating subcatchment sizes, gauge names and upstream areas. Can be used for calibration workflow.
#'   
#'   \bold{Hillslope/soil_particles.dat}\cr
#'   File contains particle size distributions of topmost soil horizons.
#'   
#'   \bold{Hillslope/rainy_season.dat, Hillslope/x_seasons.dat, }\cr
#'   \emph{Optional} file defining days of year (i.e. nodes, cf. vegetation parameters)
#'   of the rainy/growing season for each year, subbasin and vegetation type. See
#'   doc of \code{\link[lumpR]{db_fill}} and \code{\link[lumpR]{rainy_season}} for
#'   more information. If this file is not supplied, only the first node value of 
#'   seasonal vegetation parameters is used.
#'   
#'   \bold{Hillslope/svc.dat}\cr
#'   \emph{Optional} file defining soil vegetation components and giving some erosion
#'   parameters. Mandatory for WASA's sediment module and/or saving/loading of model
#'   states. See doc of \code{\link[lumpR]{db_fill}} (-> 'soil_veg_components') for
#'   description of header.
#'  
#'   \bold{Reservoir/reservoir.dat}\cr
#'   \emph{Optional} file defining properties of strategic reservoirs.
#'   
#'   \bold{Reservoir/lake.dat, lake_number.dat, lake_maxvol.dat}\cr
#'   \emph{Optional} file defining properties of small reservoirs.
#'   
#' @references 
#'      lumpR package introduction with literature study and sensitivity analysis:\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#' 
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}

db_wasa_input <- function(
  dbname,
  dest_dir = "./",
  files=c("info.dat", "River/routing.dat", "River/response.dat", "Hillslope/hymo.dat",
          "Hillslope/soter.dat", "Hillslope/terrain.dat", "Hillslope/soil_vegetation.dat",
          "Hillslope/soil.dat", "Hillslope/vegetation.dat", "Hillslope/svc_in_tc.dat",
          "do.dat", "maxdim.dat", "part_class.dat", "Hillslope/soil_particles.dat",
          "Hillslope/rainy_season.dat", "Hillslope/x_seasons.dat", "Hillslope/svc.dat", "Reservoir/reservoir.dat",
          "Reservoir/lake.dat","Reservoir/lake_number.dat","Reservoir/lake_maxvol.dat"),
  overwrite=F,
  verbose = TRUE
) {
  
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(verbose) message("% START db_wasa_input()")
  
  if(verbose) message("%")
  if(verbose) message("% Connecting to database ...")
  
  # connect to ODBC registered database
  con <- connect_db(dbname)
  
  #modify error handler to gracefully close ODBC-connection before aborting (otherwise, ODBC-handles are left open)
  org_error_handler = getOption("error") #original error handler
  
  closeODBC_and_stop = function()
  {  
    odbcCloseAll()
    options(error=org_error_handler) #restore original handler
  }
  options(error=closeODBC_and_stop)  #modify error handler
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  if(verbose) message("% OK")
  
  
  if(verbose) message("%")
  if(verbose) message("% Check database version ...")
  
  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="lumpR")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))
  
  db_ver <- max(sqlFetch(con, "db_version")$version)
  if(db_ver < db_ver_max) stop(paste0("Database version (", db_ver,") is prior to version ", db_ver_max, ". Make sure you use the latest database version (consider function db_update())!"))
  
  if(verbose) message("% OK")
  
  
  # create and/or check output directory
  dir.create(dest_dir, recursive=T, showWarnings = F)
  dir.create(paste(dest_dir, "River", sep="/"), recursive=T, showWarnings = F)
  dir.create(paste(dest_dir, "Hillslope", sep="/"), recursive=T, showWarnings = F)
  dir.create(paste(dest_dir, "Reservoir", sep="/"), recursive=T, showWarnings = F)
  
  if(verbose) message("%")
  if(verbose) message(paste0("% Output will be written to ", dest_dir))
    
  pathfiles <- paste(dest_dir, files, sep="/")
  if(any(file.exists(pathfiles)))
    if(overwrite) {
      if(verbose)
        message(paste0("% -> The following files in specified path '", dest_dir, "' will be overwritten: ", paste(files[file.exists(pathfiles)], collapse=", ")))
    } else {
      stop(paste0("There are still files at '", dest_dir, "' that shall not be overwritten: ", paste(files[file.exists(pathfiles)], collapse=", ")))
    }
  
  
  # initialise object where database information will be stored during processing
  dat_all <- NULL


  


### info.dat #####
  if("info.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create info.dat ...")
  
    # create file
    if(!file.exists(paste(dest_dir, "info.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "info.dat", sep="/"))
    } else {
      stop("File 'info.dat' exists!")
    }
  
    # get actual database revision from table meta_info
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("meta_info"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    meta_rev <- max(dat_all$meta_info$pid)
    
    # write data into file
    writeLines(con=paste(dest_dir, "info.dat", sep="/"),
               text=paste0("generated on ", Sys.time(), " with R-Package lumpR function db_wasa_input() version ",
                      installed.packages()["lumpR","Version"], " from database version ",
                      db_ver, " revision ", meta_rev,"."))
    
    if(verbose) message("% OK")
  }




### River/routing.dat #####
  if("River/routing.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create River/routing.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "River/routing.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "River/routing.dat", sep="/"))
    } else {
      stop("File 'River/routing.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "River/routing.dat", sep="/"),
               text=c("Specification of routing order (flow directions)",
                      "No.\tSubasin-ID(upstream)\tSubasin-ID(downstream)"))

    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    if(any(is.na(cbind(dat_all$subbasins$pid, dat_all$subbasins$drains_to, dat_all$subbasins$a_stream_order))) | nrow(dat_all$subbasins) == 0)
      stop("Cannot write file River/routing.dat. Column(s) 'pid', 'drains_to' and/or 'a_stream_order' of table 'subbasins' contain missing values!")
    
    # sort
    r_order <- order(dat_all$subbasins$a_stream_order, decreasing=TRUE)
    dat_out <- dat_all$subbasins[r_order,]
    dat_out <- cbind(1:nrow(dat_out), dat_out$pid, dat_out$drains_to)
    
    # write output
    write.table(dat_out, paste(dest_dir, "River/routing.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    
    if(verbose) message("% OK")
  } # River/routing.dat




### River/response.dat #####
  if("River/response.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create River/response.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "River/response.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "River/response.dat", sep="/"))
    } else {
      stop("File 'River/response.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "River/response.dat", sep="/"),
               text=c("Specification of routing parameter",
                      "Subbasin-ID\tlag time [d]\tretention [d]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # data for output
    dat_out <- cbind(dat_all$subbasins$pid, dat_all$subbasins$lag_time, dat_all$subbasins$retention)
    if(any(is.na(dat_out)) | nrow(dat_all$subbasins) == 0)
      stop("Cannot write file River/response.dat Column(s) 'pid', 'lag_time' and/or 'retention' of table 'subbasins' contain missing values!")
    
    # write output
    write.table(dat_out, paste(dest_dir, "River/response.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    if(verbose) message("% OK")
    
  } # River/response.dat




### Hillslope/hymo.dat #####
  if("Hillslope/hymo.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/hymo.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/hymo.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/hymo.dat", sep="/"))
    } else {
      stop("File 'Hillslope/hymo.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/hymo.dat", sep="/"),
               text=c("Specification of the sub-basins and their total number, type and areal fraction of SOTER units",
                      "Subasin-ID[-]\tArea[km**2]\tnbr[-]\tLU-ID[-]\tareal_fraction_of_LU[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins", "r_subbas_contains_lu"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # check if subbasins are in the contains table
    r_sub_out <- which(!(dat_all$subbasins$pid %in% dat_all$r_subbas_contains_lu$subbas_id))
    if(any(r_sub_out))
      stop(paste0("Subbasin(s) ", paste0(dat_all$subbasins$pid[r_sub_out], collapse=", "), " from table 'subbasins' are not in table 'r_subbas_contains_lu'! Consider db_check()!"))

    if(nrow(dat_all$subbasins) == 0)
      stop("Cannot write file Hillslope/hymo.dat No validly specified subbasins in 'subbasins' found!")
    
    if(any(is.na(cbind(dat_all$subbasins$pid,dat_all$subbasins$area))))
      stop("Cannot write file Hillslope/hymo.dat Column(s) 'pid' and/or 'area' of table 'subbasins' contain missing values!")
    if(any(is.na(dat_all$r_subbas_contains_lu)) | nrow(dat_all$r_subbas_contains_lu) == 0)
      stop("Cannot write file Hillslope/hymo.dat Table 'r_subbas_contains_lu' contains missing values!")

    
    # loop over subbasins, reversely ordered by a_stream_order
    r_sub_ordered <- rev(order(dat_all$subbasins$a_stream_order))
    for(s in r_sub_ordered) {
      # identify rows in contains table of current subbasin
      r_contains <- which(dat_all$r_subbas_contains_lu$subbas_id == dat_all$subbasins$pid[s])
      
      # check that fractions sum up to 1
      if(round(sum(dat_all$r_subbas_contains_lu$fraction[r_contains]),3) != 1)
        stop(paste0("For subbasin ", dat_all$subbasins$pid[s], " sum of areal fractions of landscape units (table 'r_subbas_contains_lu') are not equal to one. Consider function db_check()."))
      
      # string for output file with relevant information
#       str_out <- paste(dat_sub$pid[s], round(dat_sub$area[s],2), length(r_contains), 
#                        paste(dat_contains$lu_id[r_contains], collapse="\t"),
#                        paste(round(dat_contains$fraction[r_contains],3), collapse="\t"), sep="\t")
      str_out <- paste(dat_all$subbasins$pid[s], dat_all$subbasins$area[s], length(r_contains), 
                       paste(dat_all$r_subbas_contains_lu$lu_id[r_contains], collapse="\t"),
                       paste(dat_all$r_subbas_contains_lu$fraction[r_contains], collapse="\t"), sep="\t")
      
      # write output
      write(file=paste(dest_dir, "Hillslope/hymo.dat", sep="/"),x=str_out,append=T,sep="\n")
    }
    
    
    if(verbose) message("% OK")
    
  } # Hillslope/hymo.dat
  




### Hillslope/soter.dat #####
  if("Hillslope/soter.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/soter.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/soter.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/soter.dat", sep="/"))
    } else {
      stop("File 'Hillslope/soter.dat' exists!")
    }
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("landscape_units", "r_lu_contains_tc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    dat_all[["landscape_units"]]$frgw_delay = round(dat_all[["landscape_units"]]$frgw_delay, digits = 2) #round gw-edlay to 2 digits
    
    # write header
    htext=c("Specification of landscape units",
           "LU-ID[id]\tNo._of_TC[-]\tTC1[id]\tTC2[id]\tTC3[id]\tkfsu[mm/d]\tlength[m]\tmeandep[mm]\tmaxdep[mm]\tRiverbed[mm]\tgwflag[0/1]\tgw_dist[mm]\tfrgw_delay[day]")
    omit_fields = which(names(dat_all$landscape_units) == "description")
    if (all(is.na(dat_all$landscape_units$sdr_lu))) 
      omit_fields = c(omit_fields, which(names(dat_all$landscape_units) == "sdr_lu"))
    else
      htext[2]=paste0(htext[2],"\tSDR_LU[-]")  
    
    writeLines(con=paste(dest_dir, "Hillslope/soter.dat", sep="/"), text=htext)
    
    # check that LUs are in the contains table
    r_lu_out <- which(!(dat_all$landscape_units$pid %in% dat_all$r_lu_contains_tc$lu_id))
    if(any(r_lu_out))
      stop(paste0("LUs ", paste0(dat_all$landscape_units$pid[r_lu_out], collapse=", "), " from table 'landscape_units' are not in table 'r_lu_contains_tc'! Consider db_check()!"))
    
    if(any(is.na(dat_all$landscape_units[,-omit_fields])) | nrow(dat_all$landscape_units) == 0)
      stop("Cannot write file Hillslope/soter.dat Table 'landscape_units' contains missing values!")
    if(any(is.na(dat_all$r_lu_contains_tc)) | nrow(dat_all$r_lu_contains_tc) == 0)
      stop("Cannot write file Hillslope/soter.dat Table 'r_lu_contains_tc' contains missing values!")
    
    # loop over LUs
    for(s in 1:nrow(dat_all$landscape_units)) {
      # identify rows in contains table of current LU and ensure correct order of TCs
      r_contains <- which(dat_all$r_lu_contains_tc$lu_id == dat_all$landscape_units$pid[s])
      ord <- order(dat_all$r_lu_contains_tc$position[r_contains])
      
      # check that fractions sum up to 1
      if(round(sum(dat_all$r_lu_contains_tc$fraction[r_contains]),3) != 1)
        stop(paste0("For LU ", dat_all$landscape_units$pid[s], " sum of areal fractions of terrain components (table 'r_lu_contains_tc') are not equal to one. Consider function db_check()."))
      
      # string for output file with relevant information
      str_out <- paste(dat_all$landscape_units$pid[s], length(r_contains), 
                       paste(dat_all$r_lu_contains_tc$tc_id[r_contains][ord], collapse="\t"),
                       paste(dat_all$landscape_units[s,c("kf_bedrock", "slopelength", "soil_depth", "allu_depth",
                                                   "riverbed_depth", "gw_flag", "gw_dist", "frgw_delay")], collapse = "\t"), sep="\t")
      if (!all(is.na(dat_all$landscape_units$sdr_lu)))
        str_out <- paste(str_out, dat_all$landscape_units[s, "sdr_lu"], sep="\t")

      
      # write output
      write(file=paste(dest_dir, "Hillslope/soter.dat", sep="/"),x=str_out,append=T,sep="\n")
    }
    
    
    if(verbose) message("% OK")
    
  } # Hillslope/soter.dat




### Hillslope/terrain.dat #####
  if("Hillslope/terrain.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/terrain.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/terrain.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/terrain.dat", sep="/"))
    } else {
      stop("File 'Hillslope/terrain.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/terrain.dat", sep="/"),
               text=c("Specification of terrain components",
                      "TC-ID\tfraction\tslope[%]\tposition[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("terrain_components", "r_lu_contains_tc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # check that TCs are in the contains table
    r_tc_out <- which(!(dat_all$terrain_components$pid %in% dat_all$r_lu_contains_tc$tc_id))
    if(any(r_tc_out))
      stop(paste0("TCs ", paste0(dat_all$terrain_components$pid[r_tc_out], collapse=", "), " from table 'terrain_components' are not in table 'r_lu_contains_tc'! Consider db_check()!"))
    
    if(any(is.na(dat_all$terrain_components$pid)) | any(is.na(dat_all$terrain_components$slope)) | nrow(dat_all$terrain_components) == 0)
      stop("Cannot write file Hillslope/terrain.dat. Column(s) 'pid' and/or 'slope' of table 'terrain_components' contain missing values!")
    if(any(is.na(dat_all$r_lu_contains_tc)) | nrow(dat_all$r_lu_contains_tc) == 0)
      stop("Cannot write file Hillslope/terrain.dat. Table 'r_lu_contains_tc' contains missing values!")
    
    # reverse positions (in WASA input the opposite order compared to database)
    for (l in unique(dat_all$r_lu_contains_tc$lu_id)){
      rows <- which(dat_all$r_lu_contains_tc$lu_id == l)
      dat_all$r_lu_contains_tc$position[rows] <- rev(dat_all$r_lu_contains_tc$position[rows])
    }
    
    # loop over TCs
    for(s in 1:nrow(dat_all$terrain_components)) {
      # identify rows in contains table of current TC
      r_contains <- which(dat_all$r_lu_contains_tc$tc_id == dat_all$terrain_components$pid[s])
      
      if (nrow(unique(dat_all$r_lu_contains_tc[r_contains,-1])) > 1)
        stop(paste0("TC ", dat_all$terrain_components$pid[s], " is part of multiple LUs (", paste0(dat_all$r_lu_contains_tc$lu_id[r_contains], collapse=" ,"),") which is currently not supported. Duplicate these TCs and assign a different ID for each instance."))
      else
        r_contains = r_contains[1]
      
      # string for output file with relevant information
      str_out <- paste(dat_all$terrain_components$pid[s], dat_all$r_lu_contains_tc$fraction[r_contains],
                       dat_all$terrain_components$slope[s], dat_all$r_lu_contains_tc$position[r_contains], sep="\t")
      
      # write output
      write(file=paste(dest_dir, "Hillslope/terrain.dat", sep="/"),x=str_out,append=T,sep="\n")
    }
    
    
    if(verbose) message("% OK")
    
  } # Hillslope/terrain.dat




### Hillslope/svc_in_tc.dat #####
  if("Hillslope/svc_in_tc.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/svc_in_tc.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/svc_in_tc.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/svc_in_tc.dat", sep="/"))
    } else {
      stop("File 'Hillslope/svc_in_tc.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/svc_in_tc.dat", sep="/"),
               text=c("Specification of which SVCs are contained in each TC",
                      "TC-ID[-]\tSVC-ID[-]\tfraction[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("r_tc_contains_svc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # check data
    dat_out <- dat_all$r_tc_contains_svc[which(dat_all$r_tc_contains_svc$svc_id != -1),] # remove special area flag
    dat_out <- dat_out[,c("tc_id", "svc_id", "fraction")]
    if(any(is.na(dat_out)) | nrow(dat_out) == 0)
      stop("Could not write file Hillslope/svc_in_tc.dat. There are missing values in table 'r_tc_contains_svc'!")
    flawed_tcs <- check_fix_fractions(dat_tbl=dat_all$r_tc_contains_svc, fix=FALSE, update_frac_impervious=FALSE, verbose=FALSE)
    if(length(flawed_tcs) > 0)
        stop(paste0("The fractions of TCs ", paste(flawed_tcs, collapse = ", "), " do not sum up to one. Check tables 'r_tc_contains_svc' and 'terrain_components' (column frac_rocky) or call db_check(..., check=\"check_fix_fractions\", fix=TRUE)!"))
    
    # write output
    write.table(dat_out, paste(dest_dir, "Hillslope/svc_in_tc.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    if(verbose) message("% OK")
    
  } # Hillslope/svc_in_tc.dat




### Hillslope/soil_vegetation.dat #####
  if("Hillslope/soil_vegetation.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/soil_vegetation.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/soil_vegetation.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/soil_vegetation.dat", sep="/"))
    } else {
      stop("File 'Hillslope/soil_vegetation.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/soil_vegetation.dat", sep="/"),
               text=c("Specification of soil-vegetation components (links soter, terrain component, soil and vegetation properties)",
                      "For each block: first line Soil IDs, Second line Land use, third line fraction of SVCs in each terrain component",
                      "Subasin-ID[id]\tLU-ID[id]\tTC-ID[id]\tfraction_rocky[-]\tnbrSVC[-]\tSoil-ID(n_values)[-]\tVegetation-ID(n_values)[-]\tfraction(n_values)[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("terrain_components", "soil_veg_components", "r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # loop over subbasins
    for(s in unique(dat_all$r_subbas_contains_lu$subbas_id)) {
      # identify rows in contains table of current subbasin
      r_sub_contains <- which(dat_all$r_subbas_contains_lu$subbas_id == s)
      
      # loop over LUs in this subbasin
      for(l in dat_all$r_subbas_contains_lu$lu_id[r_sub_contains]) {
        # identify rows in contains table of current LU
        r_lu_contains <- which(dat_all$r_lu_contains_tc$lu_id == l)
        
        # loop over TCs in this LU
        for(tc in dat_all$r_lu_contains_tc$tc_id[r_lu_contains]){
          
          # identify rows in contains table of current TC
          dat_tc_svc_t <- dat_all$r_tc_contains_svc[which(dat_all$r_tc_contains_svc$tc_id == tc), ]
          
          # check that fractions sum up to 1
          flawed_tcs <- check_fix_fractions(dat_tbl=dat_tc_svc_t, fix=FALSE, update_frac_impervious=FALSE, verbose=FALSE)
          if(length(flawed_tcs) > 0)
            stop(paste0("The fractions of TCs ", paste(flawed_tcs, collapse = ", "), " do not sum up to one. Check tables 'r_tc_contains_svc' and 'terrain_components' (column frac_rocky) or call db_check(..., check=\"check_fix_fractions\", fix=TRUE)!"))
          
          # remove special area flag
          dat_tc_svc_t <- dat_tc_svc_t[which(dat_tc_svc_t$svc_id != -1),]
          
          # rows in svc table (only svc in contains table are considered)
          r_svc <- which(dat_all$soil_veg_components$pid %in% dat_tc_svc_t$svc_id)
          
          # string for output file with relevant information
          str_out_1 <- paste(s, l, tc, dat_all$terrain_components$frac_rocky[which(dat_all$terrain_components$pid == tc)], nrow(dat_tc_svc_t),
                             paste(dat_all$soil_veg_components$soil_id[r_svc], collapse="\t"), sep="\t")
          str_out_2 <- paste(s, l, tc, dat_all$terrain_components$frac_rocky[which(dat_all$terrain_components$pid == tc)], nrow(dat_tc_svc_t),
                             paste(dat_all$soil_veg_components$veg_id[r_svc], collapse="\t"), sep="\t")
          str_out_3 <- paste(s, l, tc, dat_all$terrain_components$frac_rocky[which(dat_all$terrain_components$pid == tc)], nrow(dat_tc_svc_t),
                             paste(dat_tc_svc_t$fraction, collapse="\t"), sep="\t")
          
          # write output
          write(file=paste(dest_dir, "Hillslope/soil_vegetation.dat", sep="/"),
                x=c(str_out_1, str_out_2, str_out_3), append=T, sep="\n")
                           
        } # TCs
      } # LUs
    } # sub
    
    
    if(verbose) message("% OK")
    
  } # Hillslope/soil_vegetation.dat




### Hillslope/svc.dat #####
  if("Hillslope/svc.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/svc.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/svc.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/svc.dat", sep="/"))
    } else {
      stop("File 'Hillslope/svc.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/svc.dat", sep="/"),
               text=c("Specifications of soil vegetation components and erosion parameters",
                      "id\tsoil_id\tveg_id\tmusle_k[(ton acre hr)/(acre ft-ton inch)]\tmusle_c[-]\tmusle_p[-]\tcoarse_fraction[%]\tmanning_n"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("soil_veg_components"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # only relevant data
    dat_out <- data.frame(dat_all$soil_veg_components$pid, dat_all$soil_veg_components$soil_id, dat_all$soil_veg_components$veg_id, dat_all$soil_veg_components$musle_k, dat_all$soil_veg_components$musle_c1,
                          dat_all$soil_veg_components$musle_p, dat_all$soil_veg_components$coarse_frac, dat_all$soil_veg_components$manning_n)
    
    # set NA values to -9999 (related to sediment which is not yet supported; file in present form only useful for saving WASA storages)
    dat_out[is.na(dat_out)] <- -9999
    
    # write output
    write.table(dat_out, paste(dest_dir, "Hillslope/svc.dat", sep="/"), sep="\t", row.names=F, col.names=F, append=T, quote=F)
    
  
    if(verbose) message("% OK")
    
  } # Hillslope/svc.dat
  
  
  
  
### Hillslope/soil.dat #####
  if("Hillslope/soil.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/soil.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/soil.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/soil.dat", sep="/"))
    } else {
      stop("File 'Hillslope/soil.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/soil.dat", sep="/"),
               text=c("Specification of soil parameters",
                      "Soil-ID[-]\tnumber(horizons)[-]\t(n_res[Vol-]\tn_PWP[-]\tn_FK2.6[-]\tn_FK1.8[-]\tn_nFK[-]\tn_saturated[-]\tn_thickness[mm]\tn_ks[mm/d]\tn_suction[mm]\tn_pore-size-index[-]\tn_bubblepressure[cm]\tn_coarse_frag[-]*n\tn_shrinks[0/1])\tbedrock[0/1]\talluvial[0/1]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("horizons", "soils", "soil_veg_components", "r_tc_contains_svc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # only soil types occurring in soil_veg_components and r_tc_contains_svc will be considered
    dat_svc <- dat_all$soil_veg_components
    dat_soil <- dat_all$soils
    r_svc_out <- which(!(dat_svc$pid %in% dat_all$r_tc_contains_svc$svc_id))
    if(any(r_svc_out))
      dat_svc <- dat_svc[-r_svc_out,]
    
    r_soil_out <- which(!(dat_soil$pid %in% dat_svc$soil_id))
    if(any(r_soil_out)) {
      warning(paste0("Soil types ", paste0(dat_all$soils$pid[r_soil_out], collapse=", "), " from table 'soils' are not in table 'soil_veg_components', or the respective SVCs are not in 'r_tc_contains_svc', and will be ignored."))
      dat_soil <- dat_soil[-r_soil_out,]
    }
    
    
    # loop over soils
    for(s in 1:nrow(dat_soil)) {
      # get rows of current soil
      r_hor <- which(dat_all$horizons$soil_id == dat_soil$pid[s])
      
      # string for output file with relevant information
      dat_out <- dat_all$horizons[r_hor, c("theta_r", "theta_pwp", "fk", "fk63", "nfk", "theta_s", "thickness",
                                        "ks", "suction", "pore_size_i", "bubb_pres", "coarse_frag", "shrinks")]
      
      if(nrow(dat_soil) == 0)
        stop(paste("Could not successfully write Hillslope/soil.dat - no data found. Check tables 'soils' and 'horizons'!"))
      
      dat_out$shrinks[is.na(dat_out$shrinks)]=0
      if(any(is.na(cbind(dat_out, dat_soil$pid[s], dat_soil$bedrock_flag[s], dat_soil$alluvial_flag[s]))) )
        stop(paste("Could not successfully write Hillslope/soil.dat. For soil(s) ", dat_soil$pid[s], " there are missing values. Check tables 'soils' and 'horizons'!"))
      
      str_out <- paste(dat_soil$pid[s], length(r_hor),
                       paste(apply(dat_out,1,paste,collapse="\t"),collapse="\t"),
                       dat_soil$bedrock_flag[s], dat_soil$alluvial_flag[s], sep="\t")
      
      # write output
      write(file=paste(dest_dir, "Hillslope/soil.dat", sep="/"),
            x=str_out, append=T, sep="\n")
    }
    
    if(verbose) message("% OK")
    
  } # Hillslope/soil.dat




### Hillslope/vegetation.dat #####
  if("Hillslope/vegetation.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/vegetation.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/vegetation.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/vegetation.dat", sep="/"))
    } else {
      stop("File 'Hillslope/vegetation.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/vegetation.dat", sep="/"),
               text=c("Specification of vegetation parameters",
                      "Veg-ID\tStomata_Resistance[s/m]\tminsuction[hPa]\tmaxsuction[hPa]\theight1[m]\theight2[m]\theight3[m]\theight4[m]\trootdepth1[m]\trootdepth2[m]\trootdepth3[m]\trootdepth4[m]\tLAI1[-]\tLAI2[-]\tLAI3[-]\tLAI4[-]\talbedo1[-]\talbedo2[-]\talbedo3[-]\talbedo4[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("vegetation", "soil_veg_components", "r_tc_contains_svc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # only veg types occurring in soil_veg_components and r_tc_contains_svc will be considered
    dat_veg_out <- dat_all$vegetation
    dat_svc <- dat_all$soil_veg_components
    r_svc_out <- which(!(dat_svc$pid %in% dat_all$r_tc_contains_svc$svc_id))
    if(any(r_svc_out))
      dat_svc <- dat_svc[-r_svc_out,]
    
    r_veg_out <- which(!(dat_veg_out$pid %in% dat_svc$veg_id))
    if(any(r_veg_out)) {
      warning(paste0("Vegetation types ", paste0(dat_veg_out$pid[r_veg_out], collapse=", "), " from table 'vegetation' are not in table 'soil_veg_components', or the respective SVCs are not in 'r_tc_contains_svc', and will be ignored."))
      dat_veg_out <- dat_veg_out[-r_veg_out,]
    }
      
    
    
    # only the following columns in the following order
    cols <- c("pid", "stomat_r", "min_suction", "max_suction", 
              "height1", "height2", "height3", "height4",
              "root_depth1", "root_depth2", "root_depth3", "root_depth4",
              "lai1", "lai2", "lai3", "lai4",
              "alb1", "alb2", "alb3", "alb4")
    r_cols <- which(colnames(dat_veg_out) %in% cols)
    dat_veg_out <- dat_veg_out[, r_cols]
    
    # write output
    write.table(dat_veg_out, paste(dest_dir, "Hillslope/vegetation.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    
    if(verbose) message("% OK")
    
  } # Hillslope/vegetation.dat




### do.dat #####
  if("do.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/do.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "do.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "do.dat", sep="/"))
    } else {
      stop("File 'do.dat' exists!")
    }
    
    
    ### get relevant output information
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins", "landscape_units", "r_subbas_contains_lu", "r_lu_contains_tc",
                                     "soils", "soil_veg_components", "r_tc_contains_svc", "particle_classes", 
                                     "terrain_components", "vegetation"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # no. of subbasins
    no_sub <- dat_all$subbasins[which(dat_all$subbasins$pid %in% dat_all$r_subbas_contains_lu$subbas_id),]
    no_sub <- nrow(no_sub)
    
    # no. of combinations of sub-basins, landscape units, terrain components
    dat_merge <- merge(dat_all$r_subbas_contains_lu, dat_all$r_lu_contains_tc, by.x="lu_id", by.y="lu_id")
    no_sblutc <- nrow(dat_merge)
    
    # no. of landscape units
    no_lu <- dat_all$landscape_units[which(dat_all$landscape_units$pid %in% dat_all$r_subbas_contains_lu$lu_id),]
    no_lu <- nrow(no_lu)
    
    # no. of terrain components
    no_tc <- dat_all$terrain_components[which(dat_all$terrain_components$pid %in% dat_all$r_lu_contains_tc$tc_id),]
    no_tc <- nrow(no_tc)
    
    # no. of soils
    dat_svc <- dat_all$soil_veg_components[which(dat_all$soil_veg_components$pid %in% dat_all$r_tc_contains_svc$svc_id),]
    no_soil <- dat_all$soils[which(dat_all$soils$pid %in% dat_svc$soil_id),]
    no_soil <- nrow(no_soil)
    
    # no. of veg types
    no_veg <- dat_all$vegetation[which(dat_all$vegetation$pid %in% dat_svc$veg_id),]
    no_veg <- nrow(no_veg)
    
    # no. of soil particle size classes
    no_part <- nrow(dat_all$particle_classes)
    
    
    ### write output
    writeLines(con=paste(dest_dir, "do.dat", sep="/"),
               text=c(paste0("Parameter specification for the WASA Model (SESAM-Project), generated with R-Package lumpR function db_wasa_input() version ", installed.packages()["lumpR","Version"]),
                      "path/to/your_input_dir/",
                      "path/to/your_output_dir/",
                      "//tstart (start year of simulation)",
                      "//tstop (end year of simulation)",
                      "//mstart (start month of simulation [optional: <space>start_day])",
                      "//mstop (end month of simulation [optional: <space>end_day])",
                      paste0(no_sub, "\t//no. of sub-basins"),
                      paste0(no_sblutc, "\t//no. of combinations of sub-basins, landscape units, terrain components (TC-instances)"),
                      paste0(no_lu, "\t//total no. of landscape units in study area"),
                      paste0(no_tc, "\t//total no. of terrain components (types) in study area"),
                      paste0(no_soil, "\t//total no. of soil components in study area"),
                      paste0(no_veg, "\t//total no. of vegetation units in study area"),
                      ".f.\t//doreservoir: do reservoir calculations",
                      ".f.\t//doacudes: include calculations for small reservoirs",
                      ".t.\t//dolattc: do latflow between TCs",
                      ".f.\t//doalllattc: route latflow compeletely to next downslope TC",
                      ".t.\t//dolatsc: do latflow within TCs (surface runoff)",
                      ".t.\t//dolatscsub: do latflow within TCs (subsurface runoff)",
                      ".f.\t//dotrans: do water transpositions betwen sub-basins",
                      ".f.\t//dohour: do hourly version",
                      "0\t//legacy: scenario: choose scenario (0:less rain (ECHAM), 1:no trend, 2:more rain (Hadley))",
                      "0\t//legacy: krig: type of precipitation interpolation (0:OK, 1:EDK, 2:EDKxyz, 3:csimabsed3, 4:csimreled3, 5:csimreled1, 6:csimabsed1, 7:statdata, 8:statdatacon, 9:gerstdatacon, 10:gerstdata, 11:ok_mean1cell)",
                      "15.0\t//kfkorr: hydraulic conductivity factor (for daily model version) (kfkorr)",
                      "0.30\t//intcf: interception capacity per unit LAI [mm]",
                      "0\t//dointc: type of interception routine (0:simple bucket, 1:modified bucket)",
                      ".f.\t//doscale: do scaling due to rainfall interpolation ?",
                      ".f.\t//domuncell: for muni/ezg-nocell-version, use rainfall input derived from cells ? (change kf_calib.dat !)",
                      "1.\t//sensfactor: factor for sensitivity studies",
                      "24\t//dt: time step in [hours]",
                      ".f.\t//dosediment",
                      paste0(no_part, "\t//No. of soil grain size classes"),
                      "1\t// type of sediment transport model at the hillslope",
                      "1\t//type of water / sediment model in the river: (1) old routing, (2) Muskingum & ss transport, (3) Muskingum & bedload modelling",
                      "1\t//type of sediment model in the reservoir: choose sediment transport equation (1:Wu et al., 2000; 2:Ashida and Michiue, 1973; 3: Yang, 1973 and 1984; 4: Ackers and White, 1973)",
                      ".f. .f.\t//OPTIONAL: load state of storages from files (if present) at start; optional second flag: allows the model to append to existing output files, default is .f. ",
                      ".f. .t.\t//OPTIONAL: save state of storages to files after simulation period; optional second flag: determines if the model states are saved (and overwritten) at the end of each simulation year, default is .t."))
    
    if(verbose) message("% OK")
    
  } # do.dat




### maxdim.dat #####
  if("maxdim.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create maxdim.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "maxdim.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "maxdim.dat", sep="/"))
    } else {
      stop("File 'maxdim.dat' exists!")
    }
    
    
    ### get relevant output information
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc", "horizons"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # remove special area flag from "r_tc_contains_svc"
    dat_tc_svc_t <- dat_all$r_tc_contains_svc[which(dat_all$r_tc_contains_svc$svc_id != -1),]
    
    # max no. of LU in a subbasin
    counts <- tapply(dat_all$r_subbas_contains_lu$lu_id, dat_all$r_subbas_contains_lu$subbas_id, length)
    no_max_lu <- max(counts)
    
    # max no. of TC in a LU
    counts <- tapply(dat_all$r_lu_contains_tc$tc_id, dat_all$r_lu_contains_tc$lu_id, length)
    no_max_tc <- max(counts)
    
    # max no. of SVC in a TC
    counts <- tapply(dat_tc_svc_t$svc_id, dat_tc_svc_t$tc_id, length)
    no_max_svc <- max(counts)
    
    # max no. of horizons in a soil)
    counts <- tapply(dat_all$horizons$position, dat_all$horizons$soil_id, length)
    no_max_hor <- max(counts)
    
    
    ### write output
    writeLines(con=paste(dest_dir, "maxdim.dat", sep="/"),
               text=c("contains maximum dimensions of spatial units",
                      paste0(no_max_lu, "\t//maximum no. of landscape units in a sub-basins"),
                      paste0(no_max_tc, "\t//maximum no. of terrain components in a landscape unit"),
                      paste0(no_max_svc, "\t//maximum no. of soil vegetation components in a terrain component"),
                      paste0(no_max_hor, "\t//maximum no. of horizons in a soil"),
                      "2\t//maximum no. transpositions between sub-basins (only dummy; no value > 2 supported yet)"))
    
    if(verbose) message("% OK")
    
  } # maxdim.dat




  ### part_class.dat #####
  if("part_class.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create part_class.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "part_class.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "part_class.dat", sep="/"))
    } else {
      stop("File 'part_class.dat' exists!")
    }
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("particle_classes"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # write header
    writeLines(con=paste(dest_dir, "part_class.dat", sep="/"),
               text=c("Particle size classes to be used in sediment modelling",
                      "class_number\tupper_limit[mm]"))
    
    if(any(is.na(dat_all$particle_classes)) | nrow(dat_all$particle_classes) == 0)
      stop("There are missing values in table 'particle_classes'!")
    
    # write output
    write.table(dat_all$particle_classes[,c("class_id", "upper_limit")], paste(dest_dir, "part_class.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    
    if(verbose) message("% OK")
    
  } # part_class.dat
  
### gauges_catchment_area.txt #####
  if("gauges_catchment_area.txt" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create gauges_catchment_area.txt ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "gauges_catchment_area.txt", sep="/")) | overwrite){
      file.create(paste(dest_dir, "gauges_catchment_area.txt", sep="/"))
    } else {
      stop("File 'gauges_catchment_area.txt' exists!")
    }
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # write header
    writeLines(con=paste(dest_dir, "gauges_catchment_area.txt", sep="/"),
               text="GAUGE	FOREIGN_ID	SUBBAS_ID	AREA_SUBBAS_KM2	AREA_UPSTREAM_KM2")
    outdata=dat_all$subbasin[, c("pid", "drains_to", "area", "a_stream_order",  "description")]
    noname=is.na(outdata$description)
    outdata$description = as.character(outdata$description)
    outdata$description[noname] = paste0("sub", outdata$pid[noname])
    
    #compute total upstream area
    outdata$AREA_UPSTREAM_KM2 = outdata$area #initialize total upstream area
    for (so in sort(unique(outdata$a_stream_order), decreasing = TRUE)[-1])
    {
      cur_subbasins = which(outdata$a_stream_order == so)
      ups_subbasins = which(outdata$drains_to %in% outdata$pid[cur_subbasins])
      if (length(ups_subbasins)==0) next
      ups_area = aggregate(outdata$AREA_UPSTREAM_KM2[ups_subbasins], by=list(pid=outdata$drains_to[ups_subbasins]), FUN=sum)
      subs2update = match(ups_area$pid, table = outdata$pid)
      outdata$AREA_UPSTREAM_KM2[subs2update] = outdata$AREA_UPSTREAM_KM2[subs2update] + ups_area$x
    }  
    outdata$FOREIGN_ID=NA #additional unused column
    outdata = outdata[,c("description", "FOREIGN_ID", "pid", "area", "AREA_UPSTREAM_KM2")]  #re-order columns

    # write output
    write.table(outdata, paste(dest_dir, "gauges_catchment_area.txt", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    rm(outdata)
    if(verbose) message("% OK")
    
  } # gauges_catchment_area.txt
  
  


### Hillslope/soil_particles.dat #####
  if("Hillslope/soil_particles.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/soil_particles.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/soil_particles.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/soil_particles.dat", sep="/"))
    } else {
      stop("File 'Hillslope/soil_particles.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/soil_particles.dat", sep="/"),
               text=c("Particle size distribution of topmost horizons of soils",
                      "soil_id\tpart_class_id\tfraction[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("soil_veg_components", "r_tc_contains_svc", "r_soil_contains_particles"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # only soil types occurring in soil_veg_components and r_tc_contains_svc will be considered
    dat_contains_part <- dat_all$r_soil_contains_particles
    dat_svc <- dat_all$soil_veg_components
    r_svc_out <- which(!(dat_svc$pid %in% dat_all$r_tc_contains_svc$svc_id))
    if(any(r_svc_out))
      dat_svc <- dat_svc[-r_svc_out,]
    
    r_soil_out <- which(!(dat_contains_part$soil_id %in% dat_svc$soil_id))
    if(any(r_soil_out)) {
      warning(paste0("Soil types ", paste0(unique(dat_contains_part$soil_id[r_soil_out]), collapse=", "), " from table 'r_soil_contains_particles' are not in table 'soil_veg_components', or the respective SVCs are not in 'r_tc_contains_svc', and will be ignored."))
      dat_contains_part <- dat_contains_part[-r_soil_out,]
    }
    
    frac_sums <- round(tapply(dat_contains_part$fraction, dat_contains_part$soil_id, sum),3)
    if(any(frac_sums != 1))
      stop("Could not successfully write file Hillslope/soil_particles.dat. In table 'r_soil_contains_particles' not all fractions sum up to 1.")
    
    # write output
    write.table(dat_contains_part, paste(dest_dir, "Hillslope/soil_particles.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    if(verbose) message("% OK")
    
  } # Hillslope/soil_particles.dat


### Hillslope/rainy_season.dat #####
  if("Hillslope/rainy_season.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/rainy_season.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/rainy_season.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/rainy_season.dat", sep="/"))
    } else {
      stop("File 'Hillslope/rainy_season.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/rainy_season.dat", sep="/"),
               text=c("Specification of the rainy/growing season (per year)",
                      "for the interpolation of temporal distribution of vegetation characteristics (Rootdepth,height,lai,albedo)",
                      "Subasin\tVeg_id\tYear\tDOY1\tDOY2\tDOY3\tDOY4"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("rainy_season"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    #if(any(is.na(dat_all$rainy_season)) | nrow(dat_all$rainy_season) == 0)
    #  stop("There are missing values in table 'rainy_season'!")
    
    ### sort data, i.e. wildcards at the last lines
    
    # search for years with wildcards and put them at the end of the data.frame
    dat_rs <- dat_all$rainy_season
    r_year_wild <- which(dat_rs$yearm == -1)
    if(any(r_year_wild)) {
      # substract rows from data.frame
      dat_rs_t <- dat_rs[-r_year_wild,]
      # put them at the end of the data.frame
      dat_rs <- rbind(dat_rs_t, dat_rs[r_year_wild,])
    }
    
    # loop over years and search for subbas with wildcards and put them at the end of the respective year
    for (y in unique(dat_rs$yearm)) {
      rows <- which(dat_rs$yearm == y)
      dat_rs_t <- dat_rs[rows,]
      r_sub_wild <- which(dat_rs_t$subbas_id == -1)
      if (any(r_sub_wild)) {
        dat_rs_t2 <- dat_rs_t[-r_sub_wild,]
        dat_rs_t <- rbind(dat_rs_t2, dat_rs_t[r_sub_wild,])
      }
      
      # loop over subbasins and search for veg with wildcards and put them at the end of the respective subbasin
      for (s in unique(dat_rs_t$subbas_id)) {
        rows2 <- which(dat_rs_t$subbas_id == s)
        dat_rs_t2 <- dat_rs_t[rows2,]
        r_veg_wild <- which(dat_rs_t2$veg_id == -1)
        if (any(r_veg_wild)) {
          dat_rs_t3 <- dat_rs_t2[-r_veg_wild,]
          dat_rs_t2 <- rbind(dat_rs_t3, dat_rs_t2[r_veg_wild,])
        }
        
        dat_rs_t[rows2,] <- dat_rs_t2
      }
      
      dat_rs[rows,] <- dat_rs_t
    }
    
    
    
    # write output
    dat_out <- dat_rs[,c("subbas_id", "veg_id", "yearm", "node1", "node2", "node3", "node4")]
    write.table(dat_out, paste(dest_dir, "Hillslope/rainy_season.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    
    if(verbose) message("% OK")
    
  } # Hillslope/rainy_season.dat
  

### Hillslope/x_seasons.dat #####
  if("Hillslope/x_seasons.dat" %in% files) {
    if(verbose) message("%")
    if (!("x_seasons" %in% sqlTables(con)$TABLE_NAME)) #check existence of table
      {if(verbose) message("% table 'x_seasons' not found, skipped.")} else
    {      
      # get data
      dat_all <- c(dat_all,
                   read_db_dat(tbl = c("x_seasons"),
                               con = con,
                               tbl_exist = names(dat_all), update_frac_impervious=F))
      
      #if(any(is.na(dat_all$x_seasons)) | nrow(dat_all$x_seasons) == 0)
      #  stop("There are missing values in table 'x_seasons.dat'!")
      
      params = unique(dat_all$x_seasons$parameter)
        
      if (length(params) == 0)
        message("% No records in 'x_seasons', skipped.")
      else
      for (param in params)  
      { 
        tfile = paste0(tolower(param), "_seasons.dat")
        if(verbose) message("% Create Hillslope/", tfile," ...")
      
        # create file
        if(!file.exists(paste(dest_dir, "Hillslope", tfile, sep="/")) | overwrite){
          file.create(paste(dest_dir, "Hillslope", tfile, sep="/"))
        } else {
          stop("File 'Hillslope/", tfile,"' exists! Use 'overwrite=TRUE'")
        }
    
         # write header
        writeLines(con=paste(dest_dir, "Hillslope", tfile, sep="/"),
                   text=c(paste0("Specification of seasonality of ", param," (per year)"),
                          "for the interpolation of temporal distribution between 4 nodes within the year using values in svc.dat",
                          "Subasin\tsvc_id\tyear\tDOY1\tDOY2\tDOY3\tDOY4"))
        
    ### sort data, i.e. wildcards at the last lines
    
    # search for years with wildcards and put them at the end of the data.frame
    dat_rs <- dat_all$x_seasons[dat_all$x_seasons$parameter==param,] #pick entries with current parameter
    r_year_wild <- which(dat_rs$yearm == -1)
    if(any(r_year_wild)) {
      # substract rows from data.frame
      dat_rs_t <- dat_rs[-r_year_wild,]
      # put them at the end of the data.frame
      dat_rs <- rbind(dat_rs_t, dat_rs[r_year_wild,])
    }
    
    # loop over years and search for subbas with wildcards and put them at the end of the respective year
    for (y in unique(dat_rs$yearm)) {
      rows <- which(dat_rs$yearm == y)
      dat_rs_t <- dat_rs[rows,]
      r_sub_wild <- which(dat_rs_t$subbas_id == -1)
      if (any(r_sub_wild)) {
        dat_rs_t2 <- dat_rs_t[-r_sub_wild,]
        dat_rs_t <- rbind(dat_rs_t2, dat_rs_t[r_sub_wild,])
      }
      
      # loop over subbasins and search for veg with wildcards and put them at the end of the respective subbasin
      for (s in unique(dat_rs_t$subbas_id)) {
        rows2 <- which(dat_rs_t$subbas_id == s)
        dat_rs_t2 <- dat_rs_t[rows2,]
        r_veg_wild <- which(dat_rs_t2$veg_id == -1)
        if (any(r_veg_wild)) {
          dat_rs_t3 <- dat_rs_t2[-r_veg_wild,]
          dat_rs_t2 <- rbind(dat_rs_t3, dat_rs_t2[r_veg_wild,])
        }
        
        dat_rs_t[rows2,] <- dat_rs_t2
      }
      
      dat_rs[rows,] <- dat_rs_t
    }
    
    # write output
    dat_out <- dat_rs[,c("subbas_id", "svc_id", "yearm", "node1", "node2", "node3", "node4")]
    write.table(dat_out, paste(dest_dir, "Hillslope", tfile, sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
      } #loop through parameters
    if(verbose) message("% OK")
    } 
  } # Hillslope/x_seasons.dat
  
  
### Hillslope/soil_particles.dat #####
  if("Hillslope/soil_particles.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Hillslope/soil_particles.dat ...")
    
    # create file
    if(!file.exists(paste(dest_dir, "Hillslope/soil_particles.dat", sep="/")) | overwrite){
      file.create(paste(dest_dir, "Hillslope/soil_particles.dat", sep="/"))
    } else {
      stop("File 'Hillslope/soil_particles.dat' exists!")
    }
    
    # write header
    writeLines(con=paste(dest_dir, "Hillslope/soil_particles.dat", sep="/"),
               text=c("Particle size distribution of topmost horizons of soils",
                      "soil_id\tpart_class_id\tfraction[-]"))
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("soil_veg_components", "r_tc_contains_svc", "r_soil_contains_particles"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=F))
    
    # only soil types occurring in soil_veg_components and r_tc_contains_svc will be considered
    dat_contains_part <- dat_all$r_soil_contains_particles
    dat_svc <- dat_all$soil_veg_components
    r_svc_out <- which(!(dat_svc$pid %in% dat_all$r_tc_contains_svc$svc_id))
    if(any(r_svc_out))
      dat_svc <- dat_svc[-r_svc_out,]
    
    r_soil_out <- which(!(dat_contains_part$soil_id %in% dat_svc$soil_id))
    if(any(r_soil_out)) {
      warning(paste0("Soil types ", paste0(unique(dat_contains_part$soil_id[r_soil_out]), collapse=", "), " from table 'r_soil_contains_particles' are not in table 'soil_veg_components', or the respective SVCs are not in 'r_tc_contains_svc', and will be ignored."))
      dat_contains_part <- dat_contains_part[-r_soil_out,]
    }
    
    frac_sums <- round(tapply(dat_contains_part$fraction, dat_contains_part$soil_id, sum),3)
    if(any(frac_sums != 1))
      stop("Could not successfully write file Hillslope/soil_particles.dat. In table 'r_soil_contains_particles' not all fractions sum up to 1.")
    
    # write output
    write.table(dat_contains_part, paste(dest_dir, "Hillslope/soil_particles.dat", sep="/"), append=T,
                quote=F, sep="\t", row.names=F, col.names=F)
    
    if(verbose) message("% OK")
    
  } # Hillslope/soil_particles.dat
  
  
### Reservoir/reservoir.dat #####
  if("Reservoir/reservoir.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Reservoir/reservoir.dat ...")
    
    # create file
    if(!file.exists(paste0(dest_dir, "/Reservoir/reservoir.dat")) | overwrite){
      file.create(paste0(dest_dir, "/Reservoir/reservoir.dat"))
    } else {
      stop("File 'Reservoir/reservoir.dat' exists!")
    }
    
    # prepare output file
    header_str <- "Subasin-ID, minlevel[m], maxlevel[m], vol0([1000m**3]; unknown=-999), storcap[1000m**3], damflow[m**3/s], damq_frac[-], withdrawal[m**3/s], damyear[YYYY], maxdamarea[ha], damdead[1000m**3], damalert[1000m**3], dama[-], damb[-], qoutlet[m**3/s], fvol_bottom[-], fvol_over[-], damc[-], damd[-], elevbottom[m]"

    write(file=paste0(dest_dir, "/Reservoir/reservoir.dat"),
          x=c("Specification of reservoir parameters", header_str))


    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("reservoirs_strategic"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=FALSE))
    
    dat_all[["reservoirs_strategic"]] = dat_all[["reservoirs_strategic"]][, c("pid", 
                          "minlevel",
                          "maxlevel",
                          "vol0",
                          "storecap",
                          "damflow",
                          "damq_frac",
                          "withdrawal",
                          "damyear",
                          "maxdamarea",
                          "damdead",
                          "damalert",
                          "dama",
                          "damb",
                          "q_outlet",
                          "fvol_botm",
                          "fvol_over",
                          "damc",
                          "damd",
                          "elevbottom")]
    
    
    # write output
    # write data
    write.table(dat_all[["reservoirs_strategic"]], paste0(dest_dir, "/Reservoir/reservoir.dat"), append=T, quote=F,
                sep="\t", row.names=F, col.names=F)
    
    
    if(verbose) message("% OK")
    
  } # reservoir/reservoir.dat
  
### Reservoir/lake.dat #####
  if("Reservoir/lake.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Reservoir/lake.dat ...")
    
    # create file
    if(!file.exists(paste0(dest_dir, "/Reservoir/lake.dat")) | overwrite){
      file.create(paste0(dest_dir, "/Reservoir/lake.dat"))
    } else {
      stop("File 'Reservoir/lake.dat' exists!")
    }
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("reservoirs_small_classes"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=FALSE))
    
        tt = dat_all[["reservoirs_small_classes"]]
        tt$name=NULL #omit "name" column

    # write output
    header_str <- "Reservoir_class-ID, maxlake0[m**3], lake_vol0_factor[-], lake_change[-], alpha_Molle[-], damk_Molle[-], damc_hrr[-], damd_hrr[-]"
    write(file=paste0(dest_dir, "/Reservoir/lake.dat"),
              x=c("Specification of parameters for the reservoir size classes", header_str))
        # write data
    write.table(tt, paste0(dest_dir, "/Reservoir/lake.dat"), append=T, quote=F,
                sep="\t", row.names=F, col.names=F)
 
    if(verbose) message("% OK")
    
  } # reservoir/lake.dat
  
### Reservoir/lake_number.dat #####
  if("Reservoir/lake_number.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Reservoir/lake_number.dat ...")
    
    # create file
    if(!file.exists(paste0(dest_dir, "/Reservoir/lake_number.dat")) | overwrite){
      file.create(paste0(dest_dir, "/Reservoir/lake_number.dat"))
    } else {
      stop("File 'Reservoir/lake_number.dat' exists!")
    }
    
  # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("r_subbas_contains_reservoirs_small"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=FALSE))
    tt = dat_all[["r_subbas_contains_reservoirs_small"]][,c("subbas_id", "res_class_id", "n_reservoirs")]
    dat_out = reshape(data=tt, direction = "wide", v.names = "n_reservoirs", idvar = "subbas_id", timevar = "res_class_id")
    
    # prepare output file
    header_str <- paste0("Sub-basin-ID, acud[-] (", ncol(dat_out)-1," reservoir size classes)")

    
    write(file=paste0(dest_dir, "/Reservoir/lake_number.dat"),
          x=c("Specification of total number of reservoirs in the size classes", header_str))
    
    # write output
    write.table(dat_out, paste0(dest_dir, "/Reservoir/lake_number.dat"), append=T, quote=F,
                sep="\t", row.names=F, col.names=F)
    
    
    if(verbose) message("% OK")
    
  } # reservoir/lake_number.dat
  
### Reservoir/lake_maxvol.dat #####
  if("Reservoir/lake_maxvol.dat" %in% files) {
    if(verbose) message("%")
    if(verbose) message("% Create Reservoir/reservoir.dat ...")
    
    # create file
    if(!file.exists(paste0(dest_dir, "/Reservoir/lake_maxvol.dat")) | overwrite){
      file.create(paste0(dest_dir, "/Reservoir/lake_maxvol.dat"))
    } else {
      stop("File 'Reservoir/lake_maxvol.dat' exists!")
    }
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("r_subbas_contains_reservoirs_small"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=FALSE))
    tt = dat_all[["r_subbas_contains_reservoirs_small"]][,c("subbas_id", "res_class_id", "maxlake")]
    dat_out = reshape(data=tt, direction = "wide", v.names = "maxlake", idvar = "subbas_id", timevar = "res_class_id")
    
    # prepare output file
    header_str <- paste0("Sub-basin-ID, maxlake[m**3]  (", ncol(dat_out)-1," reservoir size classes)")
    
    
    write(file=paste0(dest_dir, "/Reservoir/lake_maxvol.dat"),
          x=c("Specification of water storage capacity for the reservoir size classes", header_str))
    
    # write output
    write.table(round(dat_out, digits = 1), paste0(dest_dir, "/Reservoir/lake_maxvol.dat"), append=T, quote=F,
                sep="\t", row.names=F, col.names=F)
    
    
    
    if(verbose) message("% OK")
    
  } # reservoir/lake_maxvol.dat
  
  
  

   
###############################################################################
### end of function, write changes into 'meta_info', close connection

  # update table meta_info
  meta_dat <- sqlFetch(con, "meta_info")
  if(any(meta_dat$pid)) {
    pid_new <- max(meta_dat$pid) +1
  } else {
    pid_new <- 1
  }
  meta_out <- data.frame(pid=pid_new,
                         mod_date=as.POSIXct(Sys.time()),
                         mod_user=paste0("db_wasa_input(), v. ", installed.packages()["lumpR","Version"]),
                         affected_tables=paste0("WASA files: ", paste(files, collapse=", ")),
                         affected_columns="none",
                         remarks=paste0("WASA input files written using R package lumpR to ", dest_dir, " ."))
  write_datetabs(con, meta_out, tab="meta_info", verbose)



  if(verbose) message("%")
  if(verbose) message("% All files written successfully. Closing ODBC connection...")
  
  tryCatch(odbcClose(con), error=function(e){})
  
  if(verbose) message("%")
  if(verbose) message("% DONE!")
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")

} # EOF