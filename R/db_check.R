# LUMP/db_check.R
# Copyright (C) 2015,2016 Tobias Pilz, Till Francke
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


#'  Check parameter database for consistency
#'  
#'  Function includes several options to check and correct the parameter database for completeness
#'  and consistency.
#'  
#'  @param dbname Name of the data source (DSN) registered at ODBC.
#'  
#'  @param check Character vector specifying what shall be checked. See \code{Details}.
#'  
#'  @param option A list of options for certain checks. See \code{Details}
#'  for the different options and their meaning.
#'  
#'  @param fix \code{logical}. If \code{FALSE} (the \code{default}) a report of the
#'  selected checks will be created. The database will not be touched. If \code{TRUE}
#'  canges to the database according to the selected checks will be made.
#'  
#'  @param verbose \code{logical}. Should detailed information during execution be
#'  printed? When \code{TRUE} (the \code{default}) output of writing updated values
#'  into database can be rather long so you might want to direct output into an
#'  external log file. Always set to \code{TRUE} if \code{fix = FALSE}.
#'  
#'  @details
#'  The following checks are currently included and can be specified via argument \code{checks}.
#'  Execute checks in pre-defined order as some checks build upon each other and
#'  lead to erroneous results when interchanged. However, some checks might be
#'  unnecessary for your purpose and can be left out.
#'  
#'  \bold{filter_small_areas}\cr
#'  Tiny areas as result of landscape disaggregation considered irrelevant during model
#'  application will be removed and the areal fractions updated accordingly. The model
#'  will run faster by removing unnecessary computational burden. Namely, this affects LUs 
#'  within subbasins, TCs within LUs and SVCs within TCs.
#'  If the fraction to be removed is greater than 10 \% of the total area of the next higher 
#'  spatial level's class, datasets will be kept. In this case you might try a smaller value for area_thresh'.\cr
#'  \emph{Option: 'area_thresh'}\cr
#'  A threshold defining the minimum areal fraction of a certain spatial disaggregation
#'  unit within the next higher spatial level (e.g. LUs within subbasins). Default: 0.01.
#'  
#'  \bold{tc_slope}\cr
#'  In WASA (and other models probably as well) slopes equal to or less than zero are
#'  not allowed. This check helps to identify such TCs and provides the following option
#'  to solve the issue:\cr
#'  \emph{Option: 'treat_slope'}\cr
#'  A numeric vector with two or three elements:\cr
#'  The first being one of \{1,2,3\}:\cr
#'  1: Remove TCs with slope <= 0 from 'r_lu_contains_tc' whereas areal fraction within
#'  the LU must be smaller than a defined threshold (second value of the vector).\cr
#'  2: Where slope is 0, change it to small positive value specified as second
#'  value of the vector (interpreted as slope in \%).\cr
#'  3: A combination of the two former choices whereas option 1 is applied before
#'  option 2, the second value of the vector defining the areal threshold and the
#'  third giving the slope replacement value. This is the default setting with
#'  threshold = 0.01 and slope = 0.1 \%.
#'  
#'  \bold{special_areas}\cr
#'  Define certain Soil-Vegetation Components as special areas via column 'special_area'
#'  in table 'soil_veg_components' inferred from table 'vegetation' and/or 'soils'. 
#'  Currently values '0' for an ordinary SVC, '1' for water areas, and '2' for
#'  impervious surfaces are supported.\cr
#'  \emph{Option: 'special_area'}\cr
#'  A \code{data.frame} with 3 named vectors:\cr
#'  'reference_tbl': \code{character} giving the name(s) of the database table(s)
#'  containing the special area(s) to be mapped to SVCs. Tables 'vegetation' and
#'  'soils' are supported.\cr
#'  'ref_id': \code{integer} giving the 'pid' of the special area within the specified
#'  reference table.\cr
#'  'special_id': \code{integer} giving the flag of column 'special_area' in table
#'  'soil_veg_components'. Currently recognised in further processing are '0' for
#'  ordinary SVCs, '1' for water areas, and '2' for impervious surfaces.\cr
#'  For instance, if within table 'vegetation' you have classes with pid 3 and 4 
#'  indicating water surfaces and in 'soils' pid 10 is an impervious surface
#'  specify the following: \code{special_area = data.frame(reference_tbl=
#'  c("vegetation", "vegetation", "soils"), ref_id=c(3,4,10), special_id=c(1,1,2))}.
#'  
#'  \bold{remove_water_svc}\cr
#'  Remove SVCs marked as water from table 'r_tc_contains_svc', i.e. those SVCs where
#'  in table 'soil_veg_components' column 'special_area' is equal to 1. Areal fractions
#'  will be updated (normalized to 1). Requires column 'special_area' to be set correctly
#'  (e.g. by running option 'special_area' before)
#'  
#'  \bold{compute_rocky_frac}\cr
#'  Compute rocky fractions, i.e. fractions of impervious surfaces, for TCs (table
#'  'terrain_components', column 'frac_rocky') from impervious SVCs (column 'special_area'
#'  in 'soil_veg_components' equal to 2) and topmost soil horizons (in table horizons'
#'  column 'position' equal to 1 and 'coarse_frag' equal to 1). These undergo special
#'  treatment in the WASA model. SVCs with soil profile containing 100\% coarse
#'  fragments in topmost horizon will be marked as impervious.
#'  If the column 'frac_rocky' already contains values, the computed values are added to these.
#'  In any case, this step should be followed by 'remove_impervious_svc'.
#'  
#'  \bold{remove_impervious_svc}\cr
#'  Remove SVCs marked as impervious from table 'r_tc_contains_svc', i.e. those SVCs where
#'  in table 'soil_veg_components' column 'special_area' is equal to 2. Areal fractions
#'  will be updated.
#'  
#'  \bold{proxy_frgw_delay}\cr
#'  Estimate storage coefficient for groundwater delay ('frgw_delay') in \emph{days}
#'  for each LU based on a proxy and manually specified total mean groundwater delay
#'  in \emph{days}. The proxy is estimated from average slope length, slope and rocky
#'  fraction according to empirical formula: \code{proxy = slopelength * (1 - frac_rocky)
#'  / slope}. And finally: \code{frgw_delay = proxy * total_mean_delay / mean(proxy)}.
#'  Existing values of 'frgw_delay' will be overwritten.\cr
#'  \emph{Option: 'total_mean_delay'}\cr
#'  Total mean groundwater delay in \emph{days} estimated a priori for the whole
#'  catchment (e.g. from baseflow analysis). All proxy values are scaled, so their 
#'  mean matches this value (see formula above).
#'  
#'  \bold{delete_obsolete}\cr
#'  Delete obsolete datasets. I.e. special area SVCs, LUs not in any subbasin, TCs
#'  not in any LU, and SVCs not in any TC.
#'  
#'  \bold{completeness}\cr
#'  Check database for completeness. I.e. check if all IDs in the \emph{contains}-tables
#'  exist within the repective referenced tables. Furthermore, check if all vegetation
#'  types and soils exist as referenced within 'soil_veg_components', and if all
#'  soils exist as referenced within 'horizons'.
#'  If the check fails, function returns an error. If \code{fix=T}, additionally a warning
#'  message will be writting into table 'meta_info' of the databse.
#'  
#'  \bold{subbasin_order}\cr
#'  Compute subbasin order for WASA's routing input file routing.dat. Order will
#'  be defined in column 'a_stream_order' of table 'subbasins'.
#'  
#'  
#'  @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'  
#'  @export

db_check <- function(
  dbname,
  check = c("filter_small_areas", "tc_slope", "special_areas", "remove_water_svc",
            "compute_rocky_frac", "remove_impervious_svc", "proxy_frgw_delay",
            "delete_obsolete", "completeness", "subbasin_order"),
  option = list(area_thresh=0.01,
                treat_slope=c(3,0.01,0.1)),
  fix=F,
  verbose=TRUE
) {
  
  # if fix = F (default) set verbose = T
  if(!fix) {
    verbose <- T
    print("START database checks in report mode, i.e. fix = FALSE and database will not be touched.")
    print("")
  }
    
  
  if (verbose)
    print("Loading package 'RODBC' and connecting to database ...")
  
  # connect to ODBC registered database
  suppressWarnings(con <- odbcConnect(dbname, believeNRows=F))
  
  if (con == -1)
    print(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC.",
                 " If you want to connect to a MS Access database make sure you are using 32 bit R."))
  
  if(verbose)
    print("OK.")
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # initialise vector to keep track of changed tables
  tbl_changed <- NULL
  
  
###############################################################################
### check current db version
  if(verbose)
    print("Check database version ...")

  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="LUMP")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))

  db_ver <- max(sqlFetch(con, "db_version")$version)
  if(db_ver < db_ver_max) {
    odbcClose(con)
    stop(paste0("Database version is prior to version ", db_ver_max, ". Make sure you use the latest database version (consider function db_update())!"))
  }
  if(verbose)
    print("OK.")
  
###############################################################################
### filter small areas (use external function for every relevant table)
  if (any(grepl("filter_small_areas", check))) {
    if(verbose)
      print("Filter small areas ...")
    
    if(!("area_thresh" %in% names(option))) {
      odbcClose(con)
      stop("No option 'area_thresh' specified for check 'filter_small_areas'.")
    }
    
    thres <- option[["area_thresh"]]
    
    # LUs
    tbl_ch <- filter_small_areas(con=con, table="r_subbas_contains_lu", thres=thres, fix=fix, verbose=verbose, tbl_changed=tbl_changed)
    tbl_changed <- c(tbl_changed, tbl_ch)
    
    # TCs
    tbl_ch <- filter_small_areas(con=con, table="r_lu_contains_tc", thres=thres, fix=fix, verbose=verbose, tbl_changed=tbl_changed)
    tbl_changed <- c(tbl_changed, tbl_ch)
    
    # SVCs
    tbl_ch <- filter_small_areas(con=con, table="r_tc_contains_svc", thres=thres, fix=fix, verbose=verbose, tbl_changed=tbl_changed)
    tbl_changed <- c(tbl_changed, tbl_ch)
    
    if(verbose)
      print("OK.")
  } # filter small areas


###############################################################################
### TC with slope <= 0
  if (any(grepl("tc_slope", check))) {
    if(verbose)
      print("Find TCs with slope <= 0 ...")
    
    # get data
    dat_tc <- sqlFetch(con, "terrain_components")
    if(!any(which(dat_tc$slope <= 0))) {
      print("-> There are no TCs with slope <= 0.")
    } else {
      
      if(!("treat_slope" %in% names(option))) {
        # update table meta_info
        if(fix) {
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check tc_slope. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
        }
        odbcClose(con)
        stop("No option 'treat_slope' specified for check 'tc_slope'.")
      }
      
      # data from r_lu_contains_tc
      dat_lutc <- sqlFetch(con, "r_lu_contains_tc")
      
      # datasets of r_lu_contains_tc where slope is <= 0
      r_tc_zero <- which(dat_tc$slope <= 0)
      tc_zero <- dat_tc$pid[r_tc_zero]
      lutc_zero <- dat_lutc[which(dat_lutc$tc_id %in% tc_zero),]
      dat_out <- dat_lutc[-which(dat_lutc$tc_id %in% tc_zero),]
      
    
      # option 1: remove TC if areal fraction within LU is below threshold
      if (option[["treat_slope"]][1] == 1 | option[["treat_slope"]][1] == 3) {
        
        if (verbose)
          print("-> Option 1: Remove TCs with small fraction within LU...")
        
        # identify datasets below fraction threshold
        rows_lutc_rm <- which(lutc_zero$fraction < option[["treat_slope"]][2])
        
        if (!any(rows_lutc_rm)) {
          print("-> No TCs with fraction below defined threshold. No TCs will be removed.")
        } else {
          
          if(fix) {
            print("-> The following datasets will be removed from 'r_lu_contains_tc' ('fraction' will be updated):")
          } else {
            print("-> The following datasets in 'r_lu_contains_tc' contain slopes <= 0 and fractions below threshold:")
          }
          print(lutc_zero[rows_lutc_rm,])
          
          # keep datasets where TCs equal to more than 10% of the respective parent class' area would be removed
          rm_sum <- tapply(lutc_zero$fraction[rows_lutc_rm], list(parent=lutc_zero[[1]][rows_lutc_rm]), sum)
          if(any(rm_sum > 0.1)) {
            keep <- which(rm_sum > 0.1)
            print(paste0("-> For '", colnames(lutc_zero)[1], "' ", paste(names(rm_sum)[keep], collapse=", "),
                         " more than 10% of the area would be removed due to too many small '", colnames(lutc_zero)[2], ". These datasets will be kept."))
            
            rows_rm_keep <- which(lutc_zero[[1]][rows_lutc_rm] %in% names(rm_sum)[keep])
            rows_lutc_rm <- rows_lutc_rm[-rows_rm_keep]
          }
          
          # check if some datasets are left to remove
          if (!any(rows_lutc_rm)) {
            print(paste0("-> Nothing left to remove or choose smaller value for threshold and re-run check."))
          } else {
            
            # remove datasets with small fraction
            lutc_zero <- lutc_zero[-rows_lutc_rm,]
            
            # merge unchanged datasets with adjusted datasets where slope of TC <= 0
            dat_out <- rbind(dat_out, lutc_zero)
            
            # re-calculate fractions
            frac_sum <- tapply(dat_out$fraction, list(parent=dat_out[[1]]), sum)
            for (s in 1:nrow(dat_out))
              dat_out$fraction[s] <- dat_out$fraction[s] / frac_sum[paste0(dat_out[[1]][s])]
            
          } # still datasets left to remove?
        } # TCs wih fraction below threshold?
  
      } # end of option 1
      
      
      
      # option 2: change slope to specified small positive value
      if (option[["treat_slope"]][1] == 2 | option[["treat_slope"]][1] == 3) {
        
        if (verbose)
          print("-> Option 2: Change slope to specified small positive value...")
        
        # determine replace value
        if (option[["treat_slope"]][1] == 2)
          repl_slope <- option[["treat_slope"]][2]
        if (option[["treat_slope"]][1] == 3)
          repl_slope <- option[["treat_slope"]][3]
        
        if(fix) {
          print(paste("-> For the following datasets slopes in 'terrain_components' will be replaced by ", repl_slope, ":"))
        } else {
          print("-> The following datasets in 'terrain_components' contains slopes <= 0:")
        }
          print(dat_tc[r_tc_zero,])
          
        # replace slope value
        dat_tc[r_tc_zero,"slope"] <- repl_slope
      } # end of option 2
      
      
      
      # update database
      if(fix) {
        
        if(verbose)
          print("-> Updating table 'r_lu_contains_tc'...")
        tryCatch(
        {
          sqlQuery(con, "delete from r_lu_contains_tc")
          sqlSave(channel=con, tablename = "r_lu_contains_tc", dat=dat_out, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "r_lu_contains_tc")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check tc_slope. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating table 'r_lu_contains_tc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
        if(verbose)
          print("-> Updating table 'terrain_components'...")
        tryCatch(
        {
          sqlQuery(con, "delete from terrain_components")
          sqlSave(channel=con, tablename = "terrain_components", dat=dat_tc, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "terrain_components")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check tc_slope. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating table 'terrain_components'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix
    
    } # TC with slope <= 0?
    
    if(verbose)
      print("OK.")
    
  } # check tc_slope?


###############################################################################
### Assign special areas (water, impervious surfaces)
  if (any(grepl("special_areas", check))) {
    if(verbose)
      print("Define certain SVCs as special areas ...")
    
    # check arguments
    if(!("special_area" %in% names(option))) {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("No option 'special_area' specified for check 'special_areas'.")
    }
    
    if(class(option$special_area) != "data.frame") {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("Option 'special_area' is not a data.frame.")
    }
    
    if(any(!(c("reference_tbl", "ref_id", "special_id") %in% names(option$special_area)))) {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("Option 'special_area' does not contain all necessary named vectors.")
    }
    
    if(any(!(unique(option$special_area$reference_tbl) %in% c("vegetation", "soils")))) {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("Option 'special_area' vector 'reference_tbl' supports values 'vegetation' and 'soils' only.")
    }
    
    if(any(!(unique(option$special_area$special_id) %in% c(0,1,2)))) {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("Option 'special_area' vector 'special_id' supports values '0', '1', and '2' only.")
    }
    
    
    
    # get SVC data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    changes_made=FALSE
    
    # vegetation
    if("vegetation" %in% option$special_area$reference_tbl) {
      
      # determine relevant rows and vegetation ids
      rows_veg <- grep("vegetation", option$special_area$reference_tbl)    
      
      # loop over rows for vegetation
      for (r in rows_veg) {
        
        # get rows in dat_svc to be adjusted
        svc_rows_adj <- which(dat_svc$veg_id == option$special_area$ref_id[r])
        
        if(!any(svc_rows_adj)){
          print(paste0("-> ATTENTION: Option 'special_area': Vegetation id ", option$special_area$ref_id[r], " could not be found in column 'veg_id' of table 'soil_veg_components'."))
          next
        }
          
        # set special_area flag
        dat_svc$special_area[svc_rows_adj] <- option$special_area$special_id[r] 
        changes_made=TRUE
      }
      
    } # end vegetation
    
    
    # soils
    if("soils" %in% option$special_area$reference_tbl) {
      
      # determine relevant rows and soil ids
      rows_soil <- grep("soils", option$special_area$reference_tbl)    
      
      # loop over rows for soil
      for (r in rows_soil) {
        
        # get rows in dat_svc to be adjusted
        svc_rows_adj <- which(dat_svc$soil_id == option$special_area$ref_id[r])
        
        if(!any(svc_rows_adj)){
          print(paste0("-> ATTENTION: Option 'special_area': Soil id ", option$special_area$ref_id[r], " could not be found in column 'soil_id' of table 'soil_veg_components'."))
          next
        }
        
        # set special_area flag
        dat_svc$special_area[svc_rows_adj] <- option$special_area$special_id[r] 
        changes_made=TRUE
      }
      
    } # end soils
    
    
    # update table
    
    if(changes_made & fix)
    {
      if(verbose)
        print("-> Updating table 'soil_veg_components'...")
      tryCatch(
      {
        sqlQuery(con, "delete from soil_veg_components")
        sqlSave(channel=con, tablename = "soil_veg_components", dat=dat_svc, verbose=F, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
        tbl_changed <- c(tbl_changed, "soil_veg_components")
        if(verbose) print("Table 'soil_veg_components' updated")
      }, error = function(e) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop(paste0("An error occured when updating table 'soil_veg_components'. ",
                    "Error message of the writing function: ", e))
      })
    }  
    else
      if(verbose)
        print("Nothing done.")
    
    if(verbose)
      print("OK.")
    
  } # check special_areas





###############################################################################
### remove water SVCs
  if (any(grepl("remove_water_svc", check))) {
    if(verbose & fix)
      print("Remove SVCs marked as water ...")
    
    if(!fix)
      print("Identify SVCs marked as water ...")
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    # identify water SVCs
    rows_water <- which(dat_svc$special_area == 1)
    svc_water <- dat_svc$pid[rows_water]
    rows_contains_water <- which(dat_contains$svc_id %in% svc_water)
    
    if (length(rows_contains_water)==0)
    {
      print("-> No water-SVCs found, nothing done.")          
    } else {
    
      if(!fix){
        print("-> The following datasets in 'r_tc_contains_svc' were identified as water areas:")
        print(dat_contains[rows_contains_water,])
      } else {
        print("-> The following datasets will be removed from 'r_tc_contains_svc' ('fraction' will be updated):")
        print(dat_contains[rows_contains_water,])
      
        # remove water SVCs
        dat_contains_act <- dat_contains[-rows_contains_water,]
        
        # update fractions
        frac_sum <- tapply(dat_contains_act$fraction, list(parent=dat_contains_act$tc_id), sum)
        for (s in 1:nrow(dat_contains_act))
          dat_contains_act$fraction[s] <- dat_contains_act$fraction[s] / frac_sum[paste0(dat_contains_act$tc_id[s])]
        
        
        # update database
        if(verbose)
          print("-> Updating table 'r_tc_contains_svc'...")
        tryCatch(
        {
          sqlQuery(con, "delete from r_tc_contains_svc")
          sqlSave(channel=con, tablename = "r_tc_contains_svc", dat=dat_contains_act, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "r_tc_contains_svc")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check remove_water_svc. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating table 'r_tc_contains_svc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix
      
    } # if water SVCs found
    
    
    if(verbose)
      print("OK.")
    
  } # check remove_water_svc




###############################################################################
### compute rocky fractions for TCs
  if (any(grepl("compute_rocky_frac", check))) {
    if(verbose)
      print("Compute rocky (i.e. impervious) fractions for TCs ...")
    
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_tc <- sqlFetch(con, "terrain_components")
    dat_hor <- sqlFetch(con, "horizons")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    dat_tc$frac_rocky[is.na(dat_tc$frac_rocky)] = 0 #set NAs to 0
    if (max(dat_tc$frac_rocky)>0)
      print("-> ATTENTION: Column 'frac_rocky' in table 'terrain_components' already contains some values. The coputed fractions will be added to these.")

    # identify soils with 100% coarse fragments in topmost horizon
    soil_impervious <- dat_hor$soil_id[which(dat_hor$position == 1 & dat_hor$coarse_frag == 1)]
    
    # mark corresponding SVCs as impervious
    if(any(soil_impervious)) {
      rows_svc_impervious <- which(dat_svc$soil_id %in% soil_impervious)
      
      if(fix) {
        print("-> The following datasets in 'soil_veg_components' will be marked as impervious due to 100% coarse fragments in topmost soil horizon:")
      } else {
        print("-> The following datasets in 'soil_veg_components' were identified as impervious due to 100% coarse fragments in topmost soil horizon:")
      }
      print(dat_svc[rows_svc_impervious,])
      
      dat_svc$special_area[rows_svc_impervious] <- 2
    } else {
      if (verbose)
        print("-> No topmost horizon with 100% coarse fragments could be found.")
    }
    
    # identify TCs containing impervious SVCs
    svc_impervious <- dat_svc$pid[which(dat_svc$special_area == 2)]
    rows_tc_impervious <- which(dat_contains$svc_id %in% svc_impervious)
    
    # compute frac_rocky for every TC
    tc_rocky <- tapply(dat_contains$fraction[rows_tc_impervious], list(parent=dat_contains$tc_id[rows_tc_impervious]), sum)
    for (t in 1:length(tc_rocky)) {
      row <- which(dat_tc$pid == names(tc_rocky)[t])
      dat_tc$frac_rocky[row] <- dat_tc$frac_rocky[row] + tc_rocky[t]
    }
    
    
    # update database
    if(fix) {
      
      if(verbose)
        print("-> Updating table 'terrain_components'...")
      tryCatch(
      {
        sqlQuery(con, "delete from terrain_components")
        sqlSave(channel=con, tablename = "terrain_components", dat=dat_tc, verbose=F, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
        tbl_changed <- c(tbl_changed, "terrain_components")
      }, error = function(e) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check compute_rocky_frac. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop(paste0("An error occured when updating table 'terrain_components'. ",
                    "Error message of the writing function: ", e))
      }
      )
    
    } # if fix
    
    if(verbose)
      print("OK.")
    
  } # check compute_rocky_frac
  





###############################################################################
### remove impervious SVCs
  if (any(grepl("remove_impervious_svc", check))) {
    if(verbose & fix)
      print("Remove SVCs marked as impervious ...")
    
    if(!fix)
      print("Identify SVCs marked as impervious ...")
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    # identify impervious SVCs
    rows_impervious <- which(dat_svc$special_area == 2)
    svc_impervious <- dat_svc$pid[rows_impervious]
    rows_contains_impervious <- which(dat_contains$svc_id %in% svc_impervious)
    
    if(!any(rows_contains_impervious)) {
      print("-> No impervious SVCs could be found. Nothing done.")
    } else {
    
      if(!fix) {
        print("-> The following datasets in 'r_tc_contains_svc' were identified as impervious:")
        print(dat_contains[rows_contains_impervious,])
      } else {
        print("-> The following datasets will be removed from 'r_tc_contains_svc' ('fraction' will be updated):")
        print(dat_contains[rows_contains_impervious,])
        
        # remove impervious SVCs
        dat_contains_act <- dat_contains[-rows_contains_impervious,]
        
        # update fractions
        frac_sum <- tapply(dat_contains_act$fraction, list(parent=dat_contains_act$tc_id), sum)
        for (s in 1:nrow(dat_contains_act))
          dat_contains_act$fraction[s] <- dat_contains_act$fraction[s] / frac_sum[paste0(dat_contains_act$tc_id[s])]
        
        
        # update database
        if(verbose)
          print("-> Updating table 'r_tc_contains_svc'...")
        tryCatch(
        {
          sqlQuery(con, "delete from r_tc_contains_svc")
          sqlSave(channel=con, tablename = "r_tc_contains_svc", dat=dat_contains_act, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "r_tc_contains_svc")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check remove_impervious_svc. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating table 'r_tc_contains_svc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix
      
    } # if any impervious SVC
  
  
    if(verbose)
      print("OK.")
  
  } # check remove_impervious_svc
  




###############################################################################
### estimate frgw_delay
  if (any(grepl("proxy_frgw_delay", check))) {
    if(verbose)
      print("Estimate frgw_delay for LUs ...")
    
    # check arguments
    if(!("total_mean_delay" %in% names(option))) {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check proxy_frgw_delay. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("No option 'total_mean_delay' specified for check 'proxy_frgw_delay'.")
    }
    
    if(!fix) {
      print("-> Running in report mode. Nothing to report for this check.")
    } else {
    
      # get data
      dat_tc <- sqlFetch(con, "terrain_components")
      dat_lu <- sqlFetch(con, "landscape_units")
      dat_contains <- sqlFetch(con, "r_lu_contains_tc")
      
      # compute area-weighted average slope for every LU
      dat_contains_t <- merge(dat_contains, dat_tc, by.y="pid", by.x="tc_id")
      slope_lu <- sapply(split(dat_contains_t, dat_contains_t$lu_id), function(x) weighted.mean(x$slope, x$fraction))
      dat_lu_t <- merge(dat_lu, data.frame(id=names(slope_lu), slope=slope_lu), by.x="pid", by.y="id")
      
      # compute area-weighted average rocky fraction for every lu
      rocky_lu <- sapply(split(dat_contains_t, dat_contains_t$lu_id), function(x) weighted.mean(x$frac_rocky, x$fraction))
      dat_lu_t <- merge(dat_lu_t, data.frame(id=names(rocky_lu), rocky=rocky_lu), by.x="pid", by.y="id")
      
      # estimate proxy
      proxy <- dat_lu_t$slopelength * (1-dat_lu_t$rocky) / dat_lu_t$slope
      
      # in case of zero slopes set infinite proxy values to 10000 days
      if(any(is.infinite(proxy)))
        proxy[which(is.infinite(proxy))] <- 10000
      
      # estimate frgw_delay
      dat_lu$frgw_delay <- proxy * option$total_mean_delay / mean(proxy)
      
      
      # update database
      if(verbose)
        print("-> Updating table 'landscape_units'...")
      tryCatch(
      {
        sqlQuery(con, "delete from landscape_units")
        sqlSave(channel=con, tablename = "landscape_units", dat=dat_lu, verbose=F, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
        tbl_changed <- c(tbl_changed, "landscape_units")
      }, error = function(e) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check proxy_frgw_delay. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop(paste0("An error occured when updating table 'landscape_units'. ",
                    "Error message of the writing function: ", e))
      }
      )
      
    } # if fix
    
    if(verbose)
      print("OK.")
    
  } # check proxy_frgw_delay
  



###############################################################################
### delete obsolete datasets
  if (any(grepl("delete_obsolete", check))) {
    
    if(fix & verbose)
      print("Delete obsolete datasets ...")
    
    if(!fix)
      print("Identify obsolete datasets ...")
    
    
    ### LUs not in any subbasin
    
    # get data
    dat_sub_contains <- sqlFetch(con, "r_subbas_contains_lu")
    dat_lu <- sqlFetch(con, "landscape_units")
    
    # identify LUs not in any subbasin
    r_del <- which(!(dat_lu$pid %in% dat_sub_contains$lu_id))
    
    if(any(r_del)) {
      
      # get lu_contains_tc data
      dat_lu_contains <- sqlFetch(con, "r_lu_contains_tc")
      
      # identify datasets with LUs that will be deleted
      r_del_contains <- which(dat_lu_contains$lu_id %in% dat_lu$pid[r_del])
      
      if(!fix) {
        print("-> The following datasets in 'landscape_units' are obsolete:")
        print(dat_lu[r_del,])
        print("... affecting the following datasets in 'r_lu_contains_tc':")
        print(dat_lu_contains[r_del_contains,])
      } else {
          
        print("-> The following datasets will be removed from 'landscape_units':")
        print(dat_lu[r_del,])
        
        print("... affecting the following datasets in 'r_lu_contains_tc' that will be deleted as well (fractions will be updated):")
        print(dat_lu_contains[r_del_contains,])
        
        dat_lu <- dat_lu[-r_del,]
        dat_lu_contains <- dat_lu_contains[-r_del_contains,]
        
        # update fractions
        frac_sum <- tapply(dat_lu_contains$fraction, list(parent=dat_lu_contains$lu_id), sum)
        for (s in 1:nrow(dat_lu_contains))
          dat_lu_contains$fraction[s] <- dat_lu_contains$fraction[s] / frac_sum[paste0(dat_lu_contains$lu_id[s])]
        
        
        # update database
        tryCatch(
        {
          sqlQuery(con, "delete from landscape_units")
          sqlSave(channel=con, tablename = "landscape_units", dat=dat_lu, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          sqlQuery(con, "delete from r_lu_contains_tc")
          sqlSave(channel=con, tablename = "r_lu_contains_tc", dat=dat_lu_contains, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "landscape_units, r_lu_contains_tc")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check delete_obsolete. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating tables 'landscape_units' and 'r_lu_contains_tc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix
      
    } else {
      if (verbose)
        print("-> All LUs appear in subbasins.")
    }
    
    
    ### TCs not in any LU
    
    # get data
    dat_lu_contains <- sqlFetch(con, "r_lu_contains_tc")
    dat_tc <- sqlFetch(con, "terrain_components")
    
    # identify TCs not in any LU
    r_del <- which(!(dat_tc$pid %in% dat_lu_contains$tc_id))
    
    if(any(r_del)) {
      
      # get tc_contains_svc data
      dat_tc_contains <- sqlFetch(con, "r_tc_contains_svc")
      
      # identify datasets with LUs that will be deleted
      r_del_contains <- which(dat_tc_contains$tc_id %in% dat_tc$pid[r_del])
      
      if(!fix) {
        print("-> The following datasets in 'terrain_components' are obsolete:")
        print(dat_tc[r_del,])
        print("... affecting the following datasets in 'r_tc_contains_svc':")
        print(dat_tc_contains[r_del_contains,])
      } else {
        
        print("-> The following datasets will be removed from 'terrain_components':")
        print(dat_tc[r_del,])
        
        print("... affecting the following datasets in 'r_tc_contains_svc' that will be deleted as well (fractions will be updated):")
        print(dat_tc_contains[r_del_contains,])
        
        dat_tc <- dat_tc[-r_del,]
        dat_tc_contains <- dat_tc_contains[-r_del_contains,]
        
        # update fractions
        frac_sum <- tapply(dat_tc_contains$fraction, list(parent=dat_tc_contains$tc_id), sum)
        for (s in 1:nrow(dat_tc_contains))
          dat_tc_contains$fraction[s] <- dat_tc_contains$fraction[s] / frac_sum[paste0(dat_tc_contains$tc_id[s])]
        
        
        # update database
        tryCatch(
        {
          sqlQuery(con, "delete from terrain_components")
          sqlSave(channel=con, tablename = "terrain_components", dat=dat_tc, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          sqlQuery(con, "delete from r_tc_contains_svc")
          sqlSave(channel=con, tablename = "r_tc_contains_svc", dat=dat_tc_contains, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "terrain_components, r_tc_contains_svc")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check delete_obsolete. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating tables 'terrain_components' and 'r_tc_contains_svc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix

    } else {
      if (verbose)
        print("-> All TCs appear in LUs.")
    }
    
    
    ### SVCs not in any TC and SVCs from special areas
    
    # get data
    dat_tc_contains <- sqlFetch(con, "r_tc_contains_svc")
    dat_svc <- sqlFetch(con, "soil_veg_components")
    
    # identify SVCs not in any TC or special areas
    r_del <- which(!(dat_svc$pid %in% dat_tc_contains$svc_id) | dat_svc$special_area != 0)
    
    if(any(r_del)) {
      
      if(!fix) {
        print("-> The following datasets in 'soil_veg_components' are obsolete:")
        print(dat_svc[r_del,])
      } else {
        
        print("-> The following datasets will be removed from 'soil_veg_components':")
        print(dat_svc[r_del,])
        
        dat_svc <- dat_svc[-r_del,]
        
        # update database
        tryCatch(
        {
          sqlQuery(con, "delete from soil_veg_components")
          sqlSave(channel=con, tablename = "soil_veg_components", dat=dat_svc, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, "soil_veg_components")
        }, error = function(e) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check delete_obsolete. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop(paste0("An error occured when updating table 'soil_veg_components'. ",
                      "Error message of the writing function: ", e))
        }
        )
      
      } # if fix

    } else {
      if (verbose)
        print("-> All SVCs appear in TCs.")
    }
    
    
    
    if(verbose)
      print("OK.")
    
  } # check delete_obsolete




###############################################################################
### check completeness
  if (any(grepl("completeness", check))) {
    if(verbose)
      print("Check completeness ...")
    
    
    ### r_subbas_contains_lu
    
    # get data
    dat_sub_contains <- sqlFetch(con, "r_subbas_contains_lu")
    dat_lu <- sqlFetch(con, "landscape_units")
    dat_sub <- sqlFetch(con, "subbasins")
    
    # check subbasins
    r_miss <- which(!(dat_sub_contains$subbas_id %in% dat_sub$pid))
    
    if(any(r_miss)) {
      print("-> The following subbasins appear in 'r_subbas_contains_lu' but not in 'subbasins':")
      print(paste("Subbasin IDs:", unique(dat_sub_contains$subbas_id[r_miss])))
    }
    
    # check LUs
    r_miss <- which(!(dat_sub_contains$lu_id %in% dat_lu$pid))
    
    if(any(r_miss)) {
      print("-> The following LUs appear in 'r_subbas_contains_lu' but not in 'landscape_units':")
      print(paste("LU IDs:", unique(dat_sub_contains$lu_id[r_miss])))
    }
    
    
    ### r_lu_contains_tc
    
    # get data
    dat_lu_contains <- sqlFetch(con, "r_lu_contains_tc")
    dat_lu <- sqlFetch(con, "landscape_units")
    dat_tc <- sqlFetch(con, "terrain_components")
    
    # check LUs
    r_miss <- which(!(dat_lu_contains$lu_id %in% dat_lu$pid))
    
    if(any(r_miss)) {
      print("-> The following LUs appear in 'r_lu_contains_tc' but not in 'landscape_units':")
      print(paste("LU IDs:", unique(dat_lu_contains$lu_id[r_miss])))
    }
    
    # check TCs
    r_miss <- which(!(dat_lu_contains$tc_id %in% dat_tc$pid))
    
    if(any(r_miss)) {
      print("-> The following TCs appear in 'r_lu_contains_tc' but not in 'terrain_components':")
      print(paste("TC IDs:", unique(dat_lu_contains$tc_id[r_miss])))
    }
    
    
    ### r_tc_contains_svc
    
    # get data
    dat_tc_contains <- sqlFetch(con, "r_tc_contains_svc")
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_tc <- sqlFetch(con, "terrain_components")
    
    # check TCs
    r_miss <- which(!(dat_tc_contains$tc_id %in% dat_tc$pid))
    
    if(any(r_miss)) {
      print("-> The following TCs appear in 'r_tc_contains_svc' but not in 'terrain_components':")
      print(paste("TC IDs:", unique(dat_tc_contains$tc_id[r_miss])))
    }
    
    # check LUs
    r_miss <- which(!(dat_tc_contains$svc_id %in% dat_svc$pid))
    
    if(any(r_miss)) {
      print("-> The following SVCs appear in 'r_tc_contains_svc' but not in 'soil_veg_components':")
      print(paste("SVC IDs:", unique(dat_tc_contains$svc_id[r_miss])))
    }
    
    
    ### soil and vegetation
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_veg <- sqlFetch(con, "vegetation")
    dat_soil <- sqlFetch(con, "soils")
    
    # check soils
    r_miss <- which(!(dat_svc$soil_id %in% dat_soil$pid))
    
    if(any(r_miss)) {
      print("-> The following soils appear in 'soil_veg_components' but not in 'soils':")
      print(paste("Soil IDs:", unique(dat_svc$soil_id[r_miss])))
    }
    
    # check vegetation
    r_miss <- which(!(dat_svc$veg_id %in% dat_veg$pid))
    
    if(any(r_miss)) {
      print("-> The following vegetation types appear in 'soil_veg_components' but not in 'vegetation':")
      print(paste("Vegetation IDs:", unique(dat_svc$veg_id[r_miss])))
    }
    
    
    ### r_soil_contains_particles
    
    # get data
    dat_contains <- sqlFetch(con, "r_soil_contains_particles")
    dat_part <- sqlFetch(con, "particle_classes")
    dat_soil <- sqlFetch(con, "soils")
    
    # check soils
    r_miss <- which(!(dat_contains$soil_id %in% dat_soil$pid))
    
    if(any(r_miss)) {
      print("-> The following soils appear in 'r_soil_contains_particles' but not in 'soils':")
      print(paste("Soil IDs:", unique(dat_contains$soil_id[r_miss])))
    }
    
    # check particle classes
    r_miss <- which(!(dat_contains$class_id %in% dat_part$class_id))
    
    if(any(r_miss)) {
      print("-> The following particle classes appear in 'r_soil_contains_particles' but not in 'particle_classes':")
      print(paste("Particle class IDs:", unique(dat_contains$class_id[r_miss])))
    }
    
    
    ### soil horizons
    
    # get data
    dat_hor <- sqlFetch(con, "horizons")
    dat_soil <- sqlFetch(con, "soils")
    
    # check soils
    r_miss <- which(!(dat_hor$soil_id %in% dat_soil$pid))
    
    if(any(r_miss)) {
      print("-> The following soils appear in 'horizons' but not in 'soils':")
      print(paste("Soil IDs:", unique(dat_hor$soil_id[r_miss])))
    }
    
    
    if(any(r_miss)) {
      # update table meta_info
      if(fix) {
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check completeness. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      odbcClose(con)
      stop("Check for completeness failed. Restore data integrity before any further actions! Notice preceding output messages for more information. Close ODBC connection.")
    }
    
    if(verbose)
      print("OK.")
    
  } # check completeness




###############################################################################
### determine subbasin order
  if (any(grepl("subbasin_order", check))) {
    if(verbose)
      print("Determine subbasin_order (column 'a_stream_order' of table 'subbasins') ...")
    
    if(!fix) {
      print("-> Running in report mode. Nothing to report for this check.")
    } else {
      
      # get data
      dat_sub <- sqlFetch(con, "subbasins")
      
      if(any(!is.na(dat_sub$a_stream_order))) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check subbasin_order. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop("There are already values in column 'a_stream_order' of table 'subbasins'. Set them all to 'NULL' if you want to compute subbasin order!")
      }
      
      ### determine stream order
      
      # identify outlet subbasin
      r_outlet <- which(dat_sub$drains_to %in% c(9999,-9999,999,-999))
      
      if(!any(r_outlet)) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check subbasin_order. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop("Could not identify outlet subbasin from column 'drains_to' in table 'subbasins'. Must be one of values c(9999,-9999,999,-999).")
      }
      if(length(r_outlet) > 1) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check subbasin_order. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop("More than one subbasin has been identified as outlet. Check column 'drains_to' in table 'subbasins'!")
      }
      
      dat_sub$a_stream_order[r_outlet] <- 1
      
      # determine rest of stream order
      fin <- FALSE
      i <- 0
      while(fin == F) {
        i <- i+1
        
        # determine index of previously filled 'a_stream_order'
        r <- which(dat_sub$a_stream_order == i)
        
        # get pid of upstream subbasin
        sub_up <- dat_sub$pid[r]
        
        # find this subbasin in 'drains_to'
        r_up <- which(dat_sub$drains_to %in% sub_up)
        
        # set corresponding 'a_stream_order' value to i+1
        dat_sub$a_stream_order[r_up] <- i+1
        
        # check if finished
        if(!any(is.na(dat_sub$a_stream_order)))
          fin <- TRUE
        
        # throw an error if i is already very large (In this case there must be something wrong)
        if (i > 10000) {
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package LUMP check subbasin_order. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          odbcClose(con)
          stop("Cannot successfully determine subbasin order (column 'a_stream_order' of table 'subbasins'). Check the table for errors!")
        }
      }
      
      
      ### write results to db
      if(verbose)
        print("-> Updating table 'subbasins'...")
      tryCatch(
      {
        sqlQuery(con, "delete from subbasins")
        sqlSave(channel=con, tablename = "subbasins", dat=dat_sub, verbose=F, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
        tbl_changed <- c(tbl_changed, "subbasins")
      }, error = function(e) {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package LUMP check subbasin_order. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        odbcClose(con)
        stop(paste0("An error occured when updating table 'subbasins'. ",
                    "Error message of the writing function: ", e))
      }
      )   
      
    } # if fix
    
    if(verbose)
      print("OK.")
    
  } # determine subbasin order




###############################################################################
### end of function, write changes into 'meta_info', close connection
  
  if(fix) {
    
    # update table meta_info
    meta_dat <- sqlFetch(con, "meta_info")
    if(any(meta_dat$pid)) {
      pid_new <- max(meta_dat$pid) +1
    } else {
      pid_new <- 1
    }
    meta_out <- data.frame(pid=pid_new,
                           mod_date=as.POSIXct(Sys.time()),
                           mod_user=paste0("db_check(), v. ", installed.packages()["LUMP","Version"]),
                           affected_tables=paste(unique(tbl_changed), collapse=", "),
                           affected_columns="various",
                           remarks=paste0("Database checked and adjusted using R package LUMP. Applied checks: ", paste(check, collapse=", "), ". Options: ", paste(names(option), option, sep=" = ", collapse=", ")))
    write_datetabs(con, meta_out, tab="meta_info", verbose)
  
  } # fix


  print("All checks completed successfully. Close ODBC connection.")
  
  odbcClose(con)
  
} # EOF
