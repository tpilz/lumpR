# lumpR/db_check.R
# Copyright (C) 2015-2017 Tobias Pilz, Till Francke
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


#' Check parameter database for consistency
#' 
#' Function includes several options to check and correct the parameter database for completeness
#' and consistency.
#' 
#' @param dbname Name of the data source (DSN) registered at ODBC.
#'  
#' @param check Character vector specifying what shall be checked. See \code{Details}.
#'  
#' @param option A list of options for certain checks. See \code{Details}
#'  for the different options and their meaning.
#'  
#' @param fix \code{logical}. If \code{FALSE} (the \code{default}) a report of the
#'  selected checks will be created. The database will not be touched. If \code{TRUE}
#'  canges to the database according to the selected checks will be made.
#'  
#' @param verbose \code{logical}. Should detailed information during execution be
#'  printed? When \code{TRUE} (the \code{default}) output of writing updated values
#'  into database can be rather long so you might want to direct output into an
#'  external log file. Always set to \code{TRUE} if \code{fix = FALSE}.
#'  
#' @details
#'  The following checks are currently included and can be specified via argument \code{checks}.
#'  Execute checks in pre-defined order as some checks build upon each other and
#'  lead to erroneous results when interchanged. However, some checks might be
#'  unnecessary for your purpose and can be left out.
#'  
#'  \bold{check_fix_fractions}\cr
#'  Check/fix that the fractions of all sub-entities sum to 1.
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
#'  in table 'soil_veg_components' column 'special_area' is equal to 2.\cr
#'  \emph{Option: 'update_frac_impervious'}\cr
#'  Value of type \code{logical}:\cr
#'  F (default): Areal fractions will not be updated. I.e., the sum of 'fraction' for a specific
#'  'tc_id' plus 'frac_rocky' of table 'terrain_components' of that specific TC (calculated by
#'  check 'compute_rocky_frac') sums up to unity. This is a requirement of the WASA-SED model.\cr
#'  T: Areal fractions will be updated such that 'fraction' for a specific 'tc_id' sums to unity.
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
#'  Check for (and delete) obsolete datasets. I.e. special area SVCs, LUs not in any subbasin, TCs
#'  not in any LU, SVCs not in any TC from the \emph{contains}- and the parent tables,
#'  and datasets in 'rainy_season' with obsolete subbasins (if table 'rainy_season' exists).
#'  Dependencies are respected. Areal fractions will not be updated, run with option \code{check_fix_fractions} after removal.
#'  
#'  \bold{completeness}\cr
#'  Check database for completeness. I.e. check if all entities in a higher hierarchy are used and specified in the related tables 
#'  of lower hierarchy. Reports only, ignores \code{fix=T} and does no changes t the database.
#'  
#'  \bold{subbasin_order}\cr
#'  Compute subbasin order for WASA's routing input file \code{routing.dat}. Order will
#'  be derived from column 'drains_to' and written to 'a_stream_order' of table 'subbasins'.
#'  \emph{Option: 'overwrite'}: Overwrite existing vaues.\cr
#'  
#'  
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}

db_check <- function(
  dbname,
  check = c("check_fix_fractions", "filter_small_areas", "tc_slope", "special_areas", "remove_water_svc",
            "compute_rocky_frac", "remove_impervious_svc", "proxy_frgw_delay",
            "delete_obsolete", "completeness", "subbasin_order"),
  option = list(area_thresh=0.01,
                treat_slope=c(3,0.01,0.1),
                update_frac_impervious=F, overwrite=F),
  fix=F,
  verbose=TRUE
) {
  
  # if fix = F (default) set verbose = T
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(verbose) message("% START db_check()")
  if(verbose) {
    if(fix) {
      message("%")
      message("% -> fix = TRUE and database will touched and fixed if necessary")
    } else {
      message("%")
      message("% -> Running report mode, i.e. fix = FALSE and database will not be touched")
    }
  }

  
  
  if(verbose) message("%")
  if(verbose) message("% Loading package 'RODBC' and connecting to database ...")
  
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
  
  # initialise vector to keep track of changed tables
  tbl_changed <- NULL
  
  
###############################################################################
### check current db version
  if(verbose) message("%")
  if(verbose) message("% Check database version ...")

  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="lumpR")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))

  db_ver <- max(sqlFetch(con, "db_version")$version)
  if(db_ver < db_ver_max) stop(paste0("Database version is prior to version ", db_ver_max, ". Make sure you use the latest database version (consider function db_update())!"))

  if(verbose) message("% OK")

###############################################################################
### check fractions (use external function for every relevant table)
  if (any(grepl("check_fix_fractions", check))) {
    if(verbose) message("%")
    if(verbose) message("% Check fractions ...")
    
    # LUs
    tbl_ch <- check_fix_fractions(con=con, table="r_subbas_contains_lu", fix=fix, verbose=verbose, tbl_changed=tbl_changed)
    tbl_changed <- c(tbl_changed, tbl_ch)
    
    # TCs
    tbl_ch <- check_fix_fractions(con=con, table="r_lu_contains_tc", fix=fix, verbose=verbose, tbl_changed=tbl_changed)
    tbl_changed <- c(tbl_changed, tbl_ch)
    
    # SVCs
    tbl_ch <- check_fix_fractions(con=con, table="r_tc_contains_svc", fix=fix, verbose=verbose, tbl_changed=tbl_changed)
    tbl_changed <- c(tbl_changed, tbl_ch)
    
    if(verbose) message("% OK")
  } # check fractions
  
    
###############################################################################
### filter small areas (use external function for every relevant table)
  if (any(grepl("filter_small_areas", check))) {
    if(verbose) message("%")
    if(verbose) message("% Filter small areas ...")
    
    if(!("area_thresh" %in% names(option))) stop("No option 'area_thresh' specified for check 'filter_small_areas'.")
    
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
    
    if(verbose) message("% OK")
  } # filter small areas


###############################################################################
### TC with slope <= 0
  if (any(grepl("tc_slope", check))) {
    if(verbose) message("%")
    if(verbose) message("% Find TCs with slope <= 0 ...")
    
    # get data
    dat_tc <- sqlFetch(con, "terrain_components")
    if(!any(which(dat_tc$slope <= 0))) {
      message("% -> There are no TCs with slope <= 0.")
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
                                 mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package lumpR check tc_slope. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
        }
        stop("No option 'treat_slope' specified for check 'tc_slope'.")
      }
      
      # data from r_lu_contains_tc
      dat_lutc <- sqlFetch(con, "r_lu_contains_tc")
      
      # datasets of r_lu_contains_tc where slope is <= 0
      r_tc_zero <- which(dat_tc$slope <= 0)
      tc_zero <- dat_tc$pid[r_tc_zero]
      lutc_zero <- dat_lutc[which(dat_lutc$tc_id %in% tc_zero),]
      dat_out_lutc <- NULL # will only be touched if dat_lutc needs to be fixed
      dat_out_tc <- NULL # will only be touched if dat_tc needs to be fixed
      
    
      # option 1: remove TC if areal fraction within LU is below threshold
      if (option[["treat_slope"]][1] == 1 | option[["treat_slope"]][1] == 3) {
        
        if (verbose)
          message("% -> Option 1: Remove TCs with small fraction within LU...")
        
        # identify datasets below fraction threshold
        rows_lutc_rm <- which(lutc_zero$fraction < option[["treat_slope"]][2])
        
        if (!any(rows_lutc_rm)) {
          message("% -> No TCs with fraction below defined threshold. No TCs will be removed.")
        } else {
          
          if(fix) {
            message(paste0("% -> There are ", length(rows_lutc_rm), " datasets going to be removed from 'r_lu_contains_tc' ('fraction' will be updated)"))
          } else {
            message(paste0("% -> There are ", length(rows_lutc_rm), " datasets in 'r_lu_contains_tc' containing slopes <= 0 and fractions below threshold"))
          }
          
          # keep datasets where TCs equal to more than 10% of the respective parent class' area would be removed
          rm_sum <- tapply(lutc_zero$fraction[rows_lutc_rm], list(parent=lutc_zero[[1]][rows_lutc_rm]), sum)
          if(any(rm_sum > 0.1)) {
            keep <- which(rm_sum > 0.1)
            message(paste0("% -> For '", colnames(lutc_zero)[1], "' ", paste(names(rm_sum)[keep], collapse=", "),
                         " more than 10% of the area would be removed due to too many small '", colnames(lutc_zero)[2], ". These datasets will be kept."))
            
            rows_rm_keep <- which(lutc_zero[[1]][rows_lutc_rm] %in% names(rm_sum)[keep])
            rows_lutc_rm <- rows_lutc_rm[-rows_rm_keep]
          }
          
          # check if some datasets are left to remove
          if (!any(rows_lutc_rm)) {
            message("% -> Nothing left to remove or choose smaller value for threshold and re-run check.")
          } else {
            
            # remove datasets with small fraction
            lutc_zero <- lutc_zero[-rows_lutc_rm,]
            
            # merge unchanged datasets with adjusted datasets where slope of TC <= 0
            dat_out_lutc <- dat_lutc[-which(dat_lutc$tc_id %in% tc_zero),]
            dat_out_lutc <- rbind(dat_out_lutc, lutc_zero)
            
            # re-calculate fractions
            frac_sum <- tapply(dat_out_lutc$fraction, list(parent=dat_out_lutc[[1]]), sum)
            for (s in 1:nrow(dat_out_lutc))
              dat_out_lutc$fraction[s] <- dat_out_lutc$fraction[s] / frac_sum[paste0(dat_out_lutc[[1]][s])]
            
          } # still datasets left to remove?
        } # TCs wih fraction below threshold?
  
      } # end of option 1
      
      
      
      # option 2: change slope to specified small positive value
      if (option[["treat_slope"]][1] == 2 | option[["treat_slope"]][1] == 3) {
        
        if (verbose)
          message("% -> Option 2: Change slope to specified small positive value...")
        
        # determine replace value
        if (option[["treat_slope"]][1] == 2)
          repl_slope <- option[["treat_slope"]][2]
        if (option[["treat_slope"]][1] == 3)
          repl_slope <- option[["treat_slope"]][3]
        
        if(fix) {
          message(paste0("% -> There are ", length(r_tc_zero), " datasets where slopes in 'terrain_components' will be replaced by ", repl_slope))
          dat_out_tc <- dat_tc
          dat_out_tc[r_tc_zero,"slope"] <- repl_slope
        } else {
          message(paste0("% -> There are ", length(r_tc_zero), " datasets in 'terrain_components' containing slopes <= 0"))
        }
          
      } # end of option 2
      
      
      
      # update database
      if(fix) {
        
        if(!is.null(dat_out_lutc)) {
          if(verbose)
            message("% -> Updating table 'r_lu_contains_tc'...")
          tryCatch(
          {
            sqlQuery(con, "delete from r_lu_contains_tc")
            sqlSave(channel=con, tablename = "r_lu_contains_tc", dat=dat_out_lutc, verbose=F, 
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
                                   mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                   affected_tables=paste(unique(tbl_changed), collapse=", "),
                                   affected_columns="various",
                                   remarks=paste0("ATTENTION: Error while checking database using R package lumpR check tc_slope. Nevertheless, affected_tables have already been changed."))
            write_datetabs(con, meta_out, tab="meta_info", verbose)
            stop(paste0("An error occured when updating table 'r_lu_contains_tc'. ",
                        "Error message of the writing function: ", e))
          }
          )
        } # dat_out_lutc exists?
        
        if(!is.null(dat_out_tc)) {
          if(verbose)
            message("% -> Updating table 'terrain_components'...")
          tryCatch(
          {
            sqlQuery(con, "delete from terrain_components")
            sqlSave(channel=con, tablename = "terrain_components", dat=dat_out_tc, verbose=F, 
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
                                   mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                   affected_tables=paste(unique(tbl_changed), collapse=", "),
                                   affected_columns="various",
                                   remarks=paste0("ATTENTION: Error while checking database using R package lumpR check tc_slope. Nevertheless, affected_tables have already been changed."))
            write_datetabs(con, meta_out, tab="meta_info", verbose)
            stop(paste0("An error occured when updating table 'terrain_components'. ",
                        "Error message of the writing function: ", e))
          }
          )
        } # dat_out_tc exists?
        
      } # if fix
    
    } # TC with slope <= 0?
    
    if(verbose) message("% OK")
    
  } # check tc_slope?


###############################################################################
### Assign special areas (water, impervious surfaces)
  if (any(grepl("special_areas", check))) {
    if(verbose) message("%")
    if(verbose) message("% Define certain SVCs as special areas ...")
    
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
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
          message(paste0("% -> ATTENTION: Option 'special_area': Vegetation id ", option$special_area$ref_id[r], " could not be found in column 'veg_id' of table 'soil_veg_components'."))
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
          message(paste0("% -> ATTENTION: Option 'special_area': Soil id ", option$special_area$ref_id[r], " could not be found in column 'soil_id' of table 'soil_veg_components'."))
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
        message("% -> Updating table 'soil_veg_components'...")
      tryCatch(
      {
        sqlQuery(con, "delete from soil_veg_components")
        sqlSave(channel=con, tablename = "soil_veg_components", dat=dat_svc, verbose=F, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
        tbl_changed <- c(tbl_changed, "soil_veg_components")
        if(verbose) message("% -> Table 'soil_veg_components' updated")
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check special_areas. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        stop(paste0("An error occured when updating table 'soil_veg_components'. ",
                    "Error message of the writing function: ", e))
      })
    }  
    else
      if(verbose)
        message("% -> Nothing done.")
    
    if(verbose) message("% OK")
    
  } # check special_areas





###############################################################################
### remove water SVCs
  if (any(grepl("remove_water_svc", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Remove SVCs marked as water ...")
    if(verbose & !fix) message("% Identify SVCs marked as water ...")
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    # identify water SVCs
    rows_water <- which(dat_svc$special_area == 1)
    svc_water <- dat_svc$pid[rows_water]
    rows_contains_water <- which(dat_contains$svc_id %in% svc_water)
    
    if (length(rows_contains_water)==0) {
      message("% -> No water-SVCs found, nothing done.")          
    } else {
    
      if(!fix)
        message(paste0("% -> There are ", length(rows_contains_water), " datasets in 'r_tc_contains_svc' identified as water areas"))
      else {
        message(paste0("% -> There are ", length(rows_contains_water), " datasets going to be removed from 'r_tc_contains_svc' ('fraction' will be updated)"))
      
        # remove water SVCs
        dat_contains_act <- dat_contains[-rows_contains_water,]
        
        # update fractions
        frac_sum <- tapply(dat_contains_act$fraction, list(parent=dat_contains_act$tc_id), sum)
        for (s in 1:nrow(dat_contains_act))
          dat_contains_act$fraction[s] <- dat_contains_act$fraction[s] / frac_sum[paste0(dat_contains_act$tc_id[s])]
        
        # update database
        if(verbose)
          message("% -> Updating table 'r_tc_contains_svc'...")
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
                                 mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package lumpR check remove_water_svc. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          stop(paste0("An error occured when updating table 'r_tc_contains_svc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix
      
    } # if water SVCs found
    
    
    if(verbose) message("% OK")
    
  } # check remove_water_svc




###############################################################################
### compute rocky fractions for TCs
  if (any(grepl("compute_rocky_frac", check))) {
    if(verbose) message("%")
    if(verbose) message("% Compute rocky (i.e. impervious) fractions for TCs ...")
    
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_tc <- sqlFetch(con, "terrain_components")
    dat_hor <- sqlFetch(con, "horizons")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    dat_tc$frac_rocky[is.na(dat_tc$frac_rocky)] = 0 #set NAs to 0
    if (nrow(dat_tc)>0 & max(dat_tc$frac_rocky)>0)
      message("% -> ATTENTION: Column 'frac_rocky' in table 'terrain_components' already contains some values. The computed fractions will be added to these.")

    # identify soils with 100% coarse fragments in topmost horizon
    soil_impervious <- dat_hor$soil_id[which(dat_hor$position == 1 & dat_hor$coarse_frag == 1)]
    
    # mark corresponding SVCs as impervious
    if(any(soil_impervious)) {
      rows_svc_impervious <- which(dat_svc$soil_id %in% soil_impervious)
      
      if(fix) {
        message(paste0("% -> There are ", length(rows_svc_impervious), " datasets in 'soil_veg_components' which will be marked as impervious due to 100% coarse fragments in topmost soil horizon"))
      } else {
        message(paste0("% -> There are ", length(rows_svc_impervious), " datasets in 'soil_veg_components' identified as impervious due to 100% coarse fragments in topmost soil horizon"))
      }
      
      dat_svc$special_area[rows_svc_impervious] <- 2
    } else {
      if (verbose)
        message("% -> No topmost horizon with 100% coarse fragments could be found.")
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
        message("% -> Updating table 'terrain_components'...")
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check compute_rocky_frac. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        stop(paste0("An error occured when updating table 'terrain_components'. ",
                    "Error message of the writing function: ", e))
      }
      )
    
    } # if fix
    
    if(verbose) message("% OK")
    
  } # check compute_rocky_frac
  





###############################################################################
### remove impervious SVCs
  if (any(grepl("remove_impervious_svc", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Remove SVCs marked as impervious ...")
    if(verbose & !fix) message("% Identify SVCs marked as impervious ...")
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    # identify impervious SVCs
    rows_impervious <- which(dat_svc$special_area == 2)
    svc_impervious <- dat_svc$pid[rows_impervious]
    rows_contains_impervious <- which(dat_contains$svc_id %in% svc_impervious)
    
    if(!any(rows_contains_impervious)) {
      message("% -> No impervious SVCs could be found. Nothing done.")
    } else {
    
      if(!fix) {
        message(paste0("% -> There are ", length(rows_contains_impervious), " datasets in 'r_tc_contains_svc' identified as impervious"))
      } else {
        message(paste0("% -> There are ", length(rows_contains_impervious), " datasets which will be removed from 'r_tc_contains_svc' ('fraction' will be updated)"))
        
        # update database
        if(verbose)
          message("% -> Updating table 'r_tc_contains_svc'...")
        tryCatch(
        {
          if(!option$update_frac_impervious) {
            sqlQuery(con, paste("delete from r_tc_contains_svc ",
                                "WHERE tc_id IN (", paste(unique(dat_contains$tc_id[rows_contains_impervious]), collapse = ", "), ")",
                                "AND svc_id IN (", paste(unique(dat_contains$svc_id[rows_contains_impervious]), collapse = ", "), ")"))
            tbl_changed <- c(tbl_changed, "r_tc_contains_svc")
          } else {
            # remove impervious SVCs
            dat_contains_act <- dat_contains[-rows_contains_impervious,]
            # update fractions
            frac_sum <- tapply(dat_contains_act$fraction, list(parent=dat_contains_act$tc_id), sum)
            for (s in 1:nrow(dat_contains_act))
              dat_contains_act$fraction[s] <- dat_contains_act$fraction[s] / frac_sum[paste0(dat_contains_act$tc_id[s])]
            # adjust database
            sqlQuery(con, "delete from r_tc_contains_svc")
            sqlSave(channel=con, tablename = "r_tc_contains_svc", dat=dat_contains_act, verbose=F, 
                    append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
            tbl_changed <- c(tbl_changed, "r_tc_contains_svc")
          }
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
                                 mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package lumpR check remove_impervious_svc. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          stop(paste0("An error occured when updating table 'r_tc_contains_svc'. ",
                      "Error message of the writing function: ", e))
        }
        )
        
      } # if fix
      
    } # if any impervious SVC
  
  
    if(verbose) message("% OK")
  
  } # check remove_impervious_svc
  




###############################################################################
### estimate frgw_delay
  if (any(grepl("proxy_frgw_delay", check))) {
    if(verbose) message("%")
    if(verbose) message("% Estimate frgw_delay for LUs ...")
    
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check proxy_frgw_delay. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }
      stop("No option 'total_mean_delay' specified for check 'proxy_frgw_delay'.")
    }
    
    if(!fix) {
      message("% -> Running in report mode. Nothing to report for this check.")
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
        message("% -> Updating table 'landscape_units'...")
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
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check proxy_frgw_delay. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        stop(paste0("An error occured when updating table 'landscape_units'. ",
                    "Error message of the writing function: ", e))
      }
      )
      
    } # if fix
    
    if(verbose) message("% OK")
    
  } # check proxy_frgw_delay
  



###############################################################################
### delete obsolete datasets or check for completeness
### define chains of table dependencies
  if (any(grepl("delete_obsolete", check)) | any(grepl("completeness", check))) {
    table_chain1= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (1)
      rbind(
        c (table = "subbasins",    key2prev="", key2next="pid"),
        c (table = "r_subbas_contains_lu", key2prev="subbas_id", key2next="lu_id"),
        c (table = "landscape_units",      key2prev="pid"      , key2next="pid"),
        c (table = "r_lu_contains_tc",     key2prev="lu_id"    , key2next="tc_id"),
        c (table = "terrain_components",   key2prev="pid"      , key2next="pid"),
        c (table = "r_tc_contains_svc",    key2prev="tc_id"    , key2next="svc_id"),
        c (table = "soil_veg_components",  key2prev="pid"      , key2next="soil_id"),
        c (table = "soils",                key2prev="pid"      , key2next="pid"),
        c (table = "horizons",             key2prev="soil_id", key2next="")
      ))
    
    table_chain2= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (2)
      rbind(
        c (table = "soil_veg_components",  key2prev="pid"      , key2next="veg_id"),
        c (table = "vegetation",           key2prev="pid"      , key2next="")
      ))
    
    table_chain3= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (3)
      rbind(
        c (table = "soils",                     key2prev="pid"      , key2next="pid"),
        c (table = "r_soil_contains_particles", key2prev="soil_id"  , key2next="class_id"),
        c (table = "particle_classes"        ,  key2prev="class_id" , key2next="")
      ))
    
    table_chain4= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (4)
      rbind(
        c (table = "subbasins",    key2prev="", key2next="pid"),
        c (table = "x_seasons",    key2prev="subbas_id", key2next="")
      ))
    
    table_chain5= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (5)
      rbind(
        c (table = "soil_veg_components",  key2prev="pid"      , key2next="veg_id"),
        c (table = "x_seasons",    key2prev="svc_id", key2next="")
      ))
    
    table_chain6= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (6)
      rbind(
        c (table = "subbasins",    key2prev="", key2next="pid"),
        c (table = "rainy_season",    key2prev="subbas_id", key2next="")
      ))
    
    table_chain7= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (7)
      rbind(
        c (table = "vegetation",           key2prev="pid"      , key2next="pid"),
        c (table = "rainy_season",    key2prev="veg_id", key2next="")
      ))
    
    wildcard_fields = list(  #tables with wildcard values ("-1") need to be considered
      rainy_season = c("subbas_id", "veg_id"),
      x_seasons     = c("subbas_id", "svc_id")
    )
    chain_list = list(table_chain1, table_chain2, table_chain3, table_chain4, table_chain5, table_chain6, table_chain7) #list of all relation chains that need to be checked
}
    
###############################################################################
### delete obsolete datasets
    
if (any(grepl("delete_obsolete", check))) {
  if(verbose) message("%")
  if(verbose & fix) message("% Delete obsolete records ...")
  if(verbose & !fix) message("% Identify obsolete records ...")
    
    break_msg=""

    for (table_chain in chain_list)
    {  
      for (i in 2:nrow(table_chain))
      {
        cur_table = table_chain$table[i]
        pre_table = table_chain$table[i-1]
        statement = paste0("select ",
                           cur_table,".",table_chain$key2prev[i],
                           " from ", cur_table, " left join ", pre_table, " on ",
                     cur_table,".",table_chain$key2prev[i], "=", 
                     pre_table,".",table_chain$key2next[i-1],
                     " where ", pre_table,".",table_chain$key2next[i-1]," IS NULL")
        if (cur_table %in% names(wildcard_fields))
          statement = paste0(statement, " AND NOT ", cur_table,".",table_chain$key2prev[i], "=-1;") else
          statement = paste0(statement, ";")
              
        statement <- sql_dialect(con, statement) # adjust to specific SQL dialects
        
        res <- sqlQuery(con, statement, errors=FALSE)
        if (is.numeric(res) && res==-1)
        {
          res <- sqlQuery(con, statement, errors = T)
          break_msg = paste0("Error in SQL query execution while detecting obsolete records: \nQuery: ", statement,
                          "\nerror-message: ", res[1])
          break
        }
        if (nrow(res)==0) 
        {
          if(verbose) message(paste0("% -> No obsolete records found in '", cur_table,"'"))
          next   #no obsolete records found
        }
        if(verbose) 
        {  
          message(paste0("% -> There are ", nrow(res), " obsolete datasets in '", cur_table,"'"))
        }    
        if (!fix) 
          if(verbose) message("% -> More records may turn up after actual cleaning (fix=TRUE).")
        else
        {
          statement = paste0("delete from ", cur_table, 
                             " where ", table_chain$key2prev[i]," IN (",
                             paste0(res[,1], collapse=","),")")
          
          statement <- sql_dialect(con, statement) # adjust to specific SQL dialects  
          res <- sqlQuery(con, statement, errors=FALSE)
          if (is.numeric(res) && res==-1)
          {  
            res <- sqlQuery(con, statement, errors = T)
            break_msg = cat(paste0("Error in SQL query execution while detecting obsolete records. \nQuery: ", statement,
                            "\nerror-message: ", res[1]))
            break
          } 
          tbl_changed <- c(tbl_changed, cur_table)
        }          
      }
     
      if (break_msg!="") #anything went wrong?
      {
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check delete_obsolete. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
        stop(break_msg)
      }
    }  
    if(verbose) message("% OK")

  } # check delete_obsolete




###############################################################################
### check completeness
  if (any(grepl("completeness", check))){
    if(verbose) message("%")
    if(verbose) message("% Search referenced records without specification...")
    
    break_msg=""

    for (table_chain in chain_list)
    {  
      for (i in 1:(nrow(table_chain)-1))
      {
        cur_table = table_chain$table[i]
        nex_table = table_chain$table[i+1]
        statement = paste0("select ",
                           cur_table,".",table_chain$key2next[i],
                           " from ", cur_table, " left join ", nex_table, " on ",
                           cur_table,".",table_chain$key2next[i], "=", 
                           nex_table,".",table_chain$key2prev[i+1])
        if (nex_table %in% names(wildcard_fields))
          statement = paste0(statement, " OR ", nex_table,".",table_chain$key2prev[i+1], "=-1") 

          statement = paste0(statement, " where ", nex_table,".",table_chain$key2prev[i+1]," IS NULL")
        
        statement <- sql_dialect(con, statement) # adjust to specific SQL dialects
        
        res <- sqlQuery(con, statement, errors=FALSE)
        if (is.numeric(res) && res==-1)
        {
          res <- sqlQuery(con, statement, errors = T)
          break_msg = paste0("Error in SQL query execution while detecting incomplete records: \nQuery: ", statement,
                             "\nerror-message: ", res[1])
          break
        }
        if(verbose) {
          if (nrow(res)==0) 
            message(paste0("% -> No incomplete records found in '", cur_table,"'"))
          else 
            message(paste0("% -> There are ", nrow(res), " records in '", cur_table,"' missing their description in '", nex_table, "'"))
        }
      }
    } 
  
  #check for multiple use of TC in LUs (currently not allowed by structure of WASA input files)
    statement = paste0("select tc_id, count(*) as ct from r_lu_contains_tc group by tc_id having count(*) >1")
    statement <- sql_dialect(con, statement) # adjust to specific SQL dialects
    
    res <- sqlQuery(con, statement, errors=FALSE)
    if (is.numeric(res) && res==-1)
    {
      res <- sqlQuery(con, statement, errors = T)
      break_msg = paste0("Error in SQL query execution while detecting multiple use of TCs: \nQuery: ", statement,
                         "\nerror-message: ", res[1])
      break
    }
    if (nrow(res)>0)
      stop(paste0("TC(s) ", paste0(res$tc_id, collapse=" ,"), " is/are part of multiple LUs, which is currently not supported. Duplicate these TCs and assign a different ID for each instance."))
    
    if(verbose) message("% OK")
  }   # check completeness



###############################################################################
### determine subbasin order
  if (any(grepl("subbasin_order", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Determine subbasin order (write into column 'a_stream_order' of table 'subbasins') ...")
    if(verbose & !fix) message("% Determine subbasin order (report mode, no changes to database) ...")
  
    # get data
    dat_sub <- sqlFetch(con, "subbasins")  
    stream_order_old = dat_sub$a_stream_order #keep for comparison
    # identify outlet subbasin
    r_outlet <- which(dat_sub$drains_to %in% c(9999,-9999,999,-999))
    
    
    if((length(r_outlet) > 1) | (!any(r_outlet))) {
      if (any(tbl_changed))
      {  
        # update table meta_info
        meta_dat <- sqlFetch(con, "meta_info")
        if(any(meta_dat$pid)) {
          pid_new <- max(meta_dat$pid) +1
        } else {
          pid_new <- 1
        }
        meta_out <- data.frame(pid=pid_new,
                               mod_date=as.POSIXct(Sys.time()),
                               mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                               affected_tables=paste(unique(tbl_changed), collapse=", "),
                               affected_columns="various",
                               remarks=paste0("ATTENTION: Error while checking database using R package lumpR check subbasin_order. Nevertheless, affected_tables have already been changed."))
        write_datetabs(con, meta_out, tab="meta_info", verbose)
      }  
      if (!any(r_outlet))
          stop("Could not identify outlet subbasin from column 'drains_to' in table 'subbasins'. Must be one of values c(9999,-9999,999,-999).")
      else
          stop("More than one subbasin has been identified as outlet. Check column 'drains_to' in table 'subbasins'!")
    }
    
    ##determine stream order
    dat_sub$a_stream_order = NA
    dat_sub$a_stream_order[r_outlet] <- 1
    
    # determine rest of stream order
    fin <- FALSE
    i <- 0
    while(fin == F) {
      i <- i+1
      
      # determine indices of subbasins of previously filled 'a_stream_order' (downstream subbasins)
      r <- which(dat_sub$a_stream_order == i)
      
      # get pid(s) of downstream subbasin(s)
      sub_up <- dat_sub$pid[r]
      
      # find this upstream subbasins to current downstream subbasins by checking 'drains_to'
      r_up <- which(dat_sub$drains_to %in% sub_up)
      
      # set corresponding 'a_stream_order' value to i+1
      dat_sub$a_stream_order[r_up] <- i+1
      
      # check if finished
      if(!any(is.na(dat_sub$a_stream_order)))
        fin <- TRUE
      
      # throw an error if i is already very large (In this case there must be something wrong)
      if (i > 10000) {
        if (any(tbl_changed))
        {  
          # update table meta_info
          meta_dat <- sqlFetch(con, "meta_info")
          if(any(meta_dat$pid)) {
            pid_new <- max(meta_dat$pid) +1
          } else {
            pid_new <- 1
          }
          meta_out <- data.frame(pid=pid_new,
                                 mod_date=as.POSIXct(Sys.time()),
                                 mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                 affected_tables=paste(unique(tbl_changed), collapse=", "),
                                 affected_columns="various",
                                 remarks=paste0("ATTENTION: Error while checking database using R package lumpR check subbasin_order. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
        }  
        stop("Cannot successfully determine subbasin order (column 'a_stream_order' of table 'subbasins'). Check the table for errors!")
      }
    }
    
    if (all(!is.na(stream_order_old)) &
        all(stream_order_old== dat_sub$a_stream_order)) 
      message("% -> existing stream order ok.") else
      {
        if(!fix) {
          message("% -> existing stream order needs updating. Consider running with 'fix=TRUE' and 'option=list(overwrite=TRUE)'") 
        } else {
          
          if(is.null(option$overwrite) | !option$overwrite) {
            if (any(tbl_changed))
            { 
              # update table meta_info
              meta_dat <- sqlFetch(con, "meta_info")
              if(any(meta_dat$pid)) {
                pid_new <- max(meta_dat$pid) +1
              } else {
                pid_new <- 1
              }
              meta_out <- data.frame(pid=pid_new,
                                     mod_date=as.POSIXct(Sys.time()),
                                     mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                     affected_tables=paste(unique(tbl_changed), collapse=", "),
                                     affected_columns="various",
                                     remarks=paste0("ATTENTION: Error while checking database using R package lumpR check subbasin_order. Nevertheless, affected_tables have already been changed."))
              write_datetabs(con, meta_out, tab="meta_info", verbose)
            }  
            stop("There are already values in column 'a_stream_order' of table 'subbasins'. Use option=list(overwrite=TRUE) or manually set them all to 'NULL' if you want to compute subbasin order!")
          }

          ### write results to db
          if(verbose)
            message("% -> Updating table 'subbasins'...")
          tryCatch(
            {
              for (i in 1:nrow(dat_sub))
              {  
                statement = paste0("update subbasins set a_stream_order=",dat_sub$a_stream_order[i],
                                   " where pid=",dat_sub$pid[i],";")
              
                res <- sqlQuery(con, statement, errors=FALSE)
                if (is.numeric(res) && res==-1)
                {
                  res <- sqlQuery(con, statement, errors = T)
                  break_msg = paste0("Error in SQL query execution while updating stream order: \nQuery: ", statement,
                                     "\nerror-message: ", res[1])
                  stop(break_msg)
                }
                if (length(tbl_changed)==0 || tbl_changed[length(tbl_changed)]!="subbasins")
                  tbl_changed <- c(tbl_changed, "subbasins")
              }  

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
                                     mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                                     affected_tables=paste(unique(tbl_changed), collapse=", "),
                                     affected_columns="various",
                                     remarks=paste0("ATTENTION: Error while checking database using R package lumpR check subbasin_order. Nevertheless, affected_tables have already been changed."))
              write_datetabs(con, meta_out, tab="meta_info", verbose)
              stop(paste0("An error occured when updating table 'subbasins'. ",
                          "Error message of the writing function: ", e))
            }
          )   

        } # if fix
      }   
    if(verbose) message("% OK")
    
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
                           mod_user=paste0("db_check(), v. ", installed.packages()["lumpR","Version"]),
                           affected_tables=paste(unique(tbl_changed), collapse=", "),
                           affected_columns="various",
                           remarks=paste0("Database checked and adjusted using R package lumpR. Applied checks: ", paste(check, collapse=", "), ". Options: ", paste(names(option), option, sep=" = ", collapse=", ")))
    write_datetabs(con, meta_out, tab="meta_info", verbose)
  
  } # fix


  if(verbose) message("%")
  if(verbose) message("% All checks completed successfully. Close ODBC connection.")
  
  odbcClose(con)
  
  if(verbose) message("%")
  if(verbose) message("% DONE!")
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
} # EOF
