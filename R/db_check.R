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


#' Check and modify parameter database for consistency
#' 
#' Function includes several options to check and correct the parameter database for completeness
#' and consistency.
#' 
#' @param dbname Name of the data source (DSN) registered as ODBC-source.
#'  
#' @param check Character vector specifying what shall be checked. See \code{Details}.
#'  
#' @param option A list of options for certain checks. See \code{Details}
#'  for the different options and their meaning.
#'  
#' @param fix \code{logical}. If \code{FALSE} (the \code{default}) a report of the
#'  selected checks will be created. The database will not be modified. If \code{TRUE},
#'  changes to the database according to the selected checks will be made.
#'  WARNING: \code{fix = TRUE} may irrevocably modify your database. If in doubt, make a backup before.
#'  
#' @param verbose \code{logical}. Should detailed information during execution be
#'  printed? When \code{TRUE} (the \code{default}), output of writing updated values
#'  into database can be rather long so you might want to direct output into an
#'  external log file. Always set to \code{TRUE} if \code{fix = FALSE}.
#'  
#' @details
#'  The following checks / modifications are currently included and can be specified via argument \code{checks}.
#'  Execute checks in pre-defined order as some steps build upon each other and
#'  lead to erroneous results when interchanged. However, some steps might be
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
#'  (This check is unnecessary if you specified \code{watermask}) and \code{imperviousmask} in the workflow before)
#'  Define certain Soil-Vegetation Components as special areas via column 'special_area'
#'  in table 'soil_veg_components' inferred from table 'vegetation' and/or 'soils'. This columns is 
#'  necessary for \bold{remove_water_svc} and \bold{compute_rocky_frac} to work.
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
#'  FALSE (default): Areal fractions will not be updated. I.e., the sum of 'fraction' for a specific
#'  'tc_id' plus 'frac_rocky' of table 'terrain_components' of that specific TC (calculated by
#'  check 'compute_rocky_frac') sums up to unity. This is a requirement of the WASA-SED model.\cr
#'  TRUE: Areal fractions will be updated such that 'fraction' for a specific 'tc_id' sums to unity.
#'  
#'  \bold{proxy_frgw_delay}\cr
#'  Estimate storage coefficient for groundwater delay ('frgw_delay') in \emph{days}
#'  for each LU based on a proxy and manually specified total mean groundwater delay
#'  in \emph{days}. The proxy is estimated from average slope length, slope and rocky
#'  fraction according to empirical formulas:\cr
#'   \code{proxy = slopelength * (1 - frac_rocky) / slope}. and \cr
#'   \code{frgw_delay = proxy * total_mean_delay / mean(proxy)}.
#'  Existing values of 'frgw_delay' will be overwritten.\cr
#'  \emph{Option: 'total_mean_delay'}\cr
#'  Total mean groundwater delay in \emph{days} estimated a priori for the whole
#'  catchment (e.g. from baseflow analysis). All proxy values are scaled, so their 
#'  mean matches this value (see formula above).
#'  
#'  \bold{delete_obsolete}\cr
#'  Check for (and optionally delete) obsolete datasets. I.e. LUs not in any subbasin, TCs
#'  not in any LU, SVCs not in any TC from the \emph{contains}- and the parent tables,
#'  and datasets in 'rainy_season' with obsolete subbasins (if table 'rainy_season' exists).
#'  Dependencies are respected. Areal fractions will be updated in case obsolete datasets are removed.\cr
#'  \emph{Option: 'tbls_preserve'}\cr
#'  Vector of type \code{character} giving the names of tables where obsolete datasets will NOT be removed.
#'  Default: \code{NULL}.
#'  
#'  \bold{completeness}\cr
#'  Check database for completeness. I.e. check if all entities in a higher hierarchy are used and specified in the related tables 
#'  of lower hierarchy. Reports only, ignores \code{fix=T} and does no changes to the database.
#'  
#'  \bold{subbasin_order}\cr
#'  Compute subbasin order for WASA's routing input file \code{routing.dat}. Order will
#'  be derived from column 'drains_to' and written to 'a_stream_order' of table 'subbasins'.\cr
#'  \emph{Option: 'overwrite'}: Overwrite existing vaues.
#'  
#' @note
#'  In case the default value of option \emph{'update_frac_impervious'} shall NOT be used,
#'  you should always explicitly specify it, even when check 'remove_impervious_svc' is not applied,
#'  to make sure, 'fraction' of table 'r_tc_contains_svc' is always correctly calculated.
#'  
#'  Database will only be touched (if \code{fix = TRUE}) after all checks are completed successfully.
#'  If the function stops with an error during checks, the database will be left unchanged.
#'
#' @examples  
#' \dontrun{
#' db_check(dbname,
#' check=c("subbasin_order"),
#' fix=TRUE,
#' verbose=T, option=list(overwrite=FALSE))
#' }
#'  
#' @references 
#'      lumpR package introduction with literature study and sensitivity analysis:\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
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
                update_frac_impervious=F, overwrite=F, tbls_preserve=NULL),
  fix=F,
  verbose=TRUE
) {
  
  # if fix = F (default) set verbose = T
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(verbose) message("% START db_check()")
  if(verbose) {
    if(fix) {
      message("%")
      message("% -> fix = TRUE and database will be modified and fixed, if necessary")
    } else {
      message("%")
      message("% -> Running report mode, i.e. fix = FALSE and database will not be touched")
    }
  }

  # Check arguments (other argument checks at section of respective checks)
  if(!("update_frac_impervious" %in% names(option)))
    option[["update_frac_impervious"]] <- FALSE
  
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
  
  # initialise object where database information will be stored during processing
  dat_all <- NULL
  
  # initialise variable counting the number of checks handled
  checks_done <- 0
  
  

### check current db version ###############################################################################
  if(verbose) message("%")
  if(verbose) message("% Check database version ...")

  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="lumpR")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))

  db_ver <- max(sqlFetch(con, "db_version")$version)
  if(db_ver < db_ver_max) stop(paste0("Database version is prior to version ", db_ver_max, ". Make sure you use the latest database version (consider function db_update())!"))

  if(verbose) message("% OK")

### check fractions (use external function for every relevant table) ###############################################################################
  if (any(grepl("check_fix_fractions", check))) {
    if(verbose) message("%")
    if(verbose) message("% Check fractions ...")
    
    # read data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    # check fractions
    dat_all[c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc")] <- lapply(dat_all[c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc")], check_fix_fractions, fix=fix, update_frac_impervious=option[["update_frac_impervious"]], verbose=verbose)
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check fractions
  
    

### filter small areas (use external function for every relevant table) ###############################################################################
  if (any(grepl("filter_small_areas", check))) {
    if(verbose) message("%")
    if(verbose) message("% Filter small areas ...")
    
    if(!("area_thresh" %in% names(option))) stop("No option 'area_thresh' specified for check 'filter_small_areas'.")
    
    thres <- option[["area_thresh"]]
    
    # read data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc"),
                             con = con,
                             tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    # apply area filter
    dat_all[c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc")] <- lapply(dat_all[c("r_subbas_contains_lu", "r_lu_contains_tc", "r_tc_contains_svc")], filter_small_areas, thres=thres, fix=fix, verbose=verbose)
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # filter small areas



### TC with slope <= 0 ###############################################################################
  if (any(grepl("tc_slope", check))) {
    if(verbose) message("%")
    if(verbose) message("% Find TCs with slope <= 0 ...")
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("terrain_components"), con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    # do check
    if(!any(which(dat_all$terrain_components$slope <= 0))) {
      message("% -> There are no TCs with slope <= 0.")
    } else {
      
      if(!("treat_slope" %in% names(option)))
        stop("No option 'treat_slope' specified for check 'tc_slope'.")
      
      # data from r_lu_contains_tc
      dat_all <- c(dat_all,
                   read_db_dat(tbl = c("r_lu_contains_tc"), con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
      
      # datasets of r_lu_contains_tc where slope is <= 0
      r_tc_zero <- which(dat_all$terrain_components$slope <= 0)
      tc_zero <- dat_all$terrain_components$pid[r_tc_zero]
      lutc_zero <- dat_all$r_lu_contains_tc[which(dat_all$r_lu_contains_tc$tc_id %in% tc_zero),]
      
    
      # option 1: remove TC if areal fraction within LU is below threshold
      if (option[["treat_slope"]][1] == 1 | option[["treat_slope"]][1] == 3) {
        
        if (verbose)
          message("% -> Remove TCs with small fraction within LU...")
        
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
                         " more than 10% of the area would be removed due to too many small '", colnames(lutc_zero)[2], ". No changes done. Consider using smaller threshold to remove less entries."))
            
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
            dat_all$r_lu_contains_tc <- dat_all$r_lu_contains_tc[-which(dat_all$r_lu_contains_tc$tc_id %in% tc_zero),]
            dat_all$r_lu_contains_tc <- rbind(dat_all$r_lu_contains_tc, lutc_zero)
            attr(dat_all$r_lu_contains_tc, "altered") <- TRUE
            
            # re-calculate fractions
            dat_all$r_lu_contains_tc <- check_fix_fractions(dat_tbl=dat_all$r_lu_contains_tc, fix=TRUE, update_frac_impervious=option[["update_frac_impervious"]], verbose=FALSE)
            
          } # still datasets left to remove?
        } # TCs wih fraction below threshold?
  
      } # end of option 1
      
      
      
      # option 2: change slope to specified small positive value
      if (option[["treat_slope"]][1] == 2 | option[["treat_slope"]][1] == 3) {
        
        if (verbose)
          message("% -> Change slope to specified small positive value...")
        
        # determine replace value
        if (option[["treat_slope"]][1] == 2)
          repl_slope <- option[["treat_slope"]][2]
        if (option[["treat_slope"]][1] == 3)
          repl_slope <- option[["treat_slope"]][3]
        
        if(fix) {
          message(paste0("% -> There are ", length(r_tc_zero), " datasets where slopes in 'terrain_components' will be replaced by ", repl_slope))
          dat_all$terrain_components[r_tc_zero,"slope"] <- repl_slope
          attr(dat_all$terrain_components, "altered") <- TRUE
        } else {
          message(paste0("% -> There are ", length(r_tc_zero), " datasets in 'terrain_components' containing slopes <= 0"))
        }
          
      } # end of option 2
    
    } # TC with slope <= 0?
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check tc_slope?


### Assign special areas (water, impervious surfaces) ###############################################################################
  if (any(grepl("special_areas", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Define certain SVCs as special areas ...")
    if(verbose & !fix) message("% Identify special areas in SVCs ...")
    
    # check arguments
    if(!("special_area" %in% names(option)))
      stop("No option 'special_area' specified for check 'special_areas'.")
    
    if(class(option$special_area) != "data.frame")
      stop("Option 'special_area' is not a data.frame.")
    
    if(any(!(c("reference_tbl", "ref_id", "special_id") %in% names(option$special_area))))
      stop("Option 'special_area' does not contain all necessary named vectors.")
    
    if(any(!(unique(option$special_area$reference_tbl) %in% c("vegetation", "soils"))))
      stop("Option 'special_area' vector 'reference_tbl' supports values 'vegetation' and 'soils' only.")
    
    if(any(!(unique(option$special_area$special_id) %in% c(0,1,2))))
      stop("Option 'special_area' vector 'special_id' supports values '0', '1', and '2' only.")
    
    # get SVC data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("soil_veg_components"), con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    svcs_marked <- 0
    
    # vegetation
    if("vegetation" %in% option$special_area$reference_tbl) {
      
      # determine relevant rows and vegetation ids
      rows_veg <- grep("vegetation", option$special_area$reference_tbl)
      
      # loop over rows for vegetation
      for (r in rows_veg) {
        
        # get rows in dat_svc to be adjusted
        svc_rows_adj <- which(dat_all$soil_veg_components$veg_id == option$special_area$ref_id[r])
        
        if(!any(svc_rows_adj)){
          message(paste0("% -> WARNING: Option 'special_area': Vegetation id ", option$special_area$ref_id[r], " could not be found in column 'veg_id' of table 'soil_veg_components'."))
          next
        }
          
        # set special_area flag
        dat_all$soil_veg_components$special_area[svc_rows_adj] <- option$special_area$special_id[r] 
        attr(dat_all$soil_veg_components, "altered") <- TRUE
        svcs_marked <- svcs_marked + length(svc_rows_adj)
      }
      
    } # end vegetation
    
    
    # soils
    if("soils" %in% option$special_area$reference_tbl) {
      
      # determine relevant rows and soil ids
      rows_soil <- grep("soils", option$special_area$reference_tbl)    
      
      # loop over rows for soil
      for (r in rows_soil) {
        
        # get rows in dat_svc to be adjusted
        svc_rows_adj <- which(dat_all$soil_veg_components$soil_id == option$special_area$ref_id[r])
        
        if(!any(svc_rows_adj)){
          message(paste0("% -> WARNING: Option 'special_area': Soil id ", option$special_area$ref_id[r], " could not be found in column 'soil_id' of table 'soil_veg_components'."))
          next
        }
        
        # set special_area flag
        dat_all$soil_veg_components$special_area[svc_rows_adj] <- option$special_area$special_id[r]
        attr(dat_all$soil_veg_components, "altered") <- TRUE
        svcs_marked <- svcs_marked + length(svc_rows_adj)
      }
      
    } # end soils
    
    if(verbose | !fix) message(paste0("% -> ", svcs_marked, " special area(s) found in SVCs."))
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check special_areas





### remove water SVCs ###############################################################################
  if (any(grepl("remove_water_svc", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Remove SVCs marked as water ...")
    if(verbose & !fix) message("% Identify SVCs marked as water ...")
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("soil_veg_components", "r_tc_contains_svc"), con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    # identify water SVCs
    rows_water <- which(dat_all$soil_veg_components$special_area == 1)
    svc_water <- dat_all$soil_veg_components$pid[rows_water]
    rows_contains_water <- which(dat_all$r_tc_contains_svc$svc_id %in% svc_water)
    
    if (length(rows_contains_water)==0) {
      message("% -> No water-SVCs found, nothing done.")          
    } else {
      if(!fix) {
        message(paste0("% -> There are ", length(rows_contains_water), " datasets in 'r_tc_contains_svc' identified as water areas"))
      } else {
        message(paste0("% -> There are ", length(rows_contains_water), " datasets going to be removed from 'r_tc_contains_svc' ('fraction' will be updated)"))
      
        # remove water SVCs
        dat_all$r_tc_contains_svc <- dat_all$r_tc_contains_svc[-rows_contains_water,]
        attr(dat_all$r_tc_contains_svc, "altered") <- TRUE
        
        # update fractions
        dat_all$r_tc_contains_svc <- check_fix_fractions(dat_tbl=dat_all$r_tc_contains_svc, fix = TRUE, update_frac_impervious=option[["update_frac_impervious"]], verbose = FALSE)
      } # if fix
    } # if water SVCs found
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check remove_water_svc




### compute rocky fractions for TCs ###############################################################################
  if (any(grepl("compute_rocky_frac", check))) {
    if(verbose) message("%")
    if(verbose) message("% Compute rocky (i.e. impervious) fractions for TCs ...")
    
    if(!fix) {
      message("% -> Running in report mode but nothing to report for this check.")
      } else {
      
        # get data
        dat_all <- c(dat_all,
                     read_db_dat(tbl = c("soil_veg_components", "terrain_components", "horizons", "r_tc_contains_svc"),
                                 con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
        
        # identify existing entries in frac_rocky (herein computed values will be added)
        if(any(is.na(dat_all$terrain_components$frac_rocky))) {
          dat_all$terrain_components$frac_rocky[is.na(dat_all$terrain_components$frac_rocky)] = 0 #set NAs to 0
          attr(dat_all$terrain_components, "altered") <- TRUE
          message("% -> NA values in 'terrain_components' column 'frac_rocky' set to zero.")
        }
        if (nrow(dat_all$terrain_components)>0 & max(dat_all$terrain_components$frac_rocky)>0)
          message("% -> ATTENTION: Column 'frac_rocky' in table 'terrain_components' already contains some values. The computed fractions will be added to these.")
    
        # identify soils with 100% coarse fragments in topmost horizon
        soil_impervious <- dat_all$horizons$soil_id[which(dat_all$horizons$position == 1 & dat_all$horizons$coarse_frag == 1)]
        
        # mark corresponding SVCs as impervious
        if(any(soil_impervious)) {
          rows_svc_impervious <- which(dat_all$soil_veg_components$soil_id %in% soil_impervious)
          
          if(fix) {
            message(paste0("% -> There are ", length(rows_svc_impervious), " datasets in 'soil_veg_components' which will be marked as impervious due to 100% coarse fragments in topmost soil horizon"))
          } else {
            message(paste0("% -> There are ", length(rows_svc_impervious), " datasets in 'soil_veg_components' identified as impervious due to 100% coarse fragments in topmost soil horizon"))
          }
          
          dat_all$soil_veg_components$special_area[rows_svc_impervious] <- 2
          attr(dat_all$soil_veg_components, "altered") <- TRUE
        } else {
          if (verbose)
            message("% -> No topmost horizon with 100% coarse fragments could be found.")
        }
        
        # identify TCs containing impervious SVCs
        svc_impervious <- dat_all$soil_veg_components$pid[which(dat_all$soil_veg_components$special_area == 2)]
        rows_tc_impervious <- which(dat_all$r_tc_contains_svc$svc_id %in% svc_impervious)
        
        # compute frac_rocky for every TC
        if (length(rows_tc_impervious) > 0) {
          if(verbose) message(paste0("% -> Found ", length(svc_impervious), " impervious SVCs. Adding areal fractions to corresponding TCs..."))
          tc_rocky <- tapply(dat_all$r_tc_contains_svc$fraction[rows_tc_impervious], list(parent=dat_all$r_tc_contains_svc$tc_id[rows_tc_impervious]), sum)
          for (t in 1:length(tc_rocky)) {
            row <- which(dat_all$terrain_components$pid == names(tc_rocky)[t])
            dat_all$terrain_components$frac_rocky[row] <- dat_all$terrain_components$frac_rocky[row] + tc_rocky[t]
            #also update dummy records in r_tc_contains_svc (used internally for more consistent normalization, removed later)
            row <- which(dat_all$r_tc_contains_svc$tc_id  == names(tc_rocky)[t] &
                         dat_all$r_tc_contains_svc$svc_id == -1)   #find dummy record containing rocky fraction
            dat_all$r_tc_contains_svc$fraction[row] <- dat_all$r_tc_contains_svc$fraction[row] + tc_rocky[t]
          }
          attr(dat_all$terrain_components, "altered") <- TRUE
        } else
          if(verbose) message("% -> No impervious SVCs found.")
        if (!("remove_impervious_svc" %in% check))
          message("% Please remember to call db_check(check='compute_rocky_frac', ...) later.")
    } # if fix
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check compute_rocky_frac
  





### remove impervious SVCs ###############################################################################
  if (any(grepl("remove_impervious_svc", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Remove SVCs marked as impervious ...")
    if(verbose & !fix) message("% Identify SVCs marked as impervious ...")
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("soil_veg_components", "r_tc_contains_svc"),
                             con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    # identify impervious SVCs
    rows_impervious <- which(dat_all$soil_veg_components$special_area == 2)
    svc_impervious <- dat_all$soil_veg_components$pid[rows_impervious]
    rows_contains_impervious <- which(dat_all$r_tc_contains_svc$svc_id %in% svc_impervious)
    
    if(!any(rows_contains_impervious)) {
      message("% -> No impervious SVCs could be found. Nothing done.")
    } else {
    
      if(!fix) {
        message(paste0("% -> There are ", length(rows_contains_impervious), " datasets in 'r_tc_contains_svc' identified as impervious"))
      } else {
        message(paste0("% -> There are ", length(rows_contains_impervious), " datasets which will be removed from 'r_tc_contains_svc' ('fraction' will be updated)"))
        
        # remove impervious SVCs
        dat_all$r_tc_contains_svc <- dat_all$r_tc_contains_svc[-rows_contains_impervious,]
        attr(dat_all$r_tc_contains_svc, "altered") <- TRUE
        
        # update fractions
        dat_all$r_tc_contains_svc <- check_fix_fractions(dat_tbl=dat_all$r_tc_contains_svc, fix = TRUE, update_frac_impervious=option[["update_frac_impervious"]], verbose = FALSE)
      } # if fix
      
    } # if any impervious SVC
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check remove_impervious_svc
  




### estimate frgw_delay ###############################################################################
  if (any(grepl("proxy_frgw_delay", check))) {
    if(verbose) message("%")
    if(verbose) message("% Estimate frgw_delay for LUs ...")
    
    # check arguments
    if(!("total_mean_delay" %in% names(option)))
      stop("No option 'total_mean_delay' specified for check 'proxy_frgw_delay'.")
    
    if(!fix) {
      message("% -> Running in report mode but nothing to report for this check.")
    } else {
    
      # get data
      dat_all <- c(dat_all,
                   read_db_dat(tbl = c("terrain_components", "landscape_units", "r_lu_contains_tc"),
                               con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
      
      # compute area-weighted average slope for every LU
      dat_contains_t <- merge(dat_all$r_lu_contains_tc, dat_all$terrain_components, by.y="pid", by.x="tc_id")
      slope_lu <- sapply(split(dat_contains_t, dat_contains_t$lu_id), function(x) weighted.mean(x$slope, x$fraction))
      dat_lu_t <- merge(dat_all$landscape_units, data.frame(id=names(slope_lu), slope=slope_lu), by.x="pid", by.y="id")
      
      # compute area-weighted average rocky fraction for every lu
      rocky_lu <- sapply(split(dat_contains_t, dat_contains_t$lu_id), function(x) weighted.mean(x$frac_rocky, x$fraction))
      dat_lu_t <- merge(dat_lu_t, data.frame(id=names(rocky_lu), rocky=rocky_lu), by.x="pid", by.y="id")
      
      # estimate proxy
      proxy <- dat_lu_t$slopelength * (1-dat_lu_t$rocky) / dat_lu_t$slope
      
      # in case of zero slopes set infinite proxy values to 10000 days
      if(any(is.infinite(proxy)))
        proxy[which(is.infinite(proxy))] <- 10000
      
      # estimate frgw_delay
      dat_all$landscape_units$frgw_delay <- proxy * option$total_mean_delay / mean(proxy)
      attr(dat_all$landscape_units, "altered") <- TRUE
    } # if fix
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check proxy_frgw_delay
  
  



### delete obsolete datasets or check for completeness ###############################################################################
### define chains of table dependencies
  if (any(grepl("delete_obsolete", check)) | any(grepl("completeness", check))) {
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins", "landscape_units", "terrain_components", "soil_veg_components",
                                     "vegetation", "soils", "horizons", "particle_classes", "rainy_season",
                                     "r_subbas_contains_lu",  "r_lu_contains_tc", "r_tc_contains_svc", "r_soil_contains_particles", "reservoirs_strategic",
                                     "r_subbas_contains_reservoirs_small", "reservoirs_small_classes"),
                             con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    
    # database tables
    tbl_db <- sqlTables(con)[,"TABLE_NAME"]
    
    # chain definitions
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
    
    table_chain8= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (8)
       rbind(
         c (table = "subbasins",           key2prev=""      , key2next="pid"),
         c (table = "reservoirs_strategic",    key2prev="pid", key2next="")
       ))
    
    table_chain9= data.frame(stringsAsFactors = FALSE,      #scheme of table relations (9)
                             rbind(
                               c (table = "subbasins",           key2prev=""      , key2next="pid"),
                               c (table = "r_subbas_contains_reservoirs_small",    key2prev="subbas_id", key2next="res_class_id"),
                               c (table = "reservoirs_small_classes",    key2prev="pid", key2next="")
                             ))
    
    wildcard_fields = list(  #tables with wildcard values ("-1") need to be considered
      rainy_season = c("subbas_id", "veg_id"),
      x_seasons     = c("subbas_id", "svc_id")
    )
    chain_list = list(table_chain1, table_chain2, table_chain3, table_chain4, table_chain5, table_chain6, table_chain7, table_chain8, table_chain9) #list of all relation chains that need to be checked
  }
    
### delete obsolete datasets ###############################################################################
    
  if (any(grepl("delete_obsolete", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Delete obsolete records ...")
    if(verbose & !fix) message("% Identify obsolete records ...")
    
    if(!("tbls_preserve" %in% names(option))) 
      stop("Missing option 'tbls_preserve' to run check 'delete_obsolete'!")
    
    missing_tables=NULL #record missing tables
    
    for (table_chain in chain_list)
    {
        for (i in 2:nrow(table_chain))
        {
          cur_table = table_chain$table[i]
          
          if (cur_table %in% option[["tbls_preserve"]])
          {
            message(paste0("% -> Table '", cur_table,"' ignored, because listed in options[['tbls_preserve']]"))
            next
          }
          
          pre_table = table_chain$table[i-1]
          
          if (any(!(c(pre_table, cur_table) %in% tbl_db)))
          {
            missing_tables=c(missing_tables, c(pre_table, cur_table))
            next
          }
          
          
          # exclude wildcards
          dat_cur <- dat_all[[cur_table]][[table_chain$key2prev[i]]]
          dat_cur <- dat_cur[which(dat_cur != -1)]
          dat_pre <- dat_all[[pre_table]][[table_chain$key2next[i-1]]]
          dat_pre <- dat_pre[which(dat_pre != -1)]
          
          if (length(dat_cur) == 0 | length(dat_pre) == 0) { #"length(dat_pre) == 0": if the table is empty, don't delete obsoletes
            if(verbose) message(paste0("% -> No obsolete datasets found in '", cur_table,"' with reference to '", pre_table, "'"))
            next
          }
          
          # identify obsolete records
          r_obsol <- which(!(dat_cur %in% dat_pre))
          
          if(length(r_obsol) > 0) {
            if (verbose) message(paste0("% -> There are ", length(r_obsol), " obsolete datasets in '", cur_table, "' not appearing in '", pre_table, "'"))
            
            if(!fix) {
              if(verbose) message("% -> More records may turn up after actual cleaning (fix=TRUE).")
            } else {
              if (cur_table %in% option$tbls_preserve) {
                if(verbose) message("% -> Obsolete datasets of this table will be preserved as specified")
              } else {
                if(verbose) message("% -> Datasets will be removed (and fraction updated accordingly where necessary)")
                # delete obsolete datasets
                dat_all[[cur_table]] <- dat_all[[cur_table]][-r_obsol,]
                attr(dat_all[[cur_table]], "altered") <- TRUE
                
                # re-calculate fractions of contains tables
                if(grepl("contains", cur_table) & cur_table != "r_subbas_contains_reservoirs_small")
                  dat_all[[cur_table]] <- check_fix_fractions(dat_tbl= dat_all[[cur_table]], fix=TRUE, update_frac_impervious=option[["update_frac_impervious"]], verbose=FALSE)
              }
            }
          } else if(verbose) message(paste0("% -> No obsolete datasets found in '", cur_table,"' with reference to '", pre_table, "'"))
          
        } # loop current table_chain
      
    }  # loop chain_list
    if (length(missing_tables)>0)
      message(paste0("% -> Warning: Table(s) '", paste0(setdiff(unique(missing_tables), tbl_db), collapse = "', '")," not found, checks skipped."))
    else
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # check delete_obsolete





### check completeness ###############################################################################
  if (any(grepl("completeness", check))){
    if(verbose) message("%")
    if(verbose) message("% Search referenced datasets without specification...")

    for (table_chain in chain_list)
    {  
      # check that all tables in table_chain are in the database to avoid conflicts with optional tables
      if(any(!(table_chain$table %in% tbl_db)))
        next
      else {
        for (i in 1:(nrow(table_chain)-1))
        {
          cur_table = table_chain$table[i]
          nex_table = table_chain$table[i+1]
          
          # exclude wildcards
          dat_cur <- dat_all[[cur_table]][[table_chain$key2next[i]]]
          dat_cur <- dat_cur[which(dat_cur != -1)]
          dat_nex <- dat_all[[nex_table]][[table_chain$key2prev[i+1]]]
          dat_nex <- dat_nex[which(dat_nex != -1)]
          
          dat_cur = unique(dat_cur) #simplify further treatment, no need to consider duplicates
          dat_nex = unique(dat_nex)
          if(length(dat_cur) > 0 & length(dat_nex) > 0) {
            # identify excessive datasets
            dat_excess <- list(cur=setdiff(dat_cur, dat_nex),
                               nex=setdiff(dat_nex, dat_cur) )
            
            if(any(sapply(dat_excess, any))) {
              if(any(dat_excess$cur)) message(paste0("% -> WARNING: Table '", cur_table, "' contains ", length(dat_excess$cur), " dataset(s) not referenced in '", nex_table, "': ", paste(dat_excess$cur[1:min(10, length(dat_excess$cur))], collapse=", ")))
              if(any(dat_excess$nex)) message(paste0("% -> WARNING: Table '", nex_table, "' contains ", length(dat_excess$nex), " dataset(s) not referenced in '", cur_table, "': ", paste(dat_excess$nex[1:min(10, length(dat_excess$nex))], collapse=", ")))
            } else if(verbose)
              message(paste0("% -> All datasets of '", cur_table,"' appear in referenced '", nex_table, "' and vice versa"))
          } else if(verbose)
            message(paste0("% -> All datasets of '", cur_table,"' appear in referenced '", nex_table, "' and vice versa"))
        }
      }
    } 
  
    # check for multiple use of TC in LUs (currently not allowed by structure of WASA input files)
    occ <- table(dat_all$r_lu_contains_tc$tc_id)
    if(any(occ > 1))
      message(paste0("% -> TC(s) ", paste0(names(occ[which(occ > 1)]), collapse=", "), " is/are duplicated and/or part of multiple LUs, which is currently not supported. Duplicate these TCs and assign a different ID for each instance."))
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  }   # check completeness



### determine subbasin order ###############################################################################
  if (any(grepl("subbasin_order", check))) {
    if(verbose) message("%")
    if(verbose & fix) message("% Determine subbasin order (write into column 'a_stream_order' of table 'subbasins') ...")
    if(verbose & !fix) message("% Determine subbasin order (report mode, no changes to database) ...")
  
    if(!("overwrite" %in% names(option)))
      stop("Option 'overwrite' to run check 'subbasin_order'!")
    
    # get data
    dat_all <- c(dat_all,
                 read_db_dat(tbl = c("subbasins"), con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
    stream_order_old <- dat_all$subbasins$a_stream_order #keep for comparison
    
    # identify outlet subbasin
    r_outlet <- which(dat_all$subbasins$drains_to %in% c(9999,-9999,999,-999))
    
    if (!any(r_outlet))
      stop("Could not identify outlet subbasin from column 'drains_to' in table 'subbasins'. Must be one of values c(9999,-9999,999,-999).")
    
    if((length(r_outlet) > 1))
      stop("More than one subbasin has been identified as outlet. Check column 'drains_to' in table 'subbasins'!")
    
    # determine stream order
    stream_order <- rep(NA, nrow(dat_all$subbasins))
    stream_order[r_outlet] <- 1
    
    # determine rest of stream order
    fin <- FALSE
    i <- 0
    while(fin == F) {
      i <- i+1
      
      # determine indices of subbasins of previously filled 'a_stream_order' (downstream subbasins)
      r <- which(stream_order == i)
      
      # get pid(s) of downstream subbasin(s)
      sub_up <- dat_all$subbasins$pid[r]
      
      # find this upstream subbasins to current downstream subbasins by checking 'drains_to'
      r_up <- which(dat_all$subbasins$drains_to %in% sub_up)
      
      # set corresponding 'a_stream_order' value to i+1
      stream_order[r_up] <- i+1
      
      # check if finished
      if(!any(is.na(stream_order)))
        fin <- TRUE
      
      # throw an error if i is already very large (In this case there must be something wrong)
      if (i > 10000)
        stop("Cannot successfully determine subbasin order (column 'a_stream_order' of table 'subbasins'). Check the table for errors!")
    }
    
    if (all(!is.na(dat_all$subbasins$a_stream_order)) & all(dat_all$subbasins$a_stream_order==stream_order)) 
      message("% -> existing stream order ok.")
    else {
      if(!fix) {
        message("% -> existing stream order needs updating. Consider running with 'fix=TRUE' and 'option=list(overwrite=TRUE)'") 
      } else {
        
        if(is.null(option$overwrite) | !option$overwrite)
          stop("There are already values in column 'a_stream_order' of table 'subbasins'. Use option=list(overwrite=TRUE) or manually set them all to 'NULL' if you want to compute subbasin order!")
        
        dat_all$subbasins$a_stream_order <- stream_order
        attr(dat_all$subbasins, "altered") <- TRUE
      } # if fix
    } # modify stream order
    
    if(verbose) message("% OK")
    checks_done <- checks_done+1
  } # determine subbasin order




### POST-PROCESSING ###############################################################################
  
  if(checks_done != length(check))
    stop(paste0("Number of processed checks (", checks_done, ") is not equal to the number of specified checks (", length(check), ")! Maybe you misspelled a check? Check your argument 'check' and re-run the function. Database will not be touched."))
  
  if(fix) {
    if(verbose) message("%")
    if(verbose) message("% Write changes into database and update 'meta_info' (might take a while) ...")
    
    # update rocky fraction for TCs if necessary
    if("r_tc_contains_svc" %in% names(dat_all)) {
      rocky_frac <- dat_all$r_tc_contains_svc[dat_all$r_tc_contains_svc$svc_id==-1,] #extract information on rocky fraction - this needs to go into another table
      dat_all$r_tc_contains_svc <- dat_all$r_tc_contains_svc[dat_all$r_tc_contains_svc$svc_id!=-1,] #keep only real SVCs, discard rocky fractions that had been temporally inserted as SVCs
      if(nrow(rocky_frac) > 0) {
        rocky_frac$svc_id <- NULL #svc_id=-1 was just a marker for rocky fractions, not needed anymore
        if(!("terrain_components" %in% names(dat_all)))
          dat_all <- c(dat_all, read_db_dat(tbl = c("terrain_components"), con = con, tbl_exist = names(dat_all), update_frac_impervious=option[["update_frac_impervious"]]))
        terrain_components <- merge(dat_all$terrain_components, rocky_frac, by.x="pid", by.y="tc_id")

        if (!identical(terrain_components$frac_rocky, terrain_components$fraction)) #ii: this is crap
          dat_all$terrain_components$frac_rocky <- terrain_components$fraction #correct to adjusted rocky fraction
      }
    }
    
    # update db
    tbls_changed <- sapply(dat_all, function(x) attr(x, "altered"))
    
    if(any(tbls_changed)) {
      junk <- lapply(dat_all[tbls_changed], function(x) modify_db(con, x))
      # update table meta_info
      write_metainfo(con,
                     "db_check()",
                     paste(names(which(tbls_changed)), collapse=", "), "various",
                     paste0("Database checked and adjusted using R package lumpR. Applied checks: ", paste(check, collapse=", "), ". Options: ", paste(names(option), option, sep=" = ", collapse=", ")),
                     FALSE)
      if(verbose) message(paste0("% -> Updated table(s) ", paste(names(which(tbls_changed)), collapse=", ")))
    } else {
      if(verbose) message("% -> No tables were updated, nothing to do")
    }
    if(verbose) message("% OK")
  }


  if(verbose) message("%")
  if(verbose) message("% All checks completed successfully. Closing ODBC connection.")
  
  tryCatch(odbcClose(con), error=function(e){})
  
  if(verbose) message("%")
  if(verbose) message("% DONE!")
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
} # EOF
