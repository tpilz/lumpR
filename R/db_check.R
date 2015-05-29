#'  Check parameter database for consistency
#'  
#'  Function includes several options to check the parameter database for completeness
#'  and consistency.
#'  
#'  @param dbname Name of the data source (DSN) registered at ODBC.
#'  
#'  @param check Character vector specifying what shall be checked. See \code{Details}.
#'  
#'  @param option A list of options for certain checks. See \code{Details}
#'  for the different options and their meaning.
#'  
#'  @param verbose \code{logical}. Should detailed information during execution be
#'  printed? Default: \code{FALSE}. When \code{TRUE} output of writing updated values
#'  into database can be rather long so you might want to direct output into an
#'  external log file.
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
#'  will run faster by removing unnecessary computational burden. If the fraction to be
#'  removed is greater than 10% of the total area of a next higher spatial level's class
#'  datasets will be kept. In this case you might try a smaller value for area_thresh'.\cr
#'  \emph{Option: 'area_thresh'}\cr
#'  A threshold defining the minimum areal fraction of a certain spatial disaggregation
#'  unit within the next higher spatial level. Default: 0.01.
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
#'  2: Change slope in \% of affected TCs to a small positive value specified as second
#'  value of the vector.\cr
#'  3: A combination of the two former choices whereas option 1 is applied before
#'  option 2 and the second value of the vector defining the areal threshold and the
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
#'  will be updated.
#'  
#'  \bold{compute_rocky_frac}\cr
#'  Compute rocky fractions, i.e. fractions of impervious surfaces, for TCs (table
#'  'terrain_components', column 'frac_rocky') from impervious SVCs (column 'special_area'
#'  in 'soil_veg_components' equal to 2) and topmost soil horizons (in table horizons'
#'  column 'position' equal to 1 and 'coarse_frag' equal to 1). These undergo special
#'  treatment in the WASA model. SVCs with soil profile containing 100\% coarse
#'  fragments in topmost horizon will be marked as impervious.
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
#'  catchment. All proxy values are scaled, so their mean matches this value (see formula
#'  above).
#'  
#'  
#'  @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'  
#'  @export

db_check <- function(
  dbname,
  check = c("filter_small_areas", "tc_slope", "special_areas", "remove_water_svc",
            "compute_rocky_frac", "remove_impervious_svc", "proxy_frgw_delay"),
  option = list(area_thresh=0.01,
                treat_slope=c(3,0.01,0.1)),
  verbose=FALSE
) {
  
  if (verbose)
    print("Loading package 'RODBC' and connecting to database ...")
  
  # load ODBC R interface
  require(RODBC)
  
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
  
  
###############################################################################
### check current db version
  if(verbose)
    print("Check database version ...")
  db_ver <- sqlFetch(con, "db_version")$version
  if(max(db_ver) != 19) {
    odbcClose(con)
    stop("Database version is not equal to 19. Make sure you use the latest database version 19 (consider function db_update())!")
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
    filter_small_areas(con=con, table="r_subbas_contains_lu", thres=thres, verbose=verbose)
    
    # TCs
    filter_small_areas(con=con, table="r_lu_contains_tc", thres=thres, verbose=verbose)
    
    # SVCs
    filter_small_areas(con=con, table="r_tc_contains_svc", thres=thres, verbose=verbose)
    
    if(verbose)
      print("OK.")
  } # filter small areas


###############################################################################
### TC with slope <= 0
  if (any(grepl("tc_slope", check))) {
    if(verbose)
      print("Find and handle TCs with slope <= 0 ...")
    
    # get data
    dat_tc <- sqlFetch(con, "terrain_components")
    if(!any(which(dat_tc$slope <= 0))) {
      print("-> There are no TCs with slope <= 0.")
    } else {
      
      if(!("treat_slope" %in% names(option))) {
        odbcClose(con)
        stop("No option 'treat_slope' specified for check 'tc_slope'.")
      }
      
      # data from r_lu_contains_tc
      dat_lutc <- sqlFetch(con, "r_lu_contains_tc")
      
      # datasets of r_lu_contains_tc where slope is <= 0
      tc_zero <- dat_tc$pid[which(dat_tc$slope <= 0)]
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
          
          print("-> The following datasets will be removed from 'r_lu_contains_tc' ('fraction' will be updated):")
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
          repl_slope <- option[["treat_slope"]][2]
        
        # replace slope value
        dat_tc[tc_zero,"slope"] <- repl_slope
      } # end of option 2
      
      
      
      # update database
      if(verbose)
        print("-> Updating table 'r_lu_contains_tc'...")
      tryCatch(
      {
        sqlQuery(con, "delete from r_lu_contains_tc")
        sqlSave(channel=con, tablename = "r_lu_contains_tc", dat=dat_out, verbose=verbose, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
      }, error = function(e) {
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
        sqlSave(channel=con, tablename = "terrain_components", dat=dat_tc, verbose=verbose, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
      }, error = function(e) {
        odbcClose(con)
        stop(paste0("An error occured when updating table 'terrain_components'. ",
                    "Error message of the writing function: ", e))
      }
      )
    
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
      odbcClose(con)
      stop("No option 'special_area' specified for check 'special_areas'.")
    }
    
    if(class(option$special_area) != "data.frame") {
      odbcClose(con)
      stop("Option 'special_area' is not a data.frame.")
    }
    
    if(any(!(c("reference_tbl", "ref_id", "special_id") %in% names(option$special_area)))) {
      odbcClose(con)
      stop("Option 'special_area' does not contain all necessary named vectors.")
    }
    
    if(any(!(unique(option$special_area$reference_tbl) %in% c("vegetation", "soils")))) {
      odbcClose(con)
      stop("Option 'special_area' vector 'reference_tbl' supports values 'vegetation' and 'soils' only.")
    }
    
    if(any(!(unique(option$special_area$special_id) %in% c(0,1,2)))) {
      odbcClose(con)
      stop("Option 'special_area' vector 'special_id' supports values '0', '1', and '2' only.")
    }
    
    
    # get SVC data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    
    
    # vegetation
    if("vegetation" %in% option$special_area$reference_tbl) {
      
      # determine relevant rows and vegetation ids
      rows_veg <- grep("vegetation", option$special_area$reference_tbl)    
      
      # loop over rows for vegetation
      for (r in rows_veg) {
        
        # get rows in dat_svc to be adjusted
        svc_rows_adj <- which(dat_svc$veg_id == option$special_area$ref_id[r])
        
        if(!any(svc_rows_adj)){
          odbcClose(con)
          stop(paste0("Option 'special_area': Vegetation id ", option$special_area$ref_id[r], " could not be found in column 'veg_id' of table 'soil_veg_components'."))
        }
        
        # set special_area flag
        dat_svc$special_area[svc_rows_adj] <- option$special_area$special_id[r] 
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
          odbcClose(con)
          stop(paste0("Option 'special_area': Soil id ", option$special_area$ref_id[r], " could not be found in column 'soil_id' of table 'soil_veg_components'."))
        }
        
        # set special_area flag
        dat_svc$special_area[svc_rows_adj] <- option$special_area$special_id[r] 
      }
      
    } # end soils
    
    
    # update table
    if(verbose)
      print("-> Updating table 'soil_veg_components'...")
    tryCatch(
    {
      sqlQuery(con, "delete from soil_veg_components")
      sqlSave(channel=con, tablename = "soil_veg_components", dat=dat_svc, verbose=verbose, 
              append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
    }, error = function(e) {
      odbcClose(con)
      stop(paste0("An error occured when updating table 'soil_veg_components'. ",
                  "Error message of the writing function: ", e))
    }
    )
    
    if(verbose)
      print("OK.")
    
  } # check special_areas





###############################################################################
### remove water SVCs
  if (any(grepl("remove_water_svc", check))) {
    if(verbose)
      print("Remove SVCs marked as water ...")
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    # identify water SVCs
    rows_water <- which(dat_svc$special_area == 1)
    svc_water <- dat_svc$pid[rows_water]
    rows_contains_water <- which(dat_contains$svc_id %in% svc_water)
    
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
      sqlSave(channel=con, tablename = "r_tc_contains_svc", dat=dat_contains_act, verbose=verbose, 
              append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
    }, error = function(e) {
      odbcClose(con)
      stop(paste0("An error occured when updating table 'r_tc_contains_svc'. ",
                  "Error message of the writing function: ", e))
    }
    )
    
    
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
    
    # identify soils with 100% coarse fragments in topmost horizon
    soil_impervious <- dat_hor$soil_id[which(dat_hor$position == 1 & dat_hor$coarse_frag == 1)]
    
    # mark corresponding SVCs as impervious
    if(any(soil_impervious)) {
      rows_svc_impervious <- which(dat_svc$soil_id %in% soil_impervious)
      
      print("-> The following datasets in 'soil_veg_components' will be marked as impervious due to 100% coarse fragments in topmost soil horizon:")
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
    dat_tc$frac_rocky <- 0
    tc_rocky <- tapply(dat_contains$fraction[rows_tc_impervious], list(parent=dat_contains$tc_id[rows_tc_impervious]), sum)
    for (t in 1:length(tc_rocky)) {
      row <- which(dat_tc$pid == names(tc_rocky)[t])
      dat_tc$frac_rocky[row] <- tc_rocky[t]
    }
    
    
    # update database
    if(verbose)
      print("-> Updating table 'terrain_components'...")
    tryCatch(
    {
      sqlQuery(con, "delete from terrain_components")
      sqlSave(channel=con, tablename = "terrain_components", dat=dat_tc, verbose=verbose, 
              append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
    }, error = function(e) {
      odbcClose(con)
      stop(paste0("An error occured when updating table 'terrain_components'. ",
                  "Error message of the writing function: ", e))
    }
    )
    
    if(verbose)
      print("OK.")
    
  } # check compute_rocky_frac
  





###############################################################################
### remove impervious SVCs
  if (any(grepl("remove_impervious_svc", check))) {
    if(verbose)
      print("Remove SVCs marked as impervious ...")
    
    # get data
    dat_svc <- sqlFetch(con, "soil_veg_components")
    dat_contains <- sqlFetch(con, "r_tc_contains_svc")
    
    # identify impervious SVCs
    rows_impervious <- which(dat_svc$special_area == 2)
    svc_impervious <- dat_svc$pid[rows_impervious]
    rows_contains_impervious <- which(dat_contains$svc_id %in% svc_impervious)
    
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
      sqlSave(channel=con, tablename = "r_tc_contains_svc", dat=dat_contains_act, verbose=verbose, 
              append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
    }, error = function(e) {
      odbcClose(con)
      stop(paste0("An error occured when updating table 'r_tc_contains_svc'. ",
                  "Error message of the writing function: ", e))
    }
    )
  
  
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
      odbcClose(con)
      stop("No option 'total_mean_delay' specified for check 'proxy_frgw_delay'.")
    }
    
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
    
    # estimate frgw_delay
    dat_lu$frgw_delay <- proxy * option$total_mean_delay / mean(proxy)
    
    
    # update database
    if(verbose)
      print("-> Updating table 'landscape_units'...")
    tryCatch(
    {
      sqlQuery(con, "delete from landscape_units")
      sqlSave(channel=con, tablename = "landscape_units", dat=dat_lu, verbose=verbose, 
              append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
    }, error = function(e) {
      odbcClose(con)
      stop(paste0("An error occured when updating table 'landscape_units'. ",
                  "Error message of the writing function: ", e))
    }
    )
    
    
    if(verbose)
      print("OK.")
    
  } # check proxy_frgw_delay
  



###############################################################################
### end of function, close connection

  if (verbose)
    print("All Checks completed. Close ODBC connection.")
  
  odbcClose(con)
  
} # EOF
