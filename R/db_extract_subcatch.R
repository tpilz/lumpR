# lumpR/db_extract_subcatch.R
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

#' Extracts sub-catchment(s) for specific subbasin(s) from a database
#' 
#' This function extracts the sub-catchment(s) for selected subbasin(s) from an
#' existing and fully pre-processed WASA-SED parameter database.
#' 
#' @param dbname Name of the data source (DSN) registered at ODBC. See \code{Details} of
#' \code{\link[lumpR]{db_create}}.
#' 
#' @param sub_extract \code{vector} of type numeric with the 'pid'(s) of table
#' 'subbasins' in database \code{dbname} for which the associated sub-catchments
#' shall be extracted.
#' 
#' @param dbname_new \code{vector} of type character with the names of the databases
#' containing the extracted sub-catchments. Database need to be registered for ODBC
#' (see \code{Details} of \code{\link[lumpR]{db_create}}).
#' 
#' @param ncores Ineger specifying number of cores that shall be used for computation.
#' Needs package doMC (Linux only) or doParallel for ncores > 1. Default: 1. See Note.
#' 
#' @param verbose \code{logical}. Should detailed information during execution be
#'  printed? Default: \code{TRUE}.
#'  
#' @return Function returns nothing. Only the databases are processed.
#' 
#' @note Running on multiple CPU cores (option \code{ncores}) does not work properly.
#' On Linux, just several threads are stared and processed but it is not real parallel
#' processing, which presumably does not work with ODBC. However, it runs a bit faster
#' than in serial mode. On Windows, however, my tests with \code{ncores > 1} resulted in errors.
#' So, if it does not work on your Windows machine neither, set \code{ncores = 1}.
#' 
#' @details The function first copies the original databse. Then the upstream subbasins
#' of the selected outlet subbasin are identified and the copied database is pruned
#' accordingly. The latter only affects the tables subbasins, landscape_units,
#' terrain_components, r_subbas_contains_lu, r_lu_contains_tc and r_tc_contains_svc.
#' Thus, the other tables may contain redundant information which are contained to
#' limit the processing time and retain the opportunity to (re-)include information
#' later on.
#' 
#' Processing time might be rather long depending on \code{ncores}, the number of
#' datasets to be processed, the underlying DBMS (sqlite is rather slow), and your
#' operating system (Windows was always faster in my tests).
#'  
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}
#'  
db_extract_subcatch <- function(
  dbname = NULL,
  sub_extract = NULL,
  dbname_new = NULL,
  ncores=1,
  verbose = T
) {
  
  # CHECKS #
  if(verbose)
    message("Checking Arguments and pre-processing databases ...")
  # argument checks
  if(is.null(dbname))
    stop("Argument 'dbname' is undefined!")
  if(is.null(sub_extract))
    stop("Argument 'sub_extract' is undefined!")
  if(!is.vector(sub_extract) & class(sub_extract) != "numeric")
    stop("Argument 'sub_extract' is not a vector of class numeric!")
  if(is.null(dbname_new))
    stop("Argument 'dbname_new' is undefined!")
  if(length(dbname_new) != length(sub_extract))
    stop("Arguments 'dbname_new' and 'sub_extract' need to be vectors of the same length!")
  
  # connect to ODBC registered database
  con <- connect_db(dbname)
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # check current db version
  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="lumpR")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))
  
  db_ver <- max(sqlFetch(con, "db_version")$version)
  if(db_ver < db_ver_max) {
    odbcClose(con)
    stop(paste0("Database version is prior to version ", db_ver_max, ". Make sure you use the latest database version (consider function db_update())!"))
  }
  
  # create and connect to databases of dbname_new
  junk <- lapply(dbname_new, db_create)
  junk <- lapply(dbname_new, db_update)
  con_new <- lapply(dbname_new, connect_db)
  
  # initialize parallelism
  if (ncores>1)
  {  
    if(suppressPackageStartupMessages(require(doMC)))
      # register cores
      registerDoMC(cores=ncores) else
        if (suppressPackageStartupMessages(require(doParallel)))
        {
          cl <- makePSOCKcluster(ncores) #make cluster, so we can explicitly close it later
          registerDoParallel(cl)
        } else
        {
          warning("No package for parallel backend (doMC, doParallel) found, reverting to single-core mode")
          ncores=1
        }
  }
  
  if (ncores==1) registerDoSEQ() # specify that %dopar% should run sequentially
  
  if(verbose)
    message(paste0(getDoParWorkers(), " CPU core(s) initialised."))
  
  
  if(verbose)
    message("OK.")
  
  
  
  # Extract SUBCATCHMENTS #
  if(verbose) {
    message("\nLoop over subcatchments to be extracted (this can take some time!) ...")
  }
  
  # function to read database from connection and store contents on disk
  store_db <- function(con, table, dir) {
    dat <- sqlFetch(con, table)
    file_tmp <- paste0(dir, "/", table, ".dat")
    write.table(dat, file=file_tmp, row.names=F, quote=F, sep="\t")
    return(file_tmp)
  }
  
  # apply function on temporary directory over tables of dbname
  tmp_dir <- tempdir()
  tabs <- sqlTables(con)[,"TABLE_NAME"]
  files_save <- sapply(tabs, function(x) store_db(con, x, tmp_dir), simplify = F)
  
  # loop over sub_extract
  logdata <- foreach(i = seq(1, length(sub_extract)), .errorhandling='remove', .inorder=FALSE) %dopar% {
    extract_db(sub_extract[i], dbname_new[i], con_new[[i]], files_save)
  }
  
  odbcCloseAll()
  message("\nFinished!")
  
} # EOF main


# internal function for database extraction
extract_db <- function(sub_extract, dbname_new, con, files_db) {
  
  # read in data needed herein
  dat_sub <- read.table(files_db[["subbasins"]], header=T, sep="\t")
  dat_c_lu <- read.table(files_db[["r_subbas_contains_lu"]], header=T, sep="\t")
  
  # fill in temporarily saved tables into database
  files_write <- grep("db_version|meta_info|subbasins|r_subbas_contains_lu", names(files_db), invert = T, value = T)
  lapply(files_write, function(x) writedb(con, files_db[[x]], x, overwrite=T, verbose=F))
  
  # extract relevant subbasins (all upstream of sub_extract[i])
  curr_ids <- sub_extract
  sub_upstr <- curr_ids
  while(length(curr_ids) > 0) {
    curr_ids <- dat_sub$pid[which(dat_sub$drains_to %in% curr_ids)]
    sub_upstr <- c(sub_upstr, curr_ids)
  }
  sub_rows <- which(dat_sub$pid %in% sub_upstr)
  dat_sub_up <- dat_sub[sub_rows,]
  dat_sub_up[which(dat_sub_up$pid == sub_extract), "drains_to"] <- 9999
  dat_sub_up$a_stream_order <- NA
  
  # write data into subbasins table
  sqlSave(con, dat_sub_up, "subbasins", verbose=F, append=TRUE , test = FALSE,
          nastring = NULL, fast = TRUE, rownames = FALSE)
  
  # delete from r_subbas_contains_lu table
  rows_rm <- which(!(dat_c_lu$subbas_id %in% dat_sub_up$pid))
  sqlSave(con, dat_c_lu[-rows_rm,], "r_subbas_contains_lu", verbose=F, append=TRUE , test = FALSE,
          nastring = NULL, fast = TRUE, rownames = FALSE)
  
  # apply db_check() to crop the other (really relevant) tables
  junk <- capture.output(db_check(dbname_new, check = "delete_obsolete", fix = T, verbose = F))
  
  # re-calculate subbasin order
  junk <- capture.output(db_check(dbname_new, check = "subbasin_order", fix = T, verbose = F))
  
  # update table meta_info
  meta_dat <- sqlFetch(con, "meta_info")
  meta_dat <- meta_dat[-c(nrow(meta_dat)-1, nrow(meta_dat)),] # exclude last two db_check()s
  if(any(meta_dat$pid)) {
    pid_new <- max(meta_dat$pid) +1
  } else {
    pid_new <- 1
  }
  meta_out <- data.frame(pid=pid_new,
                         mod_date=as.POSIXct(Sys.time()),
                         mod_user=paste0("db_extract_subcatch(), v. ", installed.packages()["lumpR","Version"]),
                         affected_tables="subbasins, landscape_units, terrain_components, r_subbas_contains_lu, r_lu_contains_tc, r_tc_contains_svc",
                         affected_columns="various",
                         remarks=paste0("Database copied from database ", dbname, ", extracted sub-catchment of subbas no. ", sub_extract))
  meta_out <- rbind(meta_dat, meta_out)
  sqlQuery(con, "delete from meta_info")
  write_datetabs(con, meta_out, tab="meta_info", verbose = F)
} # EOF extract_db
