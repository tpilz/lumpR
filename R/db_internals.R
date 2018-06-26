# lumpR/db_internals.R
# Copyright (C) 2015, 2017 Tobias Pilz, Till Francke
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


# interal function for db_* functions

# adjust SQL dialects
sql_dialect <- function(con, statement) {
  # SQLite
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T)) {
    # remove comments
    statement <- gsub("COMMENT.'[^']*'", "",statement )
    # remove engine specification
    statement <- gsub("ENGINE.*", "",statement )
    # AUTO_INCREMENT is not supported
    statementa <- gsub("INT\\(11\\) AUTO_INCREMENT NOT NULL", "INTEGER PRIMARY KEY",statement, ignore.case = T)
    if(statementa != statement){
      statement <- gsub(", *PRIMARY KEY *\\([^)]*\\)","",statementa)
    }
    # remove ticks
    statement <- gsub("`", "", statement)
    # close with ';' otherwise an error occurs (at least under Linux in my case)
    if(substr(statement, nchar(statement), nchar(statement)) != ";")
      statement <- paste0(statement,";")
    
    # no 'modify' supported -> workaround
    if(grepl("modify", statement, ignore.case = TRUE)) {
      # get table to be modified
      statement <- gsub(";", "", statement)
      split <- unlist(strsplit(statement, "[ ]+"))
      r_tbl_mod <- grep("modify", split, ignore.case=T)
      tbl_mod <- split[r_tbl_mod-1]
      # fetch data from table to be modified
      dat_tbl_mod <- sqlFetch(con, tbl_mod)
      meta_tbl_mod <- sqlColumns(con, tbl_mod)
      # drop table from db
      sqlQuery(con, paste0("delete from ", tbl_mod, ";"))
      sqlQuery(con, paste0("drop table ", tbl_mod, ";"))
      # modify data type of data
      r_col_mod <- grep(split[r_tbl_mod+1], colnames(dat_tbl_mod), ignore.case=T)
      varspec <- meta_tbl_mod$TYPE_NAME
      names(varspec) <- meta_tbl_mod$COLUMN_NAME
      varspec[r_col_mod] <- split[r_tbl_mod+2]
      # re-create table and write modified data to database
      # if dat_tbl_mod has no values sqlSave throws an error but table is created successfully -> suppress error in that case; could not find a more elegant workaround
      if(nrow(dat_tbl_mod) == 0) {
        try(sqlSave(channel=con, tablename=tbl_mod, dat=dat_tbl_mod, varTypes=varspec, append=FALSE, 
                    nastring = NULL, fast = TRUE, rownames = FALSE),
            silent=T)
      } else {
        suppressWarnings(sqlSave(channel=con, tablename=tbl_mod, dat=dat_tbl_mod, varTypes=varspec, append=FALSE, 
                nastring = NULL, fast = TRUE, rownames = FALSE))
      }
      
      # in case of columns of type datetime (see function write_datetabs() below)
      if(any(grepl("datetime", meta_tbl_mod$TYPE_NAME, ignore.case = T))) {
        sqlQuery(con, paste0("delete from ", tbl_mod, ";"))
        write_datetabs(con, dat=dat_tbl_mod, tab=tbl_mod, verbose=F)
      }

      
      # return NULL as statement
      statement <- NULL
    }
  }
  
  # MS Access
  if(grepl("access", odbcGetInfo(con)["DBMS_Name"], ignore.case=T)) {
    # if 'alter table' statement -> 'alter column' instead of 'modify'
    if(grepl("modify", statement, ignore.case = T))
      statement <- gsub("modify", "alter column", statement, ignore.case = T)
    # adjust column data type syntax
    statement <- gsub("INT\\([0-9]*\\)", "INT", statement)
    # nvarchar (i.e. unicode characters) are not supported -> convert to varchar
    statement <- gsub("NVARCHAR", "VARCHAR", statement)
    # auto increment syntax
    statement <- gsub("INT AUTO_INCREMENT", "AUTOINCREMENT", statement)
    # no comments supported
    statement <- gsub("COMMENT.'[^']*'", "",statement )
    # no default values supported
    statement <- gsub("DEFAULT 0", "",statement )
    # remove engine specification
    statement <- gsub("ENGINE.*", "",statement )
    # alter primary key statement
    statement <- gsub("PRIMARY KEY","CONSTRAINT pk PRIMARY KEY",statement)
    # BIT instead of BOOL to represent true/false data
    statement <- gsub("BOOL", "BIT", statement)
    # no tinyint
    statement <- gsub("TINYINT", "INT", statement)
    # maximum length of a data type is 255
    len <- suppressWarnings(as.integer(unlist(strsplit(statement, "\\(|\\)"))))
    len <- len[which(!is.na(len))]
    if(any(len)) {
      len_oversize <- len[which(len > 255)]
      if(any(len_oversize)) {
        statement <- gsub(paste(len_oversize, collapse="|"), "255", statement)
      }
    }
    
  }
  
  return(statement)
} # EOF sql_dialect





# write data from external file into parameter database
writedb <- function(con, file, table, overwrite, verbose) {
  if(verbose) message("%")
  if(verbose) message(paste0("% -> Writing data into table '", table, "' ..."))
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # read data
  dat <- read.table(file, header=T, sep="\t")
  
  # check structure
  cols <- sqlColumns(con, table)$COLUMN_NAME
  if (any(!(cols %in% colnames(dat)))) 
    stop(paste0("File '", file, "' does not contain the required columns (", paste(cols, collapse=", "), ")."))
  
  # remove unnecessary columns if available
  rm_cols <- which(!(colnames(dat) %in% cols))
  if (any(rm_cols))
    dat <- dat[-rm_cols]
  
  # delete existing values in table values
  if (overwrite)
    sqlQuery(con, paste0("delete from ", table))
  
  # write values into table; in case of an error write a more meaningful error message
  tryCatch(
{
  sqlSave(channel=con, tablename = table, dat=dat, verbose=F, 
          append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
}, error = function(e) {
  stop(paste0("An error occured when writing into table '", table, "'. ",
              "All values written until error occurence will be kept in the database! ",
              "There might be a problem with the input data structure (e.g. gaps), ",
              "duplicate entries or entries that already exist in the database table. ",
              "Error message of the writing function: ", e))
}
  )

} # EOF writedb





# write data.frame into tables having column of type datetime
# Reason: sqlSave() does not work for columns of type datetime for DBMS SQLite (recognized column size is 3 and dates are truncated to 3 characters)
write_datetabs <- function(con, dat, tab, verbose) {
  
  # loop over rows of data.frame
  for(i in 1:nrow(dat)) {
    # create statement from dat
    statement <- paste0("INSERT INTO ", tab, " VALUES (",
                        apply(dat[i,],1,function(x) paste0("'", x, "'", collapse=", ")),
                        ");")
  
    # adjust SQL dialect if necessary
    statement <- sql_dialect(con, statement)
  
    # apply statement
    res <- sqlQuery(con, statement, errors=F)
    if (res==-1)
    {  
      res <- sqlQuery(con, statement, errors=T)
      stop(paste0("Error in SQL query execution while writing into table '",tab,"': ", res))
    }  
  }
  
  if(verbose) message(paste0("% -> Updated table '",tab,"'."))
  
} # EOF write_datetabs





# check or fix that fractions sum up to 1
# returns: if fix: dat_tbl unchanged or with updated 'fraction'
#          if !fix: flawd IDs (IDs of first column where fraction does not sum up to one)
check_fix_fractions <- function(dat_tbl, fix, update_frac_impervious, verbose) {
  name_tbl <- attr(dat_tbl, "table")
  
  if(verbose) message("%")
  if(verbose) message(paste0("% -> Processing table '", name_tbl, "' ..."))
  
  # sum of 'fraction' should be 1 for every higher level class (rounding error allowed)
  dat_contains_sum <- round(tapply(dat_tbl$fraction, list(parent=dat_tbl[[1]]), sum, na.rm=T), 2)
  
  # re-calculate areal fractions if needed
  dat_tbl_new <- dat_tbl
  if(!any(dat_contains_sum!=1)) {
    if (verbose) message("%   -> Everything sums up to one")
  } else {
    if (verbose)
    {  
      message(paste0("%   -> There are ", length(which(dat_contains_sum!=1)), " elements not summing to 1 in their fractions"))
      if (!fix) 
        message(paste0("%   -> Check table '", name_tbl, "'", ifelse(name_tbl=="r_tc_contains_svc" & !update_frac_impervious, " and 'terrain_components' (column frac_rocky)","")," or call db_check(..., check=\"check_fix_fractions\", fix=TRUE)!"))
    }

    if(fix) {
      
      if(verbose)
        message("%   -> Re-calculate fractions ...")
      dat_contains_sum <- tapply(dat_tbl$fraction, list(parent=dat_tbl[[1]]), sum, na.rm=T)
      for (s in 1:nrow(dat_tbl))
        dat_tbl_new$fraction[s] <- dat_tbl$fraction[s] / dat_contains_sum[paste0(dat_tbl[[1]][s])]
      
      attr(dat_tbl_new, "altered") <- TRUE
    } # if fix
  } # if any(dat_contains_sum!=1)
  
  if(verbose) message("% -> OK")
   
  if(fix)
    return(dat_tbl_new)
  else
    return(as.numeric(names(dat_contains_sum[which(dat_contains_sum!=1)])))
  
} # EOF check_fix_fractions





# filter disaggregated areas by areal fraction threshold
filter_small_areas <- function(dat_tbl, thres, fix, verbose) {
  if(verbose) message("%")
  if(verbose) message(paste0("% -> Processing table '", attr(dat_tbl, "table"), "' ..."))

  # sum of 'fraction' should be 1 for every higher level class (rounding error allowed)
  dat_contains_sum <- round(tapply(dat_tbl$fraction, list(parent=dat_tbl[[1]]), sum, na.rm=T), 2)
  if(any(dat_contains_sum != 1)) {
    if(fix)
      stop(paste0("Before removal of tiny areas: sum of fractions per higher level unit not always equal to one. Check table '", attr(dat_tbl, "table"), "'", ifelse(attr(dat_tbl, "table")=="r_tc_contains_svc", " and 'terrain_components' (column frac_rocky)","")," or call db_check(..., check=\"check_fix_fractions\", fix=TRUE)!"))
    else
      warning(paste0("Before removal of tiny areas: sum of fractions per higher level unit not always equal to one. Check table '", attr(dat_tbl, "table"), "'", ifelse(attr(dat_tbl, "table")=="r_tc_contains_svc", " and 'terrain_components' (column frac_rocky)","")," or call db_check(..., check=\"check_fix_fractions\", fix=TRUE)!"))
  }
  
  # remove datasets where fraction < area_thresh
  rows_rm <- which(dat_tbl$fraction < thres & dat_tbl[,2]!=-1) #find entities below threshold, omit special case "rocky fractions"
  
  dat_tbl_new <- dat_tbl
  if(!any(rows_rm)) {
    message(paste0("%   -> No fraction smaller ", thres, " could be found."))
  } else {
    
    if(fix)
      message(paste0("%   -> There are ", length(rows_rm), " datasets going to be removed from '", attr(dat_tbl, "table"), "'"))
    else
      message(paste0("%   -> There are ", length(rows_rm), " datasets containing fractions < threshold in '", attr(dat_tbl, "table"), "'"))
    
    # keep datasets where entities of more than 10% of the respective parent class' area would be removed
    lu_rm_sum <- tapply(dat_tbl$fraction[rows_rm], list(parent=dat_tbl[[1]][rows_rm]), sum)
    if(any(lu_rm_sum > 0.1)) {
      keep_lu <- which(lu_rm_sum > 0.1)
      message(paste0("%   -> For '", colnames(dat_tbl)[1], "' ", paste(names(lu_rm_sum)[keep_lu], collapse=", "),
                   " more than 10% of the area would be removed due to too many small '", colnames(dat_tbl)[2], "'. These datasets will be kept."))
      
      rows_rm_keep <- which(dat_tbl[[1]][rows_rm] %in% names(lu_rm_sum)[keep_lu])
      rows_rm <- rows_rm[-rows_rm_keep]
      if (!any(rows_rm))
        message(paste0("%   -> For '", attr(dat_tbl, "table"), "' nothing to remove or choose smaller value for 'area_thresh' and re-run check."))
    }
  
    # remove datasets and re-calculate areal fractions
    if(fix & any(rows_rm)) 
    {
      dat_tbl_new <- dat_tbl[-rows_rm,]
      dat_tbl_new <- check_fix_fractions(dat_tbl=dat_tbl_new, fix=TRUE, update_frac_impervious=update_frac_impervious, verbose=FALSE)
    }  

  } # if any fraction < area_thresh

  if(verbose) message("% -> OK.")
  return(dat_tbl_new)
} # EOF filter_small_areas





# helper function to connect to a database
connect_db <- function(dbname) {
  suppressWarnings(con <- odbcConnect(dbname, believeNRows=F))
  if (con == -1)
    stop(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC.",
                 " If you want to connect to a MS Access database make sure you are using 32 bit R."))
  # use PRAGMA for SQLite to enhance speed (at the cost of less data security, for details see https://www.tutorialspoint.com/sqlite/sqlite_pragma.htm)
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T)) {
    sqlQuery(con, "PRAGMA synchronous = OFF;")
    sqlQuery(con, "PRAGMA journal_mode = OFF;")
  }
  return(con)
} # EOF connect_db





# simple function to copy data from one database table into another (same table name)
# NOTE: ODBC::sqlCopy and ODBC::sqlCopyTable do not work properly
dbCopyTable <- function(con, tab, con_dest) {
  # read data
  dat <- sqlFetch(con, tab)
  
  # check if there is a DATETIME column and use internal function write_datetabs() if so
  types <- sqlColumns(con, tab)$TYPE_NAME
  if(any(grepl("datetime", types, ignore.case = T)))
    write_datetabs(con_dest, dat, tab, verbose = F)
  else {
    sqlQuery(con_dest, paste0("delete from ", tab))
    sqlSave(con_dest, dat, tab, verbose=F, append=TRUE , test = FALSE,
            nastring = NULL, fast = TRUE, rownames = FALSE)
  }
} # EOF dbCopyTable





# function to write into table meta_info
write_metainfo <- function(con, fun, affected_tbl, affected_col, remarks, verbose) {
  meta_dat <- sqlFetch(con, "meta_info")
  if(any(meta_dat$pid)) {
    pid_new <- max(meta_dat$pid) +1
  } else {
    pid_new <- 1
  }
  meta_out <- data.frame(pid=pid_new,
                         mod_date=as.POSIXct(Sys.time()),
                         mod_user=paste0(fun, ", v. ", installed.packages()["lumpR","Version"]),
                         affected_tables=paste(unique(affected_tbl), collapse=", "),
                         affected_columns=affected_col,
                         remarks=remarks)
  write_datetabs(con, meta_out, tab="meta_info", verbose)
} # EOF write_metainfo





# function to read in data from selected tables
read_db_dat <- function(tbl, con, tbl_exist, update_frac_impervious) {
  dat_out <- NULL
  tbl_read <- tbl[which(!(tbl %in% tbl_exist))]
  for(t in tbl_read) {
    dat_out[[t]] <- sqlFetch(con, t)
    # information about rocky fractions needed for r_tc_contains_svc, see ?db_check: remove_impervious_svc, option update_frac_impervious = FALSE
    if(t == "r_tc_contains_svc" & !update_frac_impervious) {
      res <- sqlQuery(con, "select pid as tc_id, -1 as svc_id, frac_rocky as fraction from terrain_components")
      # replace NA in rocky fraction by zero
      res$fraction[is.na(res$fraction)] <- 0
      dat_out[[t]] <- rbind(dat_out[[t]], res)
    }
    # meta information (attributes)
    attr(dat_out[[t]], "altered") <- FALSE
    attr(dat_out[[t]], "table") <- t
  }
  return(dat_out)
} # EOF read_db_dat





# function for writing changes into database
modify_db <- function(con, dat_tbl) {
  tbl_name <- attr(dat_tbl, "table")
  # define index columns for the verious tables (needed by function sqlUpdate())
  tbls_keys <- list(
    subbasins="pid",
    r_subbas_contains_lu=c("subbas_id", "lu_id"),
    landscape_units="pid",
    r_lu_contains_tc=c("lu_id", "tc_id"),
    terrain_components="pid",
    r_tc_contains_svc=c("tc_id", "svc_id"),
    soil_veg_components="pid",
    vegetation="pid",
    soils="pid",
    horizons="pid",
    particle_classes="class_id",
    r_soil_contains_particles=c("soil_id", "class_id"),
    rainy_season="pid"
  )
  
  # check if table has been modified, otherwise return an flag (-1)
  if (!attr(dat_tbl, "altered"))
    return(-1)
  
  # delete removed datasets (sqlUpdate() does not delete)
  key_t <- tbls_keys[[tbl_name]]
  
  del_query <- paste0("delete from ", tbl_name, " where not ", key_t[1], " in (",
                      paste(unique(dat_tbl[, key_t[1]]), collapse = ", "), ")")
  del_query <- sql_dialect(con, del_query)
  res <- sqlQuery(con, del_query, errors=F) # throws an "error" if nothing was deleted, so don't investigate further and hope everything is fine
  
  if(length(key_t) > 1) {
    for(k in 2:length(key_t)) {
      for(i in unique(dat_tbl[,k-1])) {
        dat_t <- dat_tbl[which(dat_tbl[,k-1]==i),]
        del_query <- paste0("delete from ", tbl_name, " where ", key_t[k-1], "=", i,
                            " and not ", key_t[k], " in (", paste(dat_t[, key_t[k]], collapse = ", "), ")")
        del_query <- sql_dialect(con, del_query)
        res <- sqlQuery(con, del_query, errors=F) # throws an "error" if nothing was deleted, so don't investigate further and hope everything is fine
      }
    }
  }
  
  # update db
  sqlUpdate(con, dat_tbl, tbl_name, tbls_keys[[tbl_name]])
  
  return(0)
} # EOF modify_db
