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
  if(verbose) {
    print("")
    print(paste0("Writing data into table '", table, "' ..."))
  }
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # read data
  dat <- read.table(file, header=T, sep="\t")
  
  # check structure
  cols <- sqlColumns(con, table)$COLUMN_NAME
  if (any(!(cols %in% colnames(dat))))
    stop(paste0("File '", file, "' does not contain the required columns (",
                paste(cols, collapse=", "), ")."))
  
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
  odbcClose(con)
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
    if (res==-1){
      odbcClose(con)
      stop(paste0("Error in SQL query execution while writing into table '",tab,"'."))
    }
  }
  
  if(verbose)
    print(paste0("Updated table '",tab,"'."))
  
} # EOF write_datetabs




# check or fix that fractions sum up to 1
check_fix_fractions <- function(con, table, fix, verbose, tbl_changed) {
  
  if(verbose)
    print(paste0("-> Processing table '", table, "' ..."))
  
  dat_contains <- sqlFetch(con, table)
  if (table=="r_tc_contains_svc") #for TCs also the rocky fraction needs to be considered
  {
    res=sqlQuery(con, "select pid as tc_id, -1 as svc_id, frac_rocky as fraction from terrain_components")
    dat_contains = rbind(dat_contains, res)
  }
  
  # sum of 'fraction' should be 1 for every higher level class (rounding error allowed)
  dat_contains_sum <- round(tapply(dat_contains$fraction, list(parent=dat_contains[[1]]), sum, na.rm=T), 2)
  
  flawedIDs = names(dat_contains_sum[dat_contains_sum!=1])
  if(length(flawedIDs)>0) {
      if (verbose)
      {  
        cat(paste0("The elements of the following entities do not sum to 1 in their fractions:\n", 
                 paste0(flawedIDs, collapse = "\n ")),"\n")
        if (!fix) 
          cat(paste0("Check table '", table, "'", ifelse(table=="r_tc_contains_svc", " and 'terrain_components' (column frac_rocky)","")," or call db_check(..., check=\"check_fix_fractions\", fix=TRUE)!\n"))
      }
  } 
  if (!fix) return(flawedIDs) #just report
  
  
  # re-calculate areal fractions
    if(fix && any(dat_contains_sum != 1)) {
      
      if(verbose)
        print("-> Re-calculate fractions ...")
      dat_contains_sum <- tapply(dat_contains$fraction, list(parent=dat_contains[[1]]), sum, na.rm=T)
      dat_contains_new <- dat_contains
      for (s in 1:nrow(dat_contains))
        dat_contains_new$fraction[s] <- dat_contains$fraction[s] / dat_contains_sum[paste0(dat_contains[[1]][s])]
      
      
      # write updated data into database
      if (table=="r_tc_contains_svc") #for TCs also the rocky fraction needs to be considered
      {
        rocky_frac=        dat_contains_new[dat_contains_new$svc_id==-1,] #extract information on rocky fraction - this needs to go into another table
        rocky_frac$svc_id = NULL #svc_id=-1 was just a marker for rocky fractions, not needed anymore
        dat_contains_new = dat_contains_new[dat_contains_new$svc_id!=-1,] #keep only real SVCs, discard rocky fractions that had been temporally inserted as SVCs
        
        terrain_components <- sqlFetch(con, "terrain_components")
        terrain_components = merge(terrain_components, rocky_frac, by.x="pid", by.y="tc_id")
        
        if (!identical(terrain_components$frac_rocky, terrain_components$fraction)) {
          terrain_components$frac_rocky = terrain_components$fraction #correct to adjusted rocky fraction
          terrain_components$fraction = NULL
          tryCatch(
            {
              sqlQuery(con, paste0("delete from terrain_components"))
              sqlSave(channel=con, tablename = "terrain_components", dat=terrain_components, verbose=F, 
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
                                     remarks=paste0("ATTENTION: Error while checking database using R package lumpR check check_fix_fractions. Nevertheless, affected_tables have already been changed."))
              write_datetabs(con, meta_out, tab="meta_info", verbose)
              
              stop(paste0("An error occured when updating table terrain_components. ",
                          "Error message of the writing function: ", e))
            }
          )
        }
      }
      
      tryCatch(
        {
          sqlQuery(con, paste0("delete from ", table))
          sqlSave(channel=con, tablename = table, dat=dat_contains_new, verbose=F, 
                  append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
          tbl_changed <- c(tbl_changed, table)
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
                                 remarks=paste0("ATTENTION: Error while checking database using R package lumpR check filter_small_areas. Nevertheless, affected_tables have already been changed."))
          write_datetabs(con, meta_out, tab="meta_info", verbose)
          stop(paste0("An error occured when updating table '", table, "'. ",
                      "Error message of the writing function: ", e))
        }
      )
      
    } # if fix
    
    
    if(verbose)
      print("-> OK.")
 
  
  return(tbl_changed)
} # EOF check_fix_fractions




# filter disaggregated areas by areal fraction threshold
filter_small_areas <- function(con, table, thres, fix, verbose, tbl_changed) {

  if(verbose)
    print(paste0("-> Processing table '", table, "' ..."))
  
  dat_contains <- sqlFetch(con, table)
  if (table=="r_tc_contains_svc") #for TCs also the rocky fraction needs to be considered
  {
    res=sqlQuery(con, "select pid as tc_id, -1 as svc_id, frac_rocky as fraction from terrain_components")
    dat_contains = rbind(dat_contains, res)
  }

  # sum of 'fraction' should be 1 for every higher level class (rounding error allowed)
  dat_contains_sum <- round(tapply(dat_contains$fraction, list(parent=dat_contains[[1]]), sum, na.rm=T), 2)
  if(any(dat_contains_sum != 1)) {
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
                             remarks=paste0("ATTENTION: Error while checking database using R package lumpR check filter_small_areas. Nevertheless, affected_tables have already been changed."))
      write_datetabs(con, meta_out, tab="meta_info", verbose)
      stop(paste0("Before removal of tiny areas: sum of fractions per higher level unit not always equal to one. Check table '", table, "'", ifelse(table=="r_tc_contains_svc", " and 'terrain_components' (column frac_rocky)",""),"!"))
    } else {
      print(paste0("-> ATTENTION: Before removal of tiny areas: sum of fractions per higher level unit not always equal to one. Check table '", table, "'", ifelse(table=="r_tc_contains_svc", " and 'terrain_components' (column frac_rocky)",""),"!"))
    }
  }
  
  # remove datasets where fraction < area_thresh
  rows_rm <- which(dat_contains$fraction < thres & dat_contains[,2]!=-1) #find entities below threshold, omit special case "rocky fractions"
  
  # further processing if any fraction < area_thresh only
  if(!any(rows_rm)) {
    print(paste0("-> In '", table, "' no fraction smaller ", thres, " could be found."))
  } else {
    
    if(fix) {
      print(paste0("-> The following datasets will be removed from '", table, "':"))
    } else {
      print(paste0("-> The following datasets contain fractions < threshold in '", table, "':"))
    }
    print(dat_contains[rows_rm,])
    
    # keep datasets where entities of more than 10% of the respective parent class' area would be removed
    lu_rm_sum <- tapply(dat_contains$fraction[rows_rm], list(parent=dat_contains[[1]][rows_rm]), sum)
    if(any(lu_rm_sum > 0.1)) {
      keep_lu <- which(lu_rm_sum > 0.1)
      print(paste0("-> For '", colnames(dat_contains)[1], "' ", paste(names(lu_rm_sum)[keep_lu], collapse=", "),
                   " more than 10% of the area would be removed due to too many small '", colnames(dat_contains)[2], "'. These datasets will be kept."))
      
      rows_rm_keep <- which(dat_contains[[1]][rows_rm] %in% names(lu_rm_sum)[keep_lu])
      rows_rm <- rows_rm[-rows_rm_keep]
      if (!any(rows_rm)) {
        print(paste0("-> For '", table, "' nothing to remove or choose smaller value for 'area_thresh' and re-run check."))
        return(NULL)
      }
        
    }
  
    # remove datasets and re-calculate areal fractions
    if(fix) 
    {
      dat_contains_rm <- dat_contains[-rows_rm,]
      tbl_changed = check_fix_fractions(con = con, table = table, fix = fix, verbose = verbose, tbl_changed = tbl_changed)
    }  


  if(verbose)
    print("-> OK.")
  } # if any fraction < area_thresh

  return(tbl_changed)
} # EOF filter_small_areas





# helper function to connect to a database
connect_db <- function(dbname) {
  suppressWarnings(con <- odbcConnect(dbname, believeNRows=F))
  if (con == -1)
    stop(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC.",
                 " If you want to connect to a MS Access database make sure you are using 32 bit R."))
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
