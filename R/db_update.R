# lumpR/db_update.R
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


#' Update parameter database
#' 
#' Function updates the parameter database to the specified (or latest version).
#' 
#' @param dbname Name of the data source (DSN) registered at ODBC.
#' @param to_ver Version number to update to (default: newest version available).
#' @param checkOnly only print current version number, no updating.
#' 
#' @details
#'  This function currently is only relevant to users who already have a parameter
#'   database from times before LUMP was an R package. In this case make sure you have
#'   version 18 of the database (Do manual updates first, see db_version.txt).
#'   Apply this function to update it to the desired version.
#'   
#'   It is planned to add further functionality to this function when the database
#'   structure is being further developed.
#'   
#'   Up to version 21 is relevant for the WASA model. Versions 22 to 24 contain exclusive
#'   adaptions for ECHSE's WASA engine only!
#'   
#' @references 
#'      lumpR package introduction with literature study and sensitivity analysis:\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#'   
#' @author 
#' Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'   

db_update <- function(
  dbname, to_ver=Inf, checkOnly=FALSE  
) {
  
  # connect to ODBC registered database
  con <- connect_db(dbname)
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="lumpR")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))
  
  if(is.infinite(to_ver))
    to_ver <- db_ver_max
  
  # tables in database
  tbls <- sqlTables(con)[,"TABLE_NAME"]
    
  # check current db version
  db_ver <- sqlFetch(con, "db_version")$version
  db_ver = max(db_ver)
  db_ver_init <- db_ver
  
  if (checkOnly)
    return(db_ver)
  
  if(to_ver > db_ver_max)
    stop(paste0("Requested update (", to_ver, ") is greater than newest available database version (", db_ver_max, ")!"))
  
  if(db_ver == db_ver_max)
    return(message(paste0("Database is up-to-date (version ", db_ver_max, "). Nothing to do.")))
  
  if(db_ver > to_ver)
    stop(paste0("Database (", db_ver, ") is newer than the requested update (", to_ver, "). Nothing to do."))
  
  if(db_ver < 18)
    stop("Database needs to be at least version 18 for updating. Do manual updates first (see db_version.txt in lumpR's source directory 'example/make_wasa_input/').")
  
  
  if(db_ver == 18) #ver 18 -> 19
  {  
    if (toupper(odbcGetInfo(con)["DBMS_Name"]) == "ACCESS") #do workaround for Access
    {
       warning("It seems like you are using an Access-Database. The columns 'landscape_units.length/slopelength', 'horizons.depth/thickness', and 'description' column of the following tables will change their position and lose their column description. 
               This is irrelevant, but if prefer you can fix this manually (apparently only in MS ACCESS).
               Affected tables: horizons, landscape_units, particle_classes, soils, soil_veg_components, subbasins, terrain_components, vegetation")
      
       affected_tables= c("horizons","landscape_units","particle_classes","soils","soil_veg_components","subbasins","terrain_components","vegetation")
       for (tab in affected_tables)
       {
         if (!(tab %in% tbls))
           stop(paste0("Table '", tabs, "' does not exist but is needed to update to version 19!"))
           
         statement = paste0("ALTER TABLE ", tab," add description VARCHAR(50);")
         res <- sqlQuery(con, statement, errors=TRUE)
         if (length(res)!=0) warning(res)
         
         if (tab %in% c("horizons","landscape_units","soil_veg_components","terrain_components"))
           col_name="descr" else
           col_name="desc"
         
         statement = paste0("UPDATE [", tab,"] set description=",col_name)
         res <- sqlQuery(con, statement, errors=TRUE)
         if (length(res)!=0) {warning(res); next}
         
         statement = paste0("ALTER TABLE ", tab," drop [", col_name,"]")
         res <- sqlQuery(con, statement, errors=TRUE)
         if (length(res)!=0) {warning(res); next}     
       }
       #rename "length" to "slopelength"
       statement = paste0("ALTER TABLE landscape_units add slopelength DOUBLE ;")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
       statement = paste0("UPDATE landscape_units set slopelength=length;")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
       statement = paste0("ALTER TABLE landscape_units drop [length]")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
       
       #rename "depth" to "thickness"
       statement = paste0("ALTER TABLE horizons add thickness DOUBLE ;")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
       statement = paste0("UPDATE horizons set thickness=depth;")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
       statement = paste0("ALTER TABLE horizons drop [depth]")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
       
       statement = paste0("INSERT INTO db_version VALUES (
       19,  19, 
      'First version within lumpR R-package', 
    	'none', 
    	'horizons, landscape_units, particle_classes, soils, soil_veg_components, subbasins, terrain_components, vegetation, db_version', 
    	'adjusted data type in db_version and column names for the other tables', 
    	'",strftime(Sys.time()),"');")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
    }  else
    {
      # read file with sql statements
      sql_file <- system.file("database/update_db_v19.sql", package="lumpR")
      script  <- readLines(sql_file)
      
      # identify individual queries of the script
      script <- gsub("--.*", "", script)
      script <- gsub("\t", "", script)
      script <- paste(script, collapse=" ")
      scriptparts <- strsplit(script, ";")[[1]]
      scriptparts <- scriptparts[-length(scriptparts)]
      
      # loop over queries
      for(i in seq(along=scriptparts)){
        
        statement <- scriptparts[i]
        
        # check if column name already has been updated (that would cause an error by sqlQuery())
        if(grepl("alter table", statement, ignore.case = TRUE)) {
          split <- strsplit(statement, "[ ]+")[[1]]
          pos <- grep("change|modify|alter", split, ignore.case = T)
          tbl <- split[tail(pos, n=1)-1]
          if (!(tbl %in% tbls))
            stop(paste0("Table '", tbl, "' does not exist but is needed to update to version 19!"))
          col_old <- split[pos+1]
          col_new <- split[pos+2]
          tbl_cols <- sqlColumns(con, tbl)$COLUMN_NAME
          if (!any(grepl(paste0("^", col_old, "$"), tbl_cols, ignore.case=T)) & 
               any(grepl(paste0("^", col_new, "$"), tbl_cols, ignore.case=T))) {
            warning(paste0("In table '", tbl, "' column '", col_old, "' has already been updated to '",
                           col_new, "'. Omitting that step."))
            next
          }
        }
        
        # adjust to specific SQL dialects
        statement <- sql_dialect(con, statement)
          
        
        # send query to database
        res <- sqlQuery(con, statement, errors=F)
        if (res==-1){
          tryCatch(odbcClose(con), error=function(e){})
          stop("Error in SQL query execution while updating db.")
        }
      }
    }
    db_ver=19
  }
  
  

  while(to_ver > db_ver) # other
  { 
    #fill in update steps
    # read file with sql statements
    sql_file <- system.file(paste0("database/update_db_v", db_ver+1, ".sql"), package="lumpR")
    script  <- readLines(sql_file)
    
    # identify individual queries of the script
    script <- gsub("--.*", "", script)
    script <- gsub("\t", "", script)
    script <- paste(script, collapse=" ")
    scriptparts <- strsplit(script, ";")[[1]]
    #scriptparts <- scriptparts[-length(scriptparts)]
    
    # loop over queries
    for(i in seq(along=scriptparts)){
      
      statement <- scriptparts[i]
      
      # adjust to specific SQL dialects
      statement <- sql_dialect(con, statement)
      
      if(is.null(statement))
        next
      
      # check if table to be created already exists for some reason
      if(grepl("create table", statement, ignore.case = TRUE)) {
        split <- strsplit(statement, "[ ]+")[[1]]
        pos <- grep("create", split, ignore.case = T)
        tbl <- split[pos+2]
        if(tbl %in% tbls) {
          stop(paste0("Table '", tbl, "' already exists when updating to version ", to_ver, ". Rename / delete manually, and repeat update."))
        }
      }
      
      # check if table to be altered does exist
      if(grepl("alter table", statement, ignore.case = TRUE)) {
        split <- strsplit(statement, "[ ]+")[[1]]
        pos <- grep("alter", split, ignore.case = T)
        tbl <- split[pos+2]
        if (!(tbl %in% tbls))
          stop(paste0("Table '", tbl, "' does not exist but is needed to update database to version ", to_ver, "!"))
      }
      
      # send query to database
      res <- sqlQuery(con, statement, errors=F)
      if (res==-1){
          res <- sqlQuery(con, statement, errors = T)
          tryCatch(odbcClose(con), error=function(e){})
          stop(cat(paste0("Error in SQL query execution while updating db.\nQuery: ", statement,
                          "\nerror-message: ", res[1])))
      }
    } # query loop
  
    
    db_ver <- db_ver +1
  }
  
  

  # update table meta_info
  meta_dat <- sqlFetch(con, "meta_info")
  if(any(meta_dat$pid)) {
    pid_new <- max(meta_dat$pid) +1
  } else {
    pid_new <- 1
  }
  meta_out <- data.frame(pid=pid_new,
                         mod_date=as.POSIXct(Sys.time()),
                         mod_user=paste0("db_update(), v. ", installed.packages()["lumpR","Version"]),
                         affected_tables="See lumpRs source database/update_db_v*.sql.",
                         affected_columns="See lumpRs source database/update_db_v*.sql.",
                         remarks=paste0("Database updated from version ", db_ver_init, " to version ", db_ver, "."))
  write_datetabs(con, meta_out, tab="meta_info", verbose=F)
  
  # close connection
  tryCatch(odbcClose(con), error=function(e){})
  
} # EOF
