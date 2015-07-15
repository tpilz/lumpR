#'  Update parameter database
#'  
#'  Function updates the parameter database to the specified (or latest version).
#'  
#'  @param dbname Name of the data source (DSN) registered at ODBC.
#'  @param to_ver Version number to update to (default: newest version available).
#'  
#'  @details
#'    This function currently is only relevant to users who already have a parameter
#'    database from times before LUMP was an R package. In this case make sure you have
#'    version 18 of the database (Do manual updates first, see db_version.txt).
#'    Apply this function to update it to the desired version.
#'    
#'    It is planned to add further functionality to this function when the database
#'    structure is being further developed.
#'    
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'  
#' @export
#'    

db_update <- function(
  dbname, to_ver=Inf  
) {
  
  # load ODBC R interface
  require(RODBC)
  
  # connect to ODBC registered database
  suppressWarnings(con <- odbcConnect(dbname, believeNRows=F))
  
  if (con == -1)
    print(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC.",
                 " If you want to connect to a MS Access database make sure you are using 32 bit R."))
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="LUMP")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))
  
  if(is.infinite(to_ver))
    to_ver <- db_ver_max
    
  # check current db version
  db_ver <- sqlFetch(con, "db_version")$version
  db_ver = max(db_ver)
  db_ver_init <- db_ver
  
  if(to_ver > db_ver_max)
    stop(paste0("Requested update (", to_ver, ") is greater than newest available database version (", db_ver_max, ")!"))
  
  if(db_ver == db_ver_max)
    stop(paste0("Database is up to date (version ", db_ver_max, "). Nothing to do."))
  
  if(db_ver > to_ver)
    stop(paste0("Database (", db_ver, ") is newer than the requested update (", to_ver, "). Nothing to do."))
  
  if(db_ver < 18)
    stop("Database needs to be at least version 18 for updating. Do manual updates first (see db_version.txt in LUMP's source directory 'src/make_wasa_input/').")
  
  
  if(db_ver == 18) #ver 18 -> 19
  {  
    if (toupper(odbcGetInfo(con)["DBMS_Name"]) == "ACCESS") #do workaround for Access
    {
       warning("It seems like you are using an Access-Database. The 'decription' column of the following tables will change its position. 
               If you don't like this, you can fix this manually (apparently only in MS ACCESS), but don't have to.
               Affected tables: horizons, landscape_units, particle_classes, soils, soil_veg_components, subbasins, terrain_components, vegetation")
      
       affected_tables= c("horizons","landscape_units","particle_classes","soils","soil_veg_components","subbasins","terrain_components","vegetation")
       for (tab in affected_tables)
       {
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
       statement = paste0("INSERT INTO db_version VALUES (
       19,  19, 
      'First version within LUMP R-package', 
    	'none', 
    	'horizons, landscape_units, particle_classes, soils, soil_veg_components, subbasins, terrain_components, vegetation, db_version', 
    	'adjusted data type in db_version and column names for the other tables', 
    	'",strftime(Sys.time()),"');")
       res <- sqlQuery(con, statement, errors=TRUE)
       if (length(res)!=0) warning(res)
    }  else
    {
      # read file with sql statements
      sql_file <- system.file("database/update_db_v19.sql", package="LUMP")
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
          pos <- grep("change", split, ignore.case = T)
          tbl <- split[pos-1]
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
          odbcClose(con)
          stop("Error in SQL query execution while updating db.")
        }
      }
    }
    db_ver=19
  }
  
  

  if((db_ver == 19) & (to_ver > db_ver)) #ver 19->20
  { 
    #fill in update steps
    # read file with sql statements
    sql_file <- system.file("database/update_db_v20.sql", package="LUMP")
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
      
      # adjust to specific SQL dialects
      statement <- sql_dialect(con, statement)
      
      if(is.null(statement))
        next
      
      
      # send query to database
      res <- sqlQuery(con, statement, errors=F)
      if (res==-1){
        odbcClose(con)
        stop("Error in SQL query execution while updating db.")
      }
    }
  
    
    db_ver=20
  }
  
  if((db_ver == 20) & (to_ver > db_ver)) #ver 20->21
  { 
    #fill in update steps
  
    db_ver=21
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
                         mod_user=paste0("db_update(), v. ", installed.packages()["LUMP","Version"]),
                         affected_tables="See LUMPs source database/update_db_v*.sql.",
                         affected_columns="See LUMPs source database/update_db_v*.sql.",
                         remarks=paste0("Database updated from version ", db_ver_init, " to version ", db_ver, "."))
  write_meta(con, meta_out, verbose=F)
  
  # close connection
  odbcClose(con)
  
} # EOF
