#'  Update parameter database
#'  
#'  Function updates the parameter database to the latest version.
#'  
#'  @param dbname Name of the data source (DSN) registered at ODBC.
#'  
#'  @details
#'    This function currently is only relevant to users who already have a parameter
#'    database from times before LUMP was an R package. In this case make sure you have
#'    version 18 of the database (contact the authors if you have an earlier version).
#'    Apply this function to update it to the latest version 19.
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
  dbname  
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
  
  # check current db version
  db_ver <- sqlFetch(con, "db_version")$version
  if(max(db_ver) == 19)
    stop("Database is up to date (version 19). Nothing to do.")
  if(max(db_ver) < 18)
    stop("Database needs to be version 18 for updating. Please contact the authors.")
  
  
  
  # read file with sql statements
  sql_file <- system.file("update_db_v19.sql", package="LUMP")
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
  
  # close connection
  odbcClose(con)
  
} # EOF
