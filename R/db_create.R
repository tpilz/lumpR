#' Create parameter database
#' 
#' Function creates tables in a pre-defined empty database to store parameters relevant
#' for modelling applications with the WASA model.
#'
#' @param dbname Name of the data source (DSN) registered at ODBC. See \code{Details}.
#' 
#' @details
#'  This package uses the ODBC interface to connect to a database. Creating the database
#'  includes the following steps which are OS dependent:
#'  
#'  \itemize{
#'    \item{Install \emph{ODBC} and a \emph{Database Management System} on your computer.
#'          Currently supported (tested) are: SQLite (v. 3.8.9), MariaDB/MySQL (v. 10.0.17), 
#'          MS Access.}
#'    \item{Register an empty database at your ODBC configuration.}
#'    \item{Install and load the \code{\link[RODBC]{RODBC}} package as interface to R.}
#'    \item{Call this function to create the tables in the database.}
#'    \item{Processing of the database using external software (or R packages)
#'          and/or use functions coming with LUMP.}
#'  }
#'  
#'  More information can be found at the LUMP package wiki: \url{https://github.com/tpilz/LUMP/wiki}
#' 
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'  
#' @export
#' 
db_create <- function(
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
  
  # read file with sql statements to create tables of the database
  sql_file <- system.file("database/create_db.sql", package="LUMP")
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
    
    # identify table name
    tablename <- gsub("CREATE TABLE *([[:alpha:]_]+).*","\\1",statement)
    tablename <- gsub("[[:space:]]*", "", tablename)
    
    # adjust to specific SQL dialects
    statement <- sql_dialect(con, statement)

    # create table in database if it does not yet exist
    if(!(tablename %in% sqlTables(con)$TABLE_NAME)) {
      res <- sqlQuery(con, statement, errors=F)
      if (res==-1){
        odbcClose(con)
        stop("Error in SQL query execution while creating db.")
      }
    } 
    
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
                         mod_user=paste0("db_create(), v. ", installed.packages()["LUMP","Version"]),
                         affected_tables="all",
                         affected_columns="all",
                         remarks=paste0("Created database version ", max(sqlFetch(con, "db_version")$version), " using R package LUMP."))
  write_datetabs(con, meta_out, tab="meta_info", verbose=F)
  
  # close connection
  odbcClose(con)
  
  
} # EOF
