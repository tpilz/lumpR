#' Create parameter database
#' 
#' Function creates tables in a pre-defined empty database to store parameters relevant
#' for modelling applications with the WASA model.
#'
#' @param dbname Name of the database that should be created. See \code{Details}.
#' 
#' @details
#'  This package uses the ODBC interface to connect to a database. Creating the database
#'  includes the following steps which are OS dependent:
#'  
#'  \itemize{
#'    \item{Install \emph{ODBC} and a \emph{Database Management System} on your computer.
#'          Currently supported in this function is \emph{SQLite}.}
#'    \item{Register an empty database at your ODBC configuration.}
#'    \item{Install and load the \code{\link{RODBC}} package as interface to R.}
#'    \item{Call this function to create the tables in the database.}
#'    \item{Manual processing of the database and/or use functions coming with LUMP (TODO).}
#'  }
#' 
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}\cr
#'  Till Francke \email{francke@@uni-potsdam.de}
#'  
#' @export
#' 
create_db <- function(
  dbname
) {
  
  # load ODBC R interface
  library(RODBC)
  
  # connect to ODBC registered database
  suppressWarnings(con <- odbcConnect(dbname))
  
  if (con == -1)
    print(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC."))
  
  # read file with sql statements to create tables of the database
  sql_file <- system.file("create_db.sql", package="LUMP")
  script  <- readLines(sql_file)
  
  # identify individual queries of the script
  script <- gsub("--.*", "", script)
  script <- gsub("\t", "", script)
  script <- paste(script, collapse=" ")
  scriptparts <- strsplit(script, ";")[[1]]
  
  # loop over queries
  for(i in seq(along=scriptparts)){
    
    # remove comments
    statement <- gsub("COMMENT.'[^']*'", "",scriptparts[i] )
    
    # identify table name
    tablename <- gsub("CREATE TABLE *([[:alpha:]_]+).*","\\1",statement)
    tablename <- gsub("[[:space:]]*", "", tablename)
    
    # adjust to specific SQL dialects
    # SQLite
    if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T)) {
      statement <- gsub("ENGINE.*", "",statement )
      statementa <- gsub("INT\\(11\\) AUTO_INCREMENT NOT NULL", "INTEGER PRIMARY KEY",statement, ignore.case = T)
      if(statementa != statement){
        statement <- gsub(", *PRIMARY KEY *\\([^)]*\\)","",statementa)
      }
    }
    
    # create table in database if it does not yet exist
    if(!(tablename %in% sqlTables(con))) {
      sqlQuery(con, statement)
    } 
    
  }
  
  
} # EOF
