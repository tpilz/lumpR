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
#'          Currently supported (tested) are: SQLite (v. 3.8.9), MariaDB/MySQL (v. 10.0.17), 
#'          MS Access.}
#'    \item{Register an empty database at your ODBC configuration.}
#'    \item{Install and load the \code{\link{RODBC}} package as interface to R.}
#'    \item{Call this function to create the tables in the database.}
#'    \item{Processing of the database using external software (or R packages)
#'          and/or use functions coming with LUMP (TODO).}
#'  }
#'  
#'  More information can be found at the LUMP package wiki: \url{https://github.com/tpilz/LUMP/wiki}
#' 
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'  
#' @export
#' 
create_db <- function(
  dbname
) {
  
  # load ODBC R interface
  require(RODBC)
  
  # connect to ODBC registered database
  suppressWarnings(con <- odbcConnect(dbname, believeNRows=F))
  
  if (con == -1)
    print(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC.",
                 " If you want to connect to a MS Access database make sure you are using 32 bit R."))
  
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
    
    statement <- scriptparts[i]
    
    # identify table name
    tablename <- gsub("CREATE TABLE *([[:alpha:]_]+).*","\\1",statement)
    tablename <- gsub("[[:space:]]*", "", tablename)
    
    # adjust to specific SQL dialects
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
    }
    
    # MS Access
    if(grepl("access", odbcGetInfo(con)["DBMS_Name"], ignore.case=T)) {
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
    }
    
    
    # create table in database if it does not yet exist
    if(!(tablename %in% sqlTables(con)$TABLE_NAME)) {
      sqlQuery(con, statement)
    } 
    
  }
  
  # close connection
  odbcClose(con)
  
  
} # EOF
