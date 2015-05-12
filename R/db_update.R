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
    
    # send query to database
    res <- sqlQuery(con, statement, errors=F)
    if (res==-1){
      odbcClose(con)
      stop("Error in SQL query execution while creating db.")
    }
  }
  
  # close connection
  odbcClose(con)
  
} # EOF
