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
    # close with ';' otherwise an error occurs (at least under Linux in my case)
    statement <- paste0(statement,";")
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
    # BIT instead of BOOL to represent true/false data
    statement <- gsub("BOOL", "BIT", statement)
    # no tinyint
    statement <- gsub("TINYINT", "INT", statement)
  }
  
  return(statement)
} # EOF




# write data into parameter database
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
  sqlSave(channel=con, tablename = table, dat=dat, verbose=verbose, 
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

} # EOF
