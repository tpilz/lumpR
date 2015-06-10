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





# filter disaggregated areas by areal fraction threshold
filter_small_areas <- function(con, table, thres, verbose) {
  if(verbose)
    print(paste0("-> Processing table '", table, "' ..."))
  
  dat_contains <- sqlFetch(con, table)
  if (table=="r_tc_contains_svc") #for TCs also the rocky fraction needs to be considered
  {
    res=sqlQuery(con, "select pid as tc_id, -1 as svc_id, frac_rocky as fraction from terrain_components")
    dat_contains = rbind(dat_contains, res)
  }

  # sum of 'fraction' should be 1 for every higher level class (rounding error allowed)
  dat_contains_sum <- tapply(dat_contains$fraction, list(parent=dat_contains[[1]]), sum)
  if(any(dat_contains_sum > 1.05 | dat_contains_sum < 0.99))
    stop(paste0("Before removal of tiny areas: sum of fractions per higher level unit not always equal to one. Check table '", table, ifelse(table=="r_tc_contains_svc", " and terrain_components (column frac_rocky)",""),"'!"))
  
  # remove datasets where fraction < area_thresh
  rows_rm <- which(dat_contains$fraction < thres && dat_contains[,2]!=-1) #find entities below threshold, omit special case "rocky fractions"
  
  # further processing if any fraction < area_thresh only
  if(!any(rows_rm)) {
    print(paste0("-> In '", table, "' no fraction smaller ", thres, " could be found."))
  } else {
    
    print(paste0("-> The following datasets will be removed from '", table, "':"))
    print(dat_contains[rows_rm,])
    
    # keep datasets where entities of more than 10% of the respective parent class' area would be removed
    lu_rm_sum <- tapply(dat_contains$fraction[rows_rm], list(parent=dat_contains[[1]][rows_rm]), sum)
    if(any(lu_rm_sum > 0.1)) {
      keep_lu <- which(lu_rm_sum > 0.1)
      print(paste0("-> For '", colnames(dat_contains)[1], "' ", paste(names(lu_rm_sum)[keep_lu], collapse=", "),
                   " more than 10% of the area would be removed due to too many small '", colnames(dat_contains)[2], ". These datasets will be kept."))
      
      rows_rm_keep <- which(dat_contains[[1]][rows_rm] %in% names(lu_rm_sum)[keep_lu])
      rows_rm <- rows_rm[-rows_rm_keep]
      if (!any(rows_rm)) {
        print(paste0("-> For '", table, "' nothing to remove or choose smaller value for 'area_thresh' and re-run check."))
        return(NULL)
      }
        
    }
    
    # remove datasets
    dat_contains_rm <- dat_contains[-rows_rm,]
    
    # re-calculate areal fractions
    if(verbose)
      print("-> Re-calculate fraction ...")
    dat_contains_sum <- tapply(dat_contains_rm$fraction, list(parent=dat_contains_rm[[1]]), sum)
    dat_contains_new <- dat_contains_rm
    for (s in 1:nrow(dat_contains_rm))
      dat_contains_new$fraction[s] <- dat_contains_rm$fraction[s] / dat_contains_sum[paste0(dat_contains_rm[[1]][s])]
    
    
    # write updated data into database
    if (table=="r_tc_contains_svc") #for TCs also the rocky fraction needs to be considered
    {
      rocky_frac=        dat_contains_new[dat_contains_new$svc_id==-1,] #extract information on rocky fraction - this needs to go into another table
      rocky_frac$svc_id = NULL #was just a marker for rocky fractions, not needed anymore
      dat_contains_new = dat_contains_new[dat_contains_new$svc_id!=-1,] #keep only real SVCs, discard rocky fractions
    
      terrain_components <- sqlFetch(con, "terrain_components")
      terrain_components = merge(terrain_components, rocky_frac, by.x="pid", by.y="tc_id")
      if (!identical(terrain_components$frac_rocky, terrain_components$fraction))
      tryCatch(
      {
        sqlQuery(con, paste0("delete from terrain_components"))
        sqlSave(channel=con, tablename = "terrain_components", dat=terrain_components, verbose=verbose, 
                append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
      }, error = function(e) {
        odbcClose(con)
        stop(paste0("An error occured when updating table terrain_components. ",
                    "Error message of the writing function: ", e))
      }
            )
    }
    
    tryCatch(
    {
      sqlQuery(con, paste0("delete from ", table))
      sqlSave(channel=con, tablename = table, dat=dat_contains_new, verbose=verbose, 
              append=TRUE , test = FALSE, nastring = NULL, fast = TRUE, rownames = FALSE)
    }, error = function(e) {
      odbcClose(con)
      stop(paste0("An error occured when updating table '", table, "'. ",
                  "Error message of the writing function: ", e))
    }
    )


  if(verbose)
    print("-> OK.")
  } # if any fraction < area_thresh

  
} # EOF
