# lumpR/db_prepare_musle.R
# Copyright (C) 2019 Till Francke
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


#' Prepare various input related to use of (M)USLE module (erosion)
#' 
#' Function to write parameter values relevant for modelling application with the
#' WASA-SED hydrological model into an existing database, preferably created with
#' \code{\link[lumpR]{db_create}}.
#' 
#' @param dbname Name of the data source (DSN) registered at ODBC. See \code{Details} of
#' \code{\link[lumpR]{db_create}}.
#' 
#' @param compute_K Compute (M)USLE K-factor (soil erodibility) from properties of topmost soil horizon (i.e. use texture of topmost soil layer), write it to table "soil" (use \code{copy_from_other_tables} to transfer it to table "soil_veg_components"). \code{c(TRUE, FALSE)} 
#' 
#' @param copy_from_other_tables Copy values to table "soil_veg_components": MUSLE-C and Manning-n from table "vegetation", MUSLE-K from "soils", coarse_frag from "horizons". Array of strings with possible values \code{c("musle-c", "manning-n" ,"musle-k" , "coarse_frac")} 
#' 
#' @param setP Set entire column of (M)USLE P-factor (protection measures) to a single value (usually 1). Default NULL (do not set).
#' @param verbose \code{logical}. Enable printing of information during execution. Default: \code{TRUE}.
#' 
#' @details 
#'  after Williams (1995). [explanation to be elaborated]
#'          
#' 
#' @references
#'      \bold{lumpR paper}\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#'      
#'     
#'      
#' 
#' @author 
#'  Till Francke \email{francke@@uni-potsdam.de}
#' 
db_prepare_musle <- function(
  dbname,
  compute_K=FALSE,
  copy_from_other_tables=NULL,
  setP=NULL,
  verbose=TRUE
) {

  
    
  #this is a quick-and-dirty conversion from legacy PHP-code. It could probably be solved in a couple of lines in R. Sorry for not having had the time, though.
  
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(verbose) message("% START db_prepare_musle()")
  if(verbose) message("%")
  if(verbose) message("% Connecting to and checking database ...")
  
  # connect to ODBC registered database
  con <- connect_db(dbname)
  
  #modify error handler to gracefully close ODBC-connection before aborting (otherwise, ODBC-handles are left open)
  org_error_handler = getOption("error") #original error handler
  
  closeODBC_and_stop = function()
  {  
    odbcCloseAll()
    options(error=org_error_handler) #restore original handler
  }
  options(error=closeODBC_and_stop)  #modify error handler
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  # check database (all 'tables' have to exist)
  tables=c("soils", "horizons", "soil_veg_components", "vegetation")
  check_tbl <- tables %in% sqlTables(con)$TABLE_NAME
  if (any(!check_tbl))
    stop(paste0("Table(s) ", paste(tables[which(!check_tbl)], sep=", "), " could not be found in database '", dbname, "'."))
 
  if(verbose) message("% OK")
  
  
  if (compute_K)
{
  #these are the particle size classes that are converted to (USDA-soil classification), used by Williams (1995)
  clay_upper_limit=0.002
  silt_upper_limit=0.05
  sand_upper_limit=2.0
  
  
  res = sqlQuery2(con,  "select * from r_soil_contains_particles", info="check <r_soil_contains_particles>")  
  if(nrow(res)==0)
    stop("Table <r_soil_contains_particles> is empty. Cannot compute K-factor.")
  
  res = sqlQuery2(con,  "select * from soils", info="check <soils>")  
  if(nrow(res)==0)
    stop("Table <soils> is empty. Cannot compute K-factor.")
  
  res = sqlQuery2(con,  "select * from soil_veg_components", info="check <soil_veg_components>")  
  if(nrow(res)==0)
    stop("Table <soil_veg_components> is empty. Cannot compute K-factor.")
  
  res = sqlQuery2(con,  "select * from particle_classes", info="check <particle_classes>")  
  if(nrow(res)==0)
    stop("Table <particle_classes> is empty. Cannot compute K-factor.")
  
  
  #### compute clay fraction according to USDA-----------------------------------------------------
  statement =  paste0("select class_id, upper_limit from particle_classes where upper_limit>=",clay_upper_limit, " order by upper_limit")
  #statement =  paste0("select * from particle_classes;")
  
  #get ID of class that is at or just over usda clay
  res = sqlQuery2(con,  statement, info="get class-ID")  
  if(nrow(res)==0)
    stop("In table <particle_classes> all specified particle classes are finer than USDA-clay. Cannot compute K-factor.")
  
  class_above_usda_clay       = res[1, "class_id"   ]		#get the id of the user-defined class that sits just above USDA-clay
  class_above_usda_clay_limit = res[1, "upper_limit"]	#get uppper limit of respective class
  
  
  statement =  "DROP TABLE t_cum_above"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  
  #generate command according to SQL-engine
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T)) 
  {  #MariaDB
    statement =  paste0("create TABLE t_cum_above (soil_id INT NOT NULL, a_cum_above real) SELECT soil_id, sum(fraction) AS a_cum_above FROM r_soil_contains_particles WHERE class_id<=", class_above_usda_clay, " GROUP BY soil_id")
  }  else
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
  { #SQLite
    statement = "CREATE TABLE t_cum_above (soil_id INTEGER, a_cum_above REAL);"
    res = sqlQuery2(con, statement, info="create temporary table <t_cum_above>")  	#produce a table containing the cumulative fractions up to class_above_usda_clay for each soil
    
    statement = "INSERT INTO t_cum_above (soil_id, a_cum_above) SELECT soil_id, SUM(fraction) AS a_cum_above
                 FROM r_soil_contains_particles WHERE class_id <= 1 GROUP BY soil_id;"
  } else
  { #all other DBs  
    statement =  paste0("SELECT soil_id, sum(fraction) AS a_cum_above INTO t_cum_above FROM r_soil_contains_particles WHERE class_id<=", class_above_usda_clay, " GROUP BY soil_id;")  
  }  
  
  res = sqlQuery2(con, statement, info="create and fill temporary table <t_cum_above>")  	#produce a table containing the cumulative fractions up to class_above_usda_clay for each soil
  
  statement =  "DROP TABLE t_cum_below"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  class_below_usda_clay=class_above_usda_clay-1		#the class below USDA clay
  if (class_below_usda_clay==0)	#no lower classes that can be used as a point for interpolation
  {
    class_below_usda_clay_limit=0 #interpolation starts at 0
    #statement that produces a table containing zeros for each soil
    #generate command according to SQL-engine
    if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    {  
      #MariaDB
      statement="create TABLE t_cum_below (soil_id INT NOT NULL, a_cum_below REAL) SELECT soil_id, 0 AS a_cum_below FROM r_soil_contains_particles GROUP BY soil_id"		      
    } else
    if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
    { #SQLite
      statement = "CREATE TABLE t_cum_below (soil_id INTEGER, a_cum_below REAL);"
      res = sqlQuery2(con, statement, info="create temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions for each soil
      statement = "INSERT INTO t_cum_below (soil_id, a_cum_below) SELECT soil_id, 0 AS a_cum_below FROM r_soil_contains_particles GROUP BY soil_id;"
    } else
    { #all other DBs  
      statement="SELECT soil_id, 0 AS a_cum_below INTO t_cum_below FROM r_soil_contains_particles GROUP BY soil_id"		
    }
  } else	#use the nearest lower class as a point for interpolation
  {
    statement =  paste0("select upper_limit from particle_classes where class_id<=",class_below_usda_clay)
    res = sqlQuery2(con, statement, info="get class limit")  
    if (nrow(res) == 0)
      stop("USER-Class below USDA-clay not found.")
    class_below_usda_clay_limit=res[1,"upper_limit"]	#get upper limit of respective class
    
    if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    {  
      statement=paste0("create TABLE t_cum_below (soil_id INT NOT NULL, a_cum_below real) SELECT soil_id, sum(fraction) AS a_cum_below ",
                       " FROM r_soil_contains_particles WHERE class_id<=",class_below_usda_clay, " GROUP BY soil_id")
    }
    else
    if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
    {
      statement = "CREATE TABLE t_cum_below (soil_id INTEGER, a_cum_below REAL);"
      res = sqlQuery2(con, statement, info="create temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions for each soil
      statement = paste0("INSERT INTO t_cum_below (soil_id, a_cum_below) SELECT soil_id, sum(fraction) AS a_cum_below FROM r_soil_contains_particles WHERE class_id<=",class_below_usda_clay, " GROUP BY soil_id")
    }
    else
    { #all other DBs  
      statement=paste0("SELECT soil_id, sum(fraction) AS a_cum_below  INTO t_cum_below",
      " FROM r_soil_contains_particles WHERE class_id<=",class_below_usda_clay, 
      " GROUP BY soil_id")		#statement-statement that produces a table containing the cumulative fractions up to class_below_usda_clay for each soil
    }
  }
  
  res = sqlQuery2(con, statement, info="create and fill temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions up to class_below_usda_clay for each soil
  
  statement =  "select count(*) from soils"
  res = sqlQuery2(con, statement, info="checking entries in <soil>")  
  if(res[1,1]==0)
    stop("No rows found in table <soils>.")
  
  statement =  "UPDATE soils SET a_clay=0,a_silt=0,a_sand=0"
  res = sqlQuery2(con, statement, info="initialise USDA-fractions columns in <soil>")  
  
  #browser()
  #con <- connect_db(dbname)
  
  print("computing USDA-clay-content...")
  
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
  { #SQLite
    statement =  paste0("UPDATE soils
    SET a_clay = (
      (
        (SELECT a_cum_above FROM t_cum_above WHERE t_cum_above.soil_id = soils.pid) -
          (SELECT a_cum_below FROM t_cum_below WHERE t_cum_below.soil_id = soils.pid)
      ) * ",clay_upper_limit-class_below_usda_clay_limit,") / 0.002;")
  }   else #all other DBs  
  statement =  paste0("UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id",
                      " SET a_clay = ((a_cum_above-a_cum_below)*(",clay_upper_limit-class_below_usda_clay_limit,"))/",class_above_usda_clay_limit-class_below_usda_clay_limit,";")
  
  res = sqlQuery2(con, statement, info="compute USDA-clay content")  
  
  print("OK")
  
  
  
  ####compute silt fraction according to USDA-----------------------------------------------------
  statement =  paste0("select class_id, upper_limit from particle_classes where upper_limit>=", silt_upper_limit," order by upper_limit")
  #get ID of class that is at or just over usda silt
  res = sqlQuery2(con, statement, info="ID of class that is at or just over usda silt")  
  
  if (nrow(res) == 0)		
    stop("All specified particle classes are finer than USDA-silt, cannot compute K-factor.")
  
  class_above_usda_silt      =res[1,"class_id"]		#get the user-class that sits just above USDA-silt
  class_above_usda_silt_limit=res[1,"upper_limit"]	#get uppper limit of respective class
  
  statement =  "DROP TABLE t_cum_above"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
  #MariaDB  
  {  
    statement =  paste0("create TABLE t_cum_above (soil_id INT NOT NULL, a_cum_above real) SELECT soil_id, sum(fraction) AS a_cum_above ",
                        " FROM r_soil_contains_particles WHERE class_id<=",class_above_usda_silt," GROUP BY soil_id")
  }  else 
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
  { #SQLite
    statement = "CREATE TABLE t_cum_above (soil_id INTEGER, a_cum_above REAL);"
    res = sqlQuery2(con, statement, info="create temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions for each soil
    statement = paste0("INSERT INTO t_cum_above (soil_id, a_cum_above) SELECT soil_id, sum(fraction) AS a_cum_above ",
    " FROM r_soil_contains_particles WHERE class_id<=",class_above_usda_silt," GROUP BY soil_id")
  } else
  { #all other DBs    
    statement =  paste0("SELECT soil_id, sum(fraction) AS a_cum_above INTO t_cum_above",
                        " FROM r_soil_contains_particles WHERE class_id<=",class_above_usda_silt," GROUP BY soil_id")
  }
  res = sqlQuery2(con, statement, info="t_cum_above for silt")  	#produce a table containing the cumulative fractions up to class_above_usda_silt for each soil
  
  statement =  "DROP TABLE t_cum_below"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  class_below_usda_silt=class_above_usda_silt-1		#the class below USDA silt
  if (class_below_usda_silt==0)	#no lower classes that can be used as a point for interpolation
  {
    #statement-statement that produces a table containing zeros for each soil  
    class_below_usda_silt_limit=0 #interpolation starts at 0
    if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    #MariaDB
    {
      statement=paste0("create TABLE t_cum_below (soil_id INT NOT NULL, a_cum_below real) SELECT soil_id, 0 AS a_cum_below ",
                         " FROM r_soil_contains_particles GROUP BY soil_id")
    } else	
    if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
    { #SQLite
      statement = "CREATE TABLE t_cum_below (soil_id INTEGER, a_cum_below REAL);"
      res = sqlQuery2(con, statement, info="create temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions for each soil
      statement = paste0("INSERT INTO t_cum_below (soil_id, a_cum_below) SELECT soil_id, 0 AS a_cum_below ",
      " FROM r_soil_contains_particles GROUP BY soil_id")
    } else  
    {  
        #all other DBs      
      statement=paste0("SELECT soil_id, 0 AS a_cum_below INTO t_cum_below",
      " FROM r_soil_contains_particles GROUP BY soil_id")	
    }
  } else	#use the nearest lower class as a point for interpolation
  {
    statement =  paste0("select upper_limit from particle_classes where class_id=",class_below_usda_silt) #?
    res = sqlQuery2(con, statement, info="USER-Class below USDA-silt not found.")  
    
    #statement-statement that produces a table containing the cumulative fractions up to class_below_usda_silt for each soil
    class_below_usda_silt_limit=res[1,"upper_limit"]	#get uppper limit of respective class
    
    if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    {  
      statement=paste0("create TABLE t_cum_below (soil_id INT NOT NULL, a_cum_below real) SELECT soil_id, sum(fraction) AS a_cum_below ",
      " FROM r_soil_contains_particles WHERE class_id<=",class_below_usda_silt, " GROUP BY soil_id")    
    }  else
    if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
    { #SQLite
      statement = "CREATE TABLE t_cum_below (soil_id INTEGER, a_cum_below REAL);"
      res = sqlQuery2(con, statement, info="create temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions for each soil
      statement = paste0("INSERT INTO t_cum_below (soil_id, a_cum_below) SELECT soil_id, sum(fraction) AS a_cum_below ",
      " FROM r_soil_contains_particles WHERE class_id<=",class_below_usda_silt, " GROUP BY soil_id") 
    } else 
    statement=paste0("SELECT soil_id, sum(fraction) AS a_cum_below  INTO t_cum_below",
    " FROM r_soil_contains_particles WHERE class_id<=",class_below_usda_silt, " GROUP BY soil_id")		
  }
  
  res = sqlQuery2(con, statement, info="cumulative fractions up to class_below_usda_silt")  	#produce a table containing the  for each soil
  
  
  #   print("computing USDA-silt-content...")
  # statement =  "UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id".
  # " SET a_silt = ((a_cum_above-a_cum_below)*(silt_upper_limit-class_below_usda_silt_limit))/(class_above_usda_silt_limit-class_below_usda_silt_limit)"
  # res = sqlQuery2(con, statement, info="")  
  # if(!res)
  #   die("Could not compute USDA-silt content (sql_err_msg).")
  # 
  
    print("OK")
  
  
  
  #### compute sand fraction according to USDA ####
  statement =  paste0("select class_id, upper_limit from particle_classes where upper_limit>=",sand_upper_limit," order by upper_limit")
  #get ID of class that is at or just over usda sand
  res = sqlQuery2(con, statement, info="")  
  if (nrow(res) ==0)		
    stop("All specified particle classes are finer than USDA-sand, cannot compute K-factor.")
  
  class_above_usda_sand      =res[1, "class_id"]		#get the user-class that sits just above USDA-sand
  class_above_usda_sand_limit=res[1, "upper_limit"]	#get uppper limit of respective class
  
  statement =  "DROP TABLE t_cum_above"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
  {  
    statement =  paste0("create TABLE t_cum_above (soil_id INT NOT NULL, a_cum_above real) SELECT soil_id, sum(fraction) AS a_cum_above ",
                        " FROM r_soil_contains_particles WHERE class_id<=", class_above_usda_sand," GROUP BY soil_id")
  }  else
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
  { #SQLite
    statement = "CREATE TABLE t_cum_above (soil_id INTEGER, a_cum_above REAL);"
    res = sqlQuery2(con, statement, info="create temporary table <t_cum_above>")  	#produce a table containing the cumulative fractions for each soil
    statement = paste0("INSERT INTO t_cum_above (soil_id, a_cum_above) SELECT soil_id, sum(fraction) AS a_cum_above ",
                       " FROM r_soil_contains_particles WHERE class_id<=", class_above_usda_sand," GROUP BY soil_id") 
  }  else
  statement =  paste0("SELECT soil_id, sum(fraction) AS a_cum_above INTO t_cum_above",
  " FROM r_soil_contains_particles WHERE class_id<=", class_above_usda_sand," GROUP BY soil_id")
  res = sqlQuery2(con, statement, info="cumulative fractions up to class_above_usda_sand")  	#produce a table containing the cumulative fractions up to class_above_usda_sand for each soil
  
  statement =  "DROP TABLE t_cum_below"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  class_below_usda_sand=class_above_usda_sand-1		#the class below USDA sand
  if (class_below_usda_sand==0)	#no lower classes that can be used as a point for interpolation
  {
    class_below_usda_sand_limit=0 #interpolation starts at 0
    statement=paste0("SELECT soil_id, 0 AS a_cum_below INTO t_cum_below",
    " FROM r_soil_contains_particles GROUP BY soil_id")		#statement-statement that produces a table containing zeros for each soil
  } else	#use the nearest lower class as a point for interpolation
  {
    statement =  paste0("select upper_limit from particle_classes where class_id<=", class_below_usda_sand)
    res = sqlQuery2(con, statement, info="compute sand")  
    if (nrow(res)==0)
      stop("USER-Class below USDA-sand not found.")
    #statement-statement that produces a table containing the cumulative fractions up to class_below_usda_sand for each soil
    class_below_usda_sand_limit=res[1, "upper_limit"]	#get uppper limit of respective class
    if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    {  
      statement =  paste0("create TABLE t_cum_below (soil_id INT NOT NULL, a_cum_below real) SELECT soil_id, sum(fraction) AS a_cum_below ",
    " FROM r_soil_contains_particles WHERE class_id<=", class_below_usda_sand, " GROUP BY soil_id")
    } else
    if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
    { #SQLite
      statement = "CREATE TABLE t_cum_below (soil_id INTEGER, a_cum_below REAL);"
      res = sqlQuery2(con, statement, info="create temporary table <t_cum_below>")  	#produce a table containing the cumulative fractions for each soil
      statement = paste0("INSERT INTO t_cum_below (soil_id, a_cum_below) SELECT soil_id, sum(fraction) AS a_cum_below ",
                         " FROM r_soil_contains_particles WHERE class_id<=", class_below_usda_sand, " GROUP BY soil_id")
    } else   
    statement=paste0("SELECT soil_id, sum(fraction) AS a_cum_below  INTO t_cum_below",
    " FROM r_soil_contains_particles WHERE class_id<=", class_below_usda_sand, " GROUP BY soil_id")		
  }
  
  res = sqlQuery2(con, statement, info="fractions up to class_below_usda_sand")  	#produce a table containing the cumulative fractions up to class_below_usda_sand for each soil
  
  print("computing USDA-sand-content...")
  if(grepl("SQLite", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))  
  { #SQLite
    statement =  paste0("UPDATE soils
    SET a_sand = (
      (
        (SELECT a_cum_above FROM t_cum_above WHERE t_cum_above.soil_id = soils.pid) -
          (SELECT a_cum_below FROM t_cum_below WHERE t_cum_below.soil_id = soils.pid)
      ) * ",sand_upper_limit-class_below_usda_sand_limit,") / ",class_above_usda_sand_limit-class_below_usda_sand_limit, "")
  }   else 
  {
    statement =  paste0("UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id",
  " SET a_sand = ((a_cum_above-a_cum_below)*(",sand_upper_limit-class_below_usda_sand_limit, "))/",class_above_usda_sand_limit-class_below_usda_sand_limit,"")
  }
  res = sqlQuery2(con, statement, info="compute USDA-sand")  
  print("OK")
  
  statement = "UPDATE soils SET a_silt = 1 - a_sand - a_clay;"
  res = sqlQuery2(con, statement, info="set USDA-silt")  
  
  #### compute subfactors ####
  print("computing f_cl_si factor...")
  statement =  "UPDATE soils SET a_f_cl_si=1 WHERE a_silt=0"		#formula doesn't work for zero silt content - factor is set to 1
  
  res = sqlQuery(con, statement, errors = FALSE)  #raises an error, when no row affected, so suppress error message
  
  statement =  "UPDATE soils SET a_f_cl_si = exp(0.3*log(a_silt/(a_clay+a_silt))) WHERE a_silt>0"		
  res = sqlQuery(con, statement, errors=FALSE)  
  print("OK")
  
  
  print("computing f_csand factor...")
  #statement =  "UPDATE soils SET a_f_csand = (0.2+(0.3*exp(-0.256*a_sand*100*(1-a_silt))))"		#as cited in SWAT manual
   statement =  "UPDATE soils SET a_f_csand = (0.2+(0.3*exp(-0.0256*a_sand*100*(1-a_silt))))"		#as in Williams, 1995
  res = sqlQuery2(con, statement, info="computing f_csand factor")  
  print("OK")
  
  print("computing f_hisand factor...")
  statement =  "UPDATE soils SET a_f_hisand = (1-(0.7*(1-a_sand)/((1-a_sand)+exp(-5.51+22.9*(1-a_sand)))))"
  res = sqlQuery2(con, statement, info="f_hisand factor")  
  print("OK")
  
  print("computing f_orgc factor...")
  statement =  "UPDATE soils SET a_f_orgc = (1-(0.25*b_om/1.72*100/(b_om/1.72*100+exp(3.72-2.95*b_om/1.72*100))))"
  res = sqlQuery2(con, statement, info="computing f_orgc factor")  
  print("OK")
  
  
  #### compute K and insert results into table soil 
  print("computing MUSLE-K and inserting into <soils>...")
  statement =  "UPDATE soils SET a_musle_k = a_f_csand*a_f_cl_si*a_f_orgc*a_f_hisand"
  res = sqlQuery2(con, statement, info="computing MUSLE-K")  
  
  statement =  "DROP TABLE t_cum_above"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  statement =  "DROP TABLE t_cum_below"
  res = sqlQuery(con, query = statement, errors = FALSE)  	#delete any existing table
  
  
  } #end computation musle k
  
  
  #copy from other tables ####
  if ("musle-k" %in% tolower(copy_from_other_tables))
  {  
    statement =  "UPDATE soil_veg_components INNER JOIN soils ON soils.pid=soil_veg_components.soil_id SET musle_k = a_musle_k;"
    res = sqlQuery2(con, statement, info="copy MUSLE-K")  
  }
  
  if ("manning-n" %in% tolower(copy_from_other_tables))
  {  
    statement =  "UPDATE soil_veg_components INNER JOIN vegetation ON vegetation.pid=soil_veg_components.veg_id SET manning_n = c_manning_n;"
    res = sqlQuery2(con, statement, info="copy Manning-n")  
  }
  
  if ("musle-c" %in% tolower(copy_from_other_tables))
  {  
    statement =  "UPDATE soil_veg_components INNER JOIN vegetation ON vegetation.pid=soil_veg_components.veg_id SET musle_c1 = c_musle_c1, musle_c2 = c_musle_c2, musle_c3 = c_musle_c3, musle_c4 = c_musle_c4;"
    res = sqlQuery2(con, statement, info="copy MUSLE-C")  
  }
  
  if ("coarse_frac" %in% tolower(copy_from_other_tables))
  {  
    statement =  "UPDATE soil_veg_components INNER JOIN soils ON soils.pid=soil_veg_components.soil_id INNER JOIN horizons ON horizons.soil_id=soils.pid and horizons.position=1 SET coarse_frac = horizons.coarse_frag;"
    res = sqlQuery2(con, statement, info="copy MUSLE-C")  
  }
  
  if (!is.null(setP))
    if (!is.finite(setP))
    warning("P-factor (setP) should be numeric, ignored.") else
    {
      if (setP < 0 | setP>1)
        warning("P-factor (setP) should be between 0 and 1, ignored.")
      else
      {
        statement = paste0("UPDATE soil_veg_components set musle_p = ", setP,";")
        res = sqlQuery2(con, statement, info="copy MUSLE-C")  
      }
    }

  
  # update table meta_info
  write_metainfo(con,
                 "db_prepare_musle()",
                 c("soils","soil_veg_components"), "soils.a_*, soil_veg_components.musle_k",
                 "Computation of MUSLE K-factor ",
                 FALSE)
  
  if(verbose) message("% OK")
  
  if(verbose) message("%")
  if(verbose) message("% All data written successfully. Close ODBC connection.")
  
  tryCatch(odbcClose(con), error=function(e){})
  
  if(verbose) message("%")
  if(verbose) message("% DONE!")
  if(verbose) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
  
} # EOF

