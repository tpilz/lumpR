# lumpR/db_echse_input.R
# Copyright (C) 2015,2017 Tobias Pilz
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


#' Create ECHSE input files
#' 
#' Function takes parameters from a parameter database and generates ASCII files
#' as input to specific model engines of the ECHSE simulation environment.
#' 
#' @param dbname Name of the data source (DSN) registered at ODBC.
#'  
#' @param proj_dir Path to your working directory. Output of this function will
#'  be written to this location.
#' 
#' @param proj_name Name of your project. In \code{proj_dir} a directory \code{proj_name}
#'  will be created containing the subdirectory 'data' containing 'parameter' and 'initials' with
#'  ECHSE input files.
#' 
#' @param overwrite \code{logical}. Should existing files and directories be
#'  overwriten? Default: \code{FALSE}.
#'  
#' @param verbose \code{logical}. Should detailed information during execution be
#'  printed? Default: \code{TRUE}.
#'  
#' @details Function creates most of the input files needed for the ECHSE model engine WASA,
#'  except for meteorological forcing which has to be prepared separately considering
#'  ECHSE tools, see \url{https://github.com/echse/echse_tools}.
#'  
#'  More information about ECHSE, including the source code, documentation, specific model
#'  engines, and tools for model setup, can be found at \url{https://github.com/echse/}. For
#'  information on general file structures and how to run and install echse, read the core
#'  manual. The engine manual contains descriptions of the model engines and incorporated processes.
#'  
#' @note So far, this function is only capable to create input files for the ECHSE model engine WASA.
#'  Other ECHSE engines cannot be initialised with this function, although that might be enabled in
#'  the future.
#'  
#' @return The following subdirectories and files within \code{proj_dir}/\code{proj_name}/data
#'  will be created:
#'  
#'  \bold{catchment}
#'  
#'  \emph{objDecl.dat}\cr
#'  Object declaration table containing all object definitions ,including membership to a specific
#'  object group for the current model setup.
#'  
#'  \emph{objLink.dat}\cr
#'  Table specifying object relations, i.e. which output of a certain object is the input of
#'  another object, and the type of the relation. Hereby, \code{forwardType = true} means that
#'  the source object is simulated before the target object and vice verse if \code{forwardType = false}.
#'  
#'  \bold{parameter}
#'  
#'  \emph{dummy_fun.dat}\cr
#'  Dummy parameter function file used as input if no parameter function is given for a specific
#'  object group.
#'  
#'  \emph{dummy_num.dat}\cr
#'  Dummy parameter file used as input if no parameters are given for a specific object group.
#'  
#'  \emph{paramFun_WASA_tc.dat}\cr
#'  Defines parameter functions for objects of class 'WASA_tc' whereas the specific look-up tables are
#'  given in the linked files, in this case within subdirectory 'parFun_pos2area'. These are
#'  relating the relative position along the hillslope of a certain TC within a LU to the
#'  areal fraction covered.
#'  
#'  \emph{paramNum_*.dat}\cr
#'  Parameter files containing parameter definitions of individual objects of the respective
#'  object group. Values have been derived from the WASA parameter database (ATTENTION: some
#'  parameter names have been changed and unit conversions where necessary!). For description
#'  of parameters see the ECHSE WASA engine documentation.
#'  
#'  \emph{SharedParamNum_*.dat}\cr
#'  Definition of parameter values used by all objects of a specific object group. In this
#'  case, these are largely default values of choice flags and meteorological parameters which
#'  might have to be adapted manually, see ECHSE WASA engine documentation.
#'  
#'  \bold{vegPar_time_series}
#'  
#'  Contains time series files \emph{*_data.dat} of external input/forcing variables related to
#'  vegetation grwoth. The first column is the time stamp in ISO format and the other columns
#'  contain the respective variable value at a certain location. The file \emph{input_ext_locs.dat}
#'  references the external variable to a model object, whereas one object can be referenced
#'  to different locations with specific weight (summing up to one). \bold{ATTENTION:}
#'  \emph{input_ext_locs.dat} has to be extended by separately prepared meteorological input!
#'  
#'  This function creates time series of {juliany day} (or \emph{day of year}) and
#'  different vegetation specific variables (root depth, canopy height, albedo, and leaf
#'  area index). Vegetation growth is not explicitly modelled within the WASA engine
#'  but is deduced from four node points of the variable within a year (see WASA parameter
#'  database, table 'vegetation') and start and end of the growing season for each year
#'  estimated from a precipitation time series (see WASA parameter database table 'rainy_season'
#'  filled via lumpR functions \code{\link[lumpR]{rainy_season}} and \code{\link[lumpR]{calc_seasonality}}).
#'  
#'  \bold{initials}
#'  
#'  Initial conditions, i.e. initial values of state variables for all objects (\emph{init_scal.dat}
#'  for scalar and \emph{init_vect.dat} for vectorial state variables).
#'  
#'  Currently, soil moisture is set to a value between permanent wilting point and field capacity. Groundwater
#'  and interception storages are set to zero.
#'  
#'  To avoid crude estimates, you can as well use the state outputs of another simulation. In general,
#'  you should choose an appropriate warm-up period, which is highly catchment dependent(!), and carefully
#'  examine the development of state variables to reduce uncertainties from initial conditions.
#'  
#'  
#' @references Theoretical description of the \bold{EC}o-\bold{H}ydrological \bold{S}imulation
#'  \bold{E}nvironment (ECHSE):
#'  
#'  Kneis, D. (2015): A lightweight framework for rapid development of object-based
#'  hydrological model engines. \emph{Environmental Modelling & Software}, 68, 110-121,
#'  doi: 10.1016/j.envsoft.2015.02.009
#'  
#' @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}
#'  
#' @export

db_echse_input <- function(
  dbname,
  proj_dir = "./",
  proj_name = "",
  overwrite=F,
  verbose = TRUE
) {
  
  if (verbose)
    print("Connecting to database ...")
  
  # connect to ODBC registered database
  suppressWarnings(con <- odbcConnect(dbname, believeNRows=F))
  
  if (con == -1)
    print(paste0("Could not connect to database '", dbname, "'. Type 'odbcDataSources()' to see the data sources known to ODBC.",
                 " If you want to connect to a MS Access database make sure you are using 32 bit R."))
  
  if(verbose)
    print("OK.")
  
  # ensure MySQL/MariaDB uses ANSI quotation (double quotes instead of back ticks)
  if(grepl("MariaDB", odbcGetInfo(con)["DBMS_Name"], ignore.case=T))
    sqlQuery(con, "SET sql_mode='ANSI';")
  
  
  # get most recent db version from update sql files in source directory
  db_dir <- system.file("database/", package="lumpR")
  db_up_files <- dir(db_dir, pattern="update_[a-zA-Z0-9_]*.sql")
  db_ver_max <- max(as.integer(sub(".sql", "", sub("update_db_v", "", db_up_files))))
  
  #check current db version
  if(verbose)
    print("Check database version ...")
  db_ver <- max(sqlFetch(con, "db_version")$version)
  if(db_ver < db_ver_max) {
    odbcClose(con)
    stop(paste0("Database version is prior to version ",db_ver_max, ". Make sure you use the latest database version (consider function db_update())!"))
  }
  if(verbose)
    print("OK.")
  
  # create and/or check output directory
  if(!file_test("-d", paste(proj_dir, proj_name, sep="/")) | overwrite == T) {
    if(verbose)
      print(paste0("Output directory ", paste(proj_dir, proj_name, "data", sep="/"), " with all sub-directories will be created."))
    dir.create(paste(proj_dir, proj_name, "data", "parameter", sep="/") , recursive=T, showWarnings=F)
    dir.create(paste(proj_dir, proj_name, "data", "catchment", sep="/") , recursive=T, showWarnings=F)
    dir.create(paste(proj_dir, proj_name, "data", "initials", sep="/") , recursive=T, showWarnings=F)
    ts_dir <- "vegPar_time_series"
    dir.create(paste(proj_dir, proj_name, "data", ts_dir, sep="/") , recursive=T, showWarnings=F)
  } else {
    stop(paste0("Output directory ", paste(proj_dir, proj_name, sep="/"), " already exists!"))
  }
  

 
  
###############################################################################
### object declaration, linkage, object-specific parameters and initial conditions
  
  if(verbose)
    print(paste0("Calculate object declaration, linkage, object-specific parameters and initial conditions ..."))
  
  # create files
  objdecl <- "objDecl.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"))
  } else {
    stop(paste0("File ", objdecl, " exists!"))
  }
  
  objlink <- "objLink.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"))
  } else {
    stop(paste0("File ", objlink, " exists!"))
  }
  
  objpar_svc <- "paramNum_WASA_svc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_svc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_svc, sep="/"))
  } else {
    stop(paste0("File ", objpar_svc, " exists!"))
  }
  
  parfun_svc <- "paramFun_WASA_svc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", parfun_svc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", parfun_svc, sep="/"))
  } else {
    stop(paste0("File ", parfun_svc, " exists!"))
  }
  
  horpars_dir <- "parFun_horpars"
  if(!dir.exists(paste(proj_dir, proj_name, "data", "parameter", horpars_dir, sep="/")) | overwrite){
    dir.create(paste(proj_dir, proj_name, "data", "parameter", horpars_dir, sep="/"), recursive=T, showWarnings=F)
  } else {
    stop(paste0("Subdirectory ", horpars_dir, " exists!"))
  }
  
  objpar_tc <- "paramNum_WASA_tc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_tc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_tc, sep="/"))
  } else {
    stop(paste0("File ", objpar_tc, " exists!"))
  }
  
  parfun_tc <- "paramFun_WASA_tc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", parfun_tc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", parfun_tc, sep="/"))
  } else {
    stop(paste0("File ", parfun_tc, " exists!"))
  }
  
  pos2area_dir <- "parFun_pos2area"
  if(!dir.exists(paste(proj_dir, proj_name, "data", "parameter", pos2area_dir, sep="/")) | overwrite){
    dir.create(paste(proj_dir, proj_name, "data", "parameter", pos2area_dir, sep="/"), recursive=T, showWarnings=F)
  } else {
    stop(paste0("Subdirectory ", pos2area_dir, " exists!"))
  }
  
  objpar_lu <- "paramNum_WASA_lu.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_lu, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_lu, sep="/"))
  } else {
    stop(paste0("File ", objpar_lu, " exists!"))
  }
  
  objpar_sub <- "paramNum_WASA_sub.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_sub, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_sub, sep="/"))
  } else {
    stop(paste0("File ", objpar_sub, " exists!"))
  }
  
  initScal <- "init_scal.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "initials", initScal, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"))
  } else {
    stop(paste0("File ", initScal, " exists!"))
  }
  
  initVect <- "init_vect.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "initials", initVect, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "initials", initVect, sep="/"))
  } else {
    stop(paste0("File ", initVect, " exists!"))
  }

  
  # write header
  write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append=T,
             x=c("object\tobjectGroup"))
  write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append=T,
        x=c("targetObject\ttargetVariable\tsourceObject\tsourceVariable\tforwardType"))
  write(file=paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"), append=T,
        x=c("object\tvariable\tvalue"))
  write(file=paste(proj_dir, proj_name, "data", "initials", initVect, sep="/"), append=T,
        x=c("object\tvariable\tindex\tvalue"))

  
  # get data from database
  dat_rsub <- sqlFetch(con, "r_subbas_contains_lu")
  dat_rlu <- sqlFetch(con, "r_lu_contains_tc")
  dat_rtc <- sqlFetch(con, "r_tc_contains_svc")
  dat_svc <- sqlFetch(con, "soil_veg_components")
  dat_hor <- sqlFetch(con, "horizons")
  dat_soil <- sqlFetch(con, "soils")
  dat_lu <- sqlFetch(con, "landscape_units")
  dat_tc <- sqlFetch(con, "terrain_components")
  dat_veg <- sqlFetch(con, "vegetation")
  dat_sub <- sqlFetch(con, "subbasins")
  
  # select parameters
  dat_hor_sel <- dat_hor[,c("soil_id", "position", "thickness", "ks", "suction", "theta_s",
                            "fk", "theta_pwp", "theta_r", "pore_size_i", "bubb_pres")]
  dat_soil_sel <- dat_soil[,c("pid", "bedrock_flag", "Phil_s", "Phil_a", "Hort_ini", "Hort_end", "Hort_k")]
  dat_lu_sel <- data.frame(slopelength=dat_lu[,c("slopelength")])
  dat_rtc_sel <- data.frame(fraction=dat_rtc[,c("fraction")])
  dat_tc_sel <- data.frame(slope=dat_tc[,c("slope")])
  dat_veg_sel <- dat_veg[,c("crop_makk", "crop_faoref", "intfc", "stomat_r", "min_suction", "max_suction", "wc_etmax", "par_stressHum", "glo_half")]
  
  # check if all necessary information are given
  if (ifelse(is.data.frame(dat_hor_sel), nrow(dat_hor_sel) == 0, length(dat_hor_sel) == 0) | any(is.na(dat_hor_sel)))
    stop(paste0("Cannot write parameter file(s) as necessary column(s) contain missing values in table 'horizons'!"))
  if (ifelse(is.data.frame(dat_soil_sel), nrow(dat_soil_sel) == 0, length(dat_soil_sel) == 0) | any(is.na(dat_soil_sel)))
    stop(paste0("Cannot write parameter file(s) as necessary column(s) contain missing values in table 'soils'!"))
  if (ifelse(is.data.frame(dat_lu_sel), nrow(dat_lu_sel) == 0, length(dat_lu_sel) == 0) | any(is.na(dat_lu_sel)))
    stop(paste0("Cannot write parameter file(s) as necessary column(s) contain missing values in table 'landscape_units'!"))
  if (ifelse(is.data.frame(dat_rtc_sel), nrow(dat_rtc_sel) == 0, length(dat_rtc_sel) == 0) | any(is.na(dat_rtc_sel)))
    stop(paste0("Cannot write parameter file(s) as necessary column(s) contain missing values in table 'r_tc_contains_svc'!"))
  if (ifelse(is.data.frame(dat_tc_sel), nrow(dat_tc_sel) == 0, length(dat_tc_sel) == 0) | any(is.na(dat_tc_sel)))
    stop(paste0("Cannot write parameter file(s) as necessary column(s) contain missing values in table 'terrain_components'!"))
  if (ifelse(is.data.frame(dat_veg_sel), nrow(dat_veg_sel) == 0, length(dat_veg_sel) == 0) | any(is.na(dat_veg_sel)))
    stop(paste0("Cannot write parameter file(s) as necessary column(s) contain missing values in table 'vegetation'!"))
  
  # calculate target information (incl. unit conversion, adjusting parameter names etc.)
  names(dat_hor_sel) <- c("soil_id", "position", "hor_depth", "ksat", "suc", "wc_sat", "wc_fc",
                          "wc_pwp", "wc_res", "pores_ind", "bubble")
  soil_depth <- tapply(dat_hor$thickness, dat_hor$soil_id, sum)
  soil_depth <- data.frame(soil_depth=soil_depth, pid=names(soil_depth))
  dat_soil_sel$soil_depth <- merge(dat_soil, soil_depth, by="pid")$soil_depth
  
  dat_soil_sel$soil_depth <- dat_soil_sel$soil_depth/1000 # mm -> m
  dat_hor_sel$hor_depth <- dat_hor_sel$hor_depth/1000 # mm -> m
  dat_hor_sel$ksat <- dat_hor_sel$ksat / 1000 / 86400 # mm/day -> m/s
  dat_hor_sel$suc <- dat_hor_sel$suc/1000 # mm -> m
  dat_hor_sel$bubble <- dat_hor_sel$bubble/100 # cm -> m
  
  names(dat_rtc_sel) <- "frac_area"
  
  dat_tc_sel$slope <- dat_tc_sel$slope/100 # % -> dimensionless
  
  names(dat_veg_sel) <- c("crop_makk", "crop_faoref", "intfc", "res_leaf_min", "wstressmin", "wstressmax", "wc_etmax", "par_stressHum", "glo_half")
  
  dat_veg_sel$wstressmin <- dat_veg_sel$wstressmin/100 # hPa -> 100 hPa THAT IS cm -> m of water
  dat_veg_sel$wstressmax <- dat_veg_sel$wstressmax/100 # hPa -> 100 hPa THAT IS cm -> m of water
  
  # loop over all data and declare objects
  # first: dummy object
  write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
        x=paste("dummy", "dummy", sep="\t"))
  
  flag.col.hor <- T
  flag.col.svc <- T
  flag.col.tc <- T
  flag.col.lu <- T
  flag.col.sub <- T
  for (s in unique(dat_rsub$subbas_id)) {
    r_sub <- which(dat_rsub$subbas_id == s)
    
    
    for(l in dat_rsub$lu_id[r_sub]) {
      r_lusub <- which(dat_rsub$lu_id == l & dat_rsub$subbas_id == s)
      r_lu <- which(dat_rlu$lu_id == l)
      r_lu_order <- order(dat_rlu$position[r_lu], decreasing=T) # order accordinto position (upslope to downslope)
      
      
      r_tclu <- 0
      for(t in dat_rlu$tc_id[r_lu][r_lu_order]) {
        r_tc <- which(dat_rtc$tc_id == t)
        r_tclu <- r_tclu+1
        
        
        for(svc in dat_rtc$svc_id[r_tc]) {
          r_svc <- which(dat_svc$pid == svc)
          soil_id <- dat_svc$soil_id[r_svc]
          veg_id <- dat_svc$veg_id[r_svc]
          r_rtcpar <- which(dat_rtc$tc_id == t & dat_rtc$svc_id == svc)
          r_s <- which(dat_sub$pid == s)
          r_hors <- which(dat_hor_sel$soil_id == soil_id)
          r_soil <- which(dat_soil_sel$pid == soil_id)
          r_lupar <- which(dat_lu$pid == l)
          r_tcpar <- which(dat_tc$pid == t)
          r_veg <- which(dat_veg$pid == veg_id)
          
          
          
          # DECL #
          obj_name <- paste("svc", s, l, t, svc, sep="_")
          
          write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
                x=paste(obj_name, "WASA_svc", sep="\t"))
          
      
          
          # PARAMETERS #
          # horizon-specific -> paramFun; for each soil type one file
          parfun_soilpar_file <- paste(proj_dir, proj_name, "data", "parameter", horpars_dir, paste0("horpars_Soil", soil_id, ".dat"), sep="/")
          write.table(dat_hor_sel[r_hors, -1], parfun_soilpar_file, quote=F, sep="\t", row.names=F)
          # paramFun file
          out_dat <- data.frame(object=obj_name, "function"=names(dat_hor_sel)[-c(1,2)], col_arg="position", col_val=names(dat_hor_sel)[-c(1,2)],
                                file=parfun_soilpar_file)
          names(out_dat)[2] <- "function"
          suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", parfun_svc, sep="/"),
                      col.names=flag.col.svc, row.names=F, append=T, quote=F, sep="\t"))
          
          
          # svc parameters -> paramNum
          out_dat <- cbind(obj_name, dat_rtc_sel[r_rtcpar,], dat_sub$lat[r_s], dat_sub$lon[r_s], dat_sub$elev[r_s],
                           dat_soil_sel[r_soil,-1], dat_hor$soil_dens[r_hors][1], dat_lu_sel$slopelength[r_lupar] * dat_rlu$fraction[r_lu][r_lu_order][r_tclu],
                           dat_tc_sel$slope[r_tcpar], dat_veg_sel[r_veg,])
          if(flag.col.svc==T)
            names(out_dat) <- c("object", "frac_area", "lat", "lon", "elev",
                              "bedrock", "Phil_s", "Phil_a", "Hort_ini", "Hort_end", "Hort_k", "soil_depth", "soil_dens", "slopelength",
                              "slope", "crop_makk", "crop_faoref", "intfc", "res_leaf_min", "wstressmin", "wstressmax", "wc_etmax", "par_stressHum", "glo_half")
          
          suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_svc, sep="/"),
                      col.names=flag.col.svc, row.names=F, append=T, quote=F, sep="\t"))
          
          

          
          # INITIALS #
          # vect
          out_dat <- data.frame(object=obj_name,
                                variable=rep(c("wc", "w_eta", "mat_pot", "k_u"), each=length(r_hors)),
                                index=rep( seq(0,length(r_hors)-1), 4),
                                value=c(dat_hor_sel$wc_fc[r_hors], rep(-9999.,3*length(r_hors))))
          suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "initials", initVect, sep="/"),
                      col.names=F, row.names=F, append=T, quote=F, sep="\t"))
          
          # scal
          out_dat <- data.frame(object=obj_name,
                                variable=c("runst_surf_sat", "runst_surf_inf", "runst_surf", "runst_sub", "runst_gw",
                                           "v_interc", "s_longrad", "et_p", "et_a", "et_i", "r_interc"),
                                value=0)
          suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"),
                      col.names=F, row.names=F, append=T, quote=F, sep="\t"))
          
          
          
  
          # LINK #
          # current horizon is target object
          tar_name <- rep(obj_name, 2)
          
          # target variables are those of type inputSim from declaration file WASA_svc.txt
          tar_var <- c("r_surf_in", "r_sub_in")
          
          # source object is TC (re-)distributing flows
          sour_name <- rep(paste("tc", s, l, t, sep="_"), 2)
          
          # source variable are those of type output of the source object as defined in declaration file (engine's class declaration)
          # order compliant to order of tar_var
          sour_var <- c("r_latsur_svc_out", "r_latsub_svc_out")
          
          # feedback types
          feedb <- c(F,F)
          
          # write to link file
          write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
                x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
          
          
          
          
          flag.col.svc <- F
          
        } # svc loop
        
        
        
        
        
        
        # NODE object to combine svc fluxes of specific TC
        obj_nodetc <- c(paste("node_tc_surfsat_tc", s, l, t, sep="_"),
                      paste("node_tc_surfinf_tc", s, l, t, sep="_"),
                      paste("node_tc_sub_tc", s, l, t, sep="_"),
                      paste("node_tc_surfsat_svc", s, l, t, sep="_"),
                      paste("node_tc_surfinf_svc", s, l, t, sep="_"),
                      paste("node_tc_sub_svc", s, l, t, sep="_"),
                      paste("node_tc_gw", s, l, t, sep="_"),
                      paste("node_tc_sat", s, l, t, sep="_"),
                      paste("node_tc_plantwat", s, l, t, sep="_"),
                      paste("node_tc_soilwat", s, l, t, sep="_"),
                      paste("node_tc_etp", s, l, t, sep="_"),
                      paste("node_tc_eta", s, l, t, sep="_"),
                      paste("node_tc_eti", s, l, t, sep="_"))
        
        if( length(r_tc) <= 20 ) {
          node_n <- length(r_tc)
        } else if( (length(r_tc) > 20) && (length(r_tc) <= 50) ) {
          node_n <- 50
        } else if( (length(r_tc) > 50) && (length(r_tc) <= 100) ) {
          node_n <- 100
        } else if( (length(r_tc) > 100) && (length(r_tc) <= 150) ) {
          node_n <- 150
        } else if( (length(r_tc) > 150) && (length(r_tc) <= 200) ) {
          node_n <- 200
        } else if( (length(r_tc) > 200) && (length(r_tc) <= 250) ) {
          node_n <- 250
        } 
        # append to object declaration file
        decl_str <- paste(obj_nodetc, paste0("WASA_node_n", node_n), sep="\t")
        write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
              x=decl_str)

        
        # Link
        tar_name <- rep(obj_nodetc, each=node_n)
        
        tar_var <- rep(paste0("in", 1:node_n), length(obj_nodetc))
        
        sour_name <- paste("svc", s, l, t, dat_rtc$svc_id[r_tc], sep="_")
        if (length(sour_name) < length(tar_name)/length(obj_nodetc)) {
          sour_name <- c(sour_name, rep("dummy", length(tar_name)/length(obj_nodetc) - length(sour_name)))
        }
        
        len_diff <- length(sour_name) - length(r_tc)
        sour_var <- c(rep("run_surf_sat_tc", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("run_surf_inf_tc", length(r_tc)),
                      rep("dummy", len_diff),
                      rep("run_sub_tc", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("run_surf_sat_svc", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("run_surf_inf_svc", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("run_sub_svc", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("run_gw", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("saturation", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("v_plantwat", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("v_soilwat", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("etp", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("eta", length(r_tc)),
                      rep("dummy", len_diff), 
                      rep("eti", length(r_tc)),
                      rep("dummy", len_diff)) 
        
        feedb <- rep(T,length(sour_name))
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
              x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
        
        
        
        
        
        
        
        # TC OBJECT #
        # DECL #
        obj_name <- paste("tc", s, l, t, sep="_")
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
              x=paste(obj_name, "WASA_tc", sep="\t"))
        
        # PARAMETERS #
        # numeric
        #out_dat <- cbind(obj_name, dat_rlu$position[r_lu][r_lu_order][r_tclu], length(r_lu))
        out_dat <- cbind(obj_name, r_tclu, length(r_lu)) # reverse definition of TC "position"
        
        if(flag.col.tc==T)
          dimnames(out_dat) <- list(NULL, c("object", "position", "no_tc"))
        
        suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_tc, sep="/"),
                    col.names=flag.col.tc, row.names=F, append=T, quote=F, sep="\t"))
        
        # parameter function
        if(r_tclu == 1) {
          parfun_pos2area_file <- paste(proj_dir, proj_name, "data", "parameter", pos2area_dir, paste0("pos2area_LU",l, ".dat"), sep="/")
          out_dat <- data.frame(position=1:length(r_lu), area_frac=dat_rlu$fraction[r_lu][r_lu_order]) # reverse definition of TC "position"
          write.table(out_dat, file=parfun_pos2area_file, quote=F, sep="\t", row.names=F)
        }
          
        out_dat <- data.frame(object=obj_name, "function"="pos2area", col_arg="position", col_val="area_frac",
                              file=parfun_pos2area_file)
        names(out_dat)[2] <- "function"
        
        suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", parfun_tc, sep="/"),
                    col.names=flag.col.tc, row.names=F, append=T, quote=F, sep="\t"))
        
        flag.col.tc <- F
        
        # LINK #
        if(r_tclu == 1) { # upslope TC
          sour_name <- c("dummy", "dummy", obj_nodetc)
          sour_var <- c(rep("dummy", 2), rep("out", length(obj_nodetc)))
        } else { # downslope TC
          sour_name <- c(rep(obj_tc_last, 2), obj_nodetc)
          sour_var <- c("r_latsur_tc_out", "r_latsub_tc_out", rep("out", length(obj_nodetc)))
        }
        
        tar_name <- rep(obj_name, length(sour_var))
        tar_var <- c("r_latsur_up", "r_latsub_up",
                     "r_latsur_inf_tc", "r_latsur_sat_tc", "r_latsub_tc",
                     "r_latsur_inf_svc", "r_latsur_sat_svc", "r_latsub_svc",
                     "run_gw_svc", "sat_svc", "plantwat_svc", "soilwat_svc", "etp_svc", "eta_svc", "eti_svc")
        
        feedb <- rep(T,length(tar_name))
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
              x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
        
        # save object name for next loop
        obj_tc_last <- obj_name
        
      } # tc loop
      
      
      
      
      
      # NODE object to combine tc fluxes of specific lu
      obj_nodelu <- c(paste("node_lu_riversurf", s, l, sep="_"),
                    paste("node_lu_gw", s, l, sep="_"),
                    paste("node_lu_plantwat", s, l, sep="_"),
                    paste("node_lu_soilwat", s, l, sep="_"),
                    paste("node_lu_surfsat", s, l, sep="_"),
                    paste("node_lu_surfinf", s, l, sep="_"),
                    paste("node_lu_sub", s, l, sep="_"),
                    paste("node_lu_etp", s, l, sep="_"),
                    paste("node_lu_eta", s, l, sep="_"),
                    paste("node_lu_eti", s, l, sep="_"))
      
      if( length(r_lu) <= 20 ) {
        node_n <- length(r_lu)
      } else if( (length(r_lu) > 20) && (length(r_lu) <= 50) ) {
        node_n <- 50
      } else if( (length(r_lu) > 50) && (length(r_lu) <= 100) ) {
        node_n <- 100
      } else if( (length(r_lu) > 100) && (length(r_lu) <= 150) ) {
        node_n <- 150
      } else if( (length(r_lu) > 150) && (length(r_lu) <= 200) ) {
        node_n <- 200
      } else if( (length(r_lu) > 200) && (length(r_lu) <= 250) ) {
        node_n <- 250
      } 
      write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
            x=paste(obj_nodelu, paste0("WASA_node_n", node_n), sep="\t"))
      
      # link
      tar_name <- rep(obj_nodelu, each=node_n)
      tar_var <- rep(paste0("in", 1:node_n), length(obj_nodelu))
      
      sour_name <- paste("tc", s, l, dat_rlu$tc_id[r_lu], sep="_")
      if (length(sour_name) < length(tar_name)/length(obj_nodelu)) {
        sour_name <- c(sour_name, rep("dummy", length(tar_name)/length(obj_nodelu) - length(sour_name)))
      }
      
      len_diff <- length(sour_name) - length(r_lu)
      sour_var <- c(rep("r_river_surf", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("run_gw", length(r_lu)),
                    rep("dummy", len_diff),
                    rep("v_plantwat", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("v_soilwat", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("run_surf_sat", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("run_surf_inf", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("run_sub", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("etp", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("eta", length(r_lu)),
                    rep("dummy", len_diff), 
                    rep("eti", length(r_lu)),
                    rep("dummy", len_diff)) 
        
      feedb <- rep(T, length(sour_name))
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
            x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
      
      
      
      
      
      # LU OBJECT
      obj_name <- paste("lu", s, l, sep="_")
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
            x=paste(obj_name, "WASA_lu", sep="\t"))
      
      # PARAMETERS #
      out_dat <- cbind(obj_name, dat_rsub$fraction[r_lusub], dat_lu$frgw_delay[r_lupar]*86400, 1, -9999., -9999.)
      
      if(flag.col.lu==T)
        dimnames(out_dat) <- list(NULL, c("object", "frac_area", "ct_index", "str_base", "str_surf", "str_inter"))
      
      suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_lu, sep="/"),
                  col.names=flag.col.lu, row.names=F, append=T, quote=F, sep="\t"))
      
      flag.col.lu <- F
      
      # INITIALS #
      out_dat <- data.frame(object=obj_name,
                            variable=c("vol_surf", "vol_inter", "vol_base"),
                            value=0)
      
      suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"),
                  col.names=F, row.names=F, append=T, quote=F, sep="\t"))
      
      # Link
      tar_name <- obj_name
      tar_var <- c("r_river_sub", "r_river_surf", "r_gw_rch", "v_plantwat_tc", "v_soilwat_tc",
                   "run_surf_sat_tc", "run_surf_inf_tc", "run_sub_tc",
                   "etp_tc", "eta_tc", "eti_tc")
      
      sour_name <- c(obj_tc_last, obj_nodelu)
      sour_var <- c("r_river_sub", rep("out", length(obj_nodelu)))
      
      feedb <- T
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
            x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
      
    } # lu loop
    
    
    
    
    
    # NODE object to combine lu fluxes of specific sub
    obj_nodesu <- c(paste("node_su_riversurf", s, sep="_"),
                    paste("node_su_riversub", s, sep="_"),
                    paste("node_su_rivergw", s, sep="_"),
                    paste("node_su_plantwat", s, sep="_"),
                    paste("node_su_soilwat", s, sep="_"),
                    paste("node_su_runstor", s, sep="_"),
                    paste("node_su_surfsat", s, sep="_"),
                    paste("node_su_surfinf", s, sep="_"),
                    paste("node_su_sub", s, sep="_"),
                    paste("node_su_gw", s, sep="_"),
                    paste("node_su_etp", s, sep="_"),
                    paste("node_su_eta", s, sep="_"),
                    paste("node_su_eti", s, sep="_"))
    
    if( length(r_sub) <= 20 ) {
      node_n <- length(r_sub)
    } else if( (length(r_sub) > 20) && (length(r_sub) <= 50) ) {
      node_n <- 50
    } else if( (length(r_sub) > 50) && (length(r_sub) <= 100) ) {
      node_n <- 100
    } else if( (length(r_sub) > 100) && (length(r_sub) <= 150) ) {
      node_n <- 150
    } else if( (length(r_sub) > 150) && (length(r_sub) <= 200) ) {
      node_n <- 200
    } else if( (length(r_sub) > 200) && (length(r_sub) <= 250) ) {
      node_n <- 250
    } 
    write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
          x=paste(obj_nodesu, paste0("WASA_node_n", node_n), sep="\t"))
    
    # link
    tar_name <- rep(obj_nodesu, each=node_n)
    tar_var <- rep(paste0("in", 1:node_n), length(obj_nodesu))
    
    sour_name <- paste("lu", s, dat_rsub$lu_id[r_sub], sep="_")
    if (length(sour_name) < length(tar_name)/length(obj_nodesu)) {
      sour_name <- c(sour_name, rep("dummy", length(tar_name)/length(obj_nodesu) - length(sour_name)))
    }
    len_diff <- length(sour_name) - length(r_sub)
    sour_var <- c(rep("r_out_surf", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("r_out_inter", length(r_sub)),
                  rep("dummy", len_diff),
                  rep("r_out_base", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("v_plantwat", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("v_soilwat", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("v_runstor", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("run_surf_sat", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("run_surf_inf", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("run_sub", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("run_gw", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("etp", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("eta", length(r_sub)),
                  rep("dummy", len_diff), 
                  rep("eti", length(r_sub)),
                  rep("dummy", len_diff)) 
    
    feedb <- rep(T, length(sour_name))
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
          x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
    
    
    
    
    
    # SUB OBJECT
    obj_name <- paste("sub", s, sep="_")
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
          x=paste(obj_name, "WASA_sub", sep="\t"))
    
    # link
    tar_name <- rep(obj_name, length(obj_nodesu))
    tar_var <- c("r_river_surf", "r_river_sub", "r_river_gw", 
                 "v_plantwat_lu", "v_soilwat_lu", "v_runstor_lu", "run_surf_sat_lu", "run_surf_inf_lu", "run_sub_lu", "run_gw_lu",
                 "etp_lu", "eta_lu", "eti_lu")
    
    sour_name <- obj_nodesu
    sour_var <- rep("out", length(obj_nodesu))
    
    feedb <- rep(T, length(sour_name))
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
          x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
    
    
    # PARAMETERS #
    out_dat <- cbind(obj_name, dat_sub$area[which(dat_sub$pid == s)])
    
    if(flag.col.sub==T)
      dimnames(out_dat) <- list(NULL, c("object", "area"))
    
    suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_sub, sep="/"),
                                 col.names=flag.col.sub, row.names=F, append=T, quote=F, sep="\t"))
    
    flag.col.sub <- F
    
    
    
  } # sub loop
  
  if(verbose)
    print("OK.")
  
  
  
### GROUP SPECIFIC (SHARED) PARAMETERS ###
  
  if(verbose)
    print(paste0("Calculate group-specific (shared) parameters ..."))
  
  
  
  # SVC
  out_dat <- data.frame(parameter=c("# Meteorological parameters",
                                    "h_tempMeas", "h_windMeas", "h_humMeas", 
                                    "emis_a", "emis_b", "fcorr_a", "fcorr_b", "radex_a", "radex_b", "f_day", "f_night",
                                    "# Vegetation and land-cover parameters",
                                    "ext", "res_b", "drag_coef", "rough_bare", "eddy_decay", "rss_a", "rss_b",
                                    "# Soil water parameters",
                                    "Phil_cal", "var1", "var2", "var3", "var4", "var5", "frac1", "frac2", "frac3", "frac4", "frac5",
                                    "# Choices",
                                    "choice_et", "choice_rcs", "choice_roughLen", "choice_plantDispl", "choice_gloradmax",
                                    "choice_inf", "choice_perc", "choice_soilmod",
                                    "# Other",
                                    "na_val", "ode_accuracy", "ode_max_iter", "choice_odesolve"),
                        value=c("", 2, 2, 2, 0.34, -0.14, 1.35, -0.35, 0.25, 0.50, 0.2, 0.7,
                                "", 0.5, 25, 0.07, 0.01, 2.5, 26, -1,
                                "", 0.4, 9999, 9999, 9999, 9999, 9999, 0, 0, 0, 0, 0,
                                "", 13, 2, 2, 2, 1, 3, 2, 1,
                                "", -9999., 1e-3, 1e5, 36))
  
  sharedpar_svc <- "sharedParamNum_WASA_svc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/"))
  } else {
    stop(paste0("File ", sharedpar_svc, " exists!"))
  }
  
  suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t"))
  
  
  
  
  # LU 
  out_dat <- data.frame(parameter=c("choice_runconc", "choice_gw"),
                        value=c(2, 0))
  
  sharedpar_svc <- "sharedParamNum_WASA_lu.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/"))
  } else {
    stop(paste0("File ", sharedpar_svc, " exists!"))
  }
  
  suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t"))
  
  
  
  if(verbose)
    print("OK.")
  
  
  
### DUMMY ###
  
  if(verbose)
    print(paste0("Create dummy parameter files ..."))
  
  out_dat <- data.frame(object="NODATA", NOTHING=-9999)
  
  objpar_dummy <- "dummy_num.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"))
  } else {
    stop(paste0("File ", objpar_dummy, " exists!"))
  }
  
  suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t"))
  
  
  out_dat <- data.frame(object="NODATA", 'function'="NODATA", col_arg=0, col_val=0, file="NODATA")
  names(out_dat)[2] <- "function"
  
  objpar_dummy <- "dummy_fun.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"))
  } else {
    stop(paste0("File ", objpar_dummy, " exists!"))
  }
  
  suppressWarnings(write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t"))
  
  if(verbose)
    print("OK.")
  
  
  
### VEGETATION TIME SERIES ###
  
  if(verbose)
    print(paste0("Calculate vegetation parameter time series ..."))
  
  # prepare external locations file
  extloc_file <- "input_ext_locs.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", ts_dir, extloc_file, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", ts_dir, extloc_file, sep="/"))
  } else {
    stop(paste0("File ", extloc_file, " exists!"))
  }
  
  write(file=paste(proj_dir, proj_name, "data", ts_dir, extloc_file, sep="/"), append=T,
        x=c("object\tvariable\tlocation\tweight"))
  
  # get rainy season data
  dat_rainy <- sqlFetch(con, "rainy_season")
  
  # prepare as expected by calc_seasonality()
  dat_rainy_in <- dat_rainy[,c("subbas_id", "yearm", "node1", "node2", "node3", "node4")]
  
  # get objDecl
  objDecl_dat <- read.table(paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), header=T)
  
  # expand dat_rainy_in (unique vegetation types for each subbasin); prepare rainy_season matrix for calc_seasonality()
  # write external location links; include day of year and hour of day
  veg_vars <- c("cano_height", "rootd", "lai", "alb")
  dat_rainy_expand <- NULL
  for (s in unique(dat_rsub$subbas_id)) {
    
    # SVC
    svc <- grep(paste0("^svc_", s, "_"), as.character(objDecl_dat$object), value=T)
    svc_all <- unlist(strsplit(svc, "_"))
    svc_all <- svc_all[seq(5,length(svc_all), by=5)]
    
    # get all/unique vegetation IDs corresponding to SVCs
    veg_all <- unlist(lapply(svc_all, function(x) dat_svc$veg_id[dat_svc$pid == x]))
    
    # expand dat_rainy for each vegetation type within subbasin s
    r_rainy <- which(dat_rainy_in$subbas_id == s)
    dat_rainy_expand_t <- merge(paste(s, veg_all, sep="_"), dat_rainy_in[r_rainy,])
    dat_rainy_expand_t <- dat_rainy_expand_t[,-2]
    dat_rainy_expand <- rbind(dat_rainy_expand, dat_rainy_expand_t)
    
    # external location linkage
    extLink <- data.frame(object=rep(svc, length(veg_vars)+3), 
                              variable=c(rep(veg_vars, each=length(svc)), rep(c("doy", "hour", "utc_add"), each=length(svc))),
                              location=c(rep(paste(s, veg_all, sep="_"), length(veg_vars)), rep(c("any", "dummy", "dummy"), each=length(svc))),
                              weight=rep(1, (length(veg_vars)+3)*length(svc)))
    
    # write to file
    write.table(extLink, paste(proj_dir, proj_name, "data", ts_dir, extloc_file, sep="/"), 
                sep="\t", append=T, row.names = F, col.names = F, quote = F)
    
  }
  
  # loop over vegetation parameters and create time series
  colnames(dat_veg) <- gsub("root_depth", "rootd", colnames(dat_veg))
  colnames(dat_veg) <- gsub("height", "cano_height", colnames(dat_veg))
  for(v in subset(veg_vars, veg_vars!="rootgr")) {
    
    # prepare seasonality matrix for calc_seasonality()
    cols <- grep(v, colnames(dat_veg))
    season <- dat_veg[,c(1, cols)]
    season_in <- NULL
    for (s in unique(as.character(dat_rainy_expand$x))) {
      veg <- unlist(strsplit(s, "_"))[2]
      season_t <- season[which(season$pid == veg),]
      season_t$pid <- s
      season_in <- rbind(season_in, season_t)
    }
  
    # calculate time series
    veg_ts <- calc_seasonality(dat_rainy_expand, season_in, timezone = 'UTC')
    
    # write into ECHSE time series data file
    write(c("end_of_interval", colnames(veg_ts)), paste(proj_dir, proj_name, "data", ts_dir, paste0("veg_", v, "_data.dat"), sep="/"), sep="\t", ncolumns=ncol(veg_ts)+1) 
    write.table(veg_ts, paste(proj_dir, proj_name, "data", ts_dir, paste0("veg_", v, "_data.dat"), sep="/"),
                sep="\t", quote=F, col.names=F, row.names=format(index(veg_ts), '%Y-%m-%d %H:%M:%S'), append=T)
  
  }
  
  # day of year time series data
  doys <- as.numeric(format(index(veg_ts), "%j"))
  doy_ts <- xts(doys, index(veg_ts))
  colnames(doy_ts) <- "any"
  
  write(c("end_of_interval", colnames(doy_ts)), paste(proj_dir, proj_name, "data", ts_dir, "doy_data.dat", sep="/"), sep="\t", ncolumns=ncol(doy_ts)+1) 
  write.table(doy_ts, paste(proj_dir, proj_name, "data", ts_dir, "doy_data.dat", sep="/"),
              sep="\t", quote=F, col.names=F, row.names=format(index(doy_ts), '%Y-%m-%d %H:%M:%S'), append=T)

  
  if(verbose)
    print("OK.")
  
 
  
  print("All outputs written. You still need to prepare the meteorological forcing (consider ECHSE tools, expand 'input_ext_locs.dat' accordingly)! Adjust sharedParamNum_WASA_*.dat to your needs!")

} # EOF
