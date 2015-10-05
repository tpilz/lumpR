# LUMP/db_echse_input.R
# Copyright (C) 2015 Tobias Pilz
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


#'  Create ECHSE input files
#'  
#'  Function takes parameters from a parameter database and generates ASCII files
#'  as input to specific model engines of the ECHSE simulation environment.
#'  
#'  @param dbname Name of the data source (DSN) registered at ODBC.
#'  
#'  @param proj_dir Path to your working directory. Output of this function will
#'  be written to this location.
#'  
#'  @param proj_name Name of your project. In \code{proj_dir} a directory \code{proj_name}
#'  will be created containing the subdirectory 'data' containing 'parameter' and 'initials' with
#'  ECHSE input files.
#'  
#'  @param overwrite \code{logical}. Should existing files and directories be
#'  overwriten? Default: \code{FALSE}.
#'  
#'  @param verbose \code{logical}. Should detailed information during execution be
#'  printed? Default: \code{TRUE}.
#'  
#'  
#'  @details Function creates most of the input files needed for ECHSE model engine WASA
#'  except for meteorological forcing which has to be prepared separately considering
#'  ECHSE tools, see \url{https://github.com/echse/echse_tools}.
#'  
#'  More information about ECHSE including the source code, documentation, specific model
#'  engines and tools for model setup can be found at \url{https://github.com/echse/}. For
#'  information on general file structures and how to run and install echse read the core,
#'  manual. The engine manual contains descriptions of the model engines (this function
#'  can be used to prepare the WASA engine only!) and incorporated processes.
#'  
#'  
#'  @return The following subdirectories and files within \code{proj_dir}/\code{proj_name}/data
#'  will be created:
#'  
#'  \bold{catchment}
#'  
#'  \emph{objDecl.dat}\cr
#'  Object declaration table containing all object definitions including membership to a specific
#'  object group for the current model setup.
#'  
#'  \emph{objLink.dat}\cr
#'  Table specifying object relations, i.e. which output of a certain object is the input of
#'  another object, and the type of the relation, i.e. \code{forwardType = true} means that
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
#'  case these are largely default values of choice flags and meteorological parameters which
#'  might have to be adapted manually, see ECHSE WASA engine documentation.
#'  
#'  \bold{vegPar_time_series}
#'  
#'  Contains time series files \emph{*_data.dat} with the first column being always the time
#'  stamp in ISO format and the other columns containing the respective variable value at
#'  a certain location. The file \emph{input_ext_locs.dat} references the variable to an object
#'  using this variable as external input whereas one object can be referenced to different
#'  locations this variable was measured with specific weight (summing up to one). \bold{ATTENTION:}
#'  \emph{input_ext_locs.dat} has to be extended by separately prepared meteorological input!
#'  
#'  This directory contains time series of {juliany day} (or \emph{day of year}) and
#'  different vegetation specific variables (root depth, canopy height, albedo, and leaf
#'  area index) as vegetation growth is not explicitly modelled within the WASA engine
#'  but is deduced from four node points of the variable within a year (see WASA parameter
#'  database, table 'vegetation') and start and end of the growing season for each year
#'  estimated from a precipitation time series (see WASA parameter database table 'rainy_season'
#'  filled via LUMP functions \code{\link[LUMP]{rainy_season}} and \code{\link[LUMP]{calc_seasonality}}).
#'  
#'  \bold{initials}
#'  
#'  Initial conditions, i.e. initial values of state variables for all objects \emph{(init_scal.dat)}.
#'  \emph{init_vect.dat} is only a dummy file as the WASA engine contains no vector state variables.
#'  
#'  Currently, soil moisture is set to value between permanent wilting point and field capacity. Groundwater
#'  and interception storages are set to zero.
#'  
#'  To avoid crude estimates you can as well use the state outputs of another simulation. In general,
#'  you should choose an appropriate warm-up period which is highly catchment dependent and carefully
#'  examine the development of state variables to reduce uncertainties from initial conditions.
#'  
#'  
#'  @references Theoretical description of the \bold{EC}o-\bold{H}ydrological \bold{S}imulation
#'  \bold{E}nvironment (ECHSE):
#'  
#'  Kneis, D. (2015): A lightweight framework for rapid development of object-based
#'  hydrological model engines. \emph{Environmental Modelling & Software}, 68, 110-121,
#'  doi: 10.1016/j.envsoft.2015.02.009
#'  
#'  @author 
#'  Tobias Pilz \email{tpilz@@uni-potsdam.de}
#'  
#'  @export

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
  db_dir <- system.file("database/", package="LUMP")
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
    dir.create(paste(proj_dir, proj_name, "data", "parameter", sep="/") , recursive=T)
    dir.create(paste(proj_dir, proj_name, "data", "catchment", sep="/") , recursive=T)
    dir.create(paste(proj_dir, proj_name, "data", "initials", sep="/") , recursive=T)
    ts_dir <- "vegPar_time_series"
    dir.create(paste(proj_dir, proj_name, "data", ts_dir, sep="/") , recursive=T)
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
  
  objpar_hor <- "paramNum_WASA_horizon.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_hor, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_hor, sep="/"))
  } else {
    stop(paste0("File ", objpar_hor, " exists!"))
  }
  
  objpar_svc <- "paramNum_WASA_svc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_svc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_svc, sep="/"))
  } else {
    stop(paste0("File ", objpar_svc, " exists!"))
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
    dir.create(paste(proj_dir, proj_name, "data", "parameter", pos2area_dir, sep="/"), recursive=T)
  } else {
    stop(paste0("Subdirectory ", pos2area_dir, " exists!"))
  }
  
  objpar_lu <- "paramNum_WASA_lu.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_lu, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_lu, sep="/"))
  } else {
    stop(paste0("File ", objpar_lu, " exists!"))
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
  # vector state initials not needed, create dummy file
  write.table(file=paste(proj_dir, proj_name, "data", "initials", initVect, sep="/"), append=T, sep="\t",
        x=data.frame(object="none", variable="none", index="none", value="none"), row.names = F, col.names = T, quote=F)

  
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
  dat_hor_sel <- dat_hor[,c("position", "thickness", "ks", "suction", "theta_s",
                            "fk", "theta_pwp", "theta_r", "pore_size_i", "bubb_pres")]
  dat_soil_sel <- data.frame(bedrock=dat_soil[,c("bedrock_flag")])
  dat_lu_sel <- dat_lu[,c("kf_bedrock", "slopelength")]
  dat_rtc_sel <- data.frame(fraction=dat_rtc[,c("fraction")])
  dat_tc_sel <- data.frame(slope=dat_tc[,c("slope")])
  dat_veg_sel <- dat_veg[,c("crop_makk", "intfc", "stomat_r", "min_suction", "max_suction")]
  
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
  names(dat_hor_sel) <- c("position", "hor_depth", "k_s", "psi", "wc_s", "wc_fc",
                          "wc_pwp", "wc_res", "pores_ind", "bubble")
  soil_depth <- tapply(dat_hor$thickness, dat_hor$soil_id, sum)
  soil_depth <- data.frame(soil_depth=soil_depth, soil_id=names(soil_depth))
  dat_hor_sel$soil_depth <- merge(dat_hor, soil_depth, by="soil_id")$soil_depth
  
  dat_hor_sel$soil_depth <- dat_hor_sel$soil_depth/1000 # mm -> m
  dat_hor_sel$hor_depth <- dat_hor_sel$hor_depth/1000 # mm -> m
  dat_hor_sel$k_s <- dat_hor_sel$k_s / 1000 / 86400 # mm/day -> m/s
  dat_hor_sel$psi <- dat_hor_sel$psi/1000 # mm -> m
  
  names(dat_lu_sel) <- c("ks_bed", "slopelength")
  dat_lu_sel$ks_bed <- dat_lu_sel$ks_bed / 1000 / 86400 # mm/day -> m/s
  
  names(dat_rtc_sel) <- "frac_area"
  
  dat_tc_sel$slope <- dat_tc_sel$slope/100 # % -> dimensionless
  
  names(dat_veg_sel) <- c("crop_makk", "intfc", "res_leaf_min", "wstressmin", "wstressmax")
  
  
  # loop over all data and declare objects
  # first: dummy object
  write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
        x=paste("dummy", "dummy", sep="\t"))
  
  flag.col.hor <- T
  flag.col.svc <- T
  flag.col.tc <- T
  flag.col.lu <- T
  for (s in unique(dat_rsub$subbas_id)) {
    r_sub <- which(dat_rsub$subbas_id == s)
    
    
    for(l in dat_rsub$lu_id[r_sub]) {
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
          r_hor <- which(dat_hor$soil_id == soil_id)
          r_s <- which(dat_sub$pid == s)
          
          # DECL #
          obj_name <- paste("svc", s, l, t, svc, sep="_")
          
          write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
                x=paste(obj_name, "WASA_svc", sep="\t"))
          
          
          # PARAMETERS #
          wc_sat_prof <- weighted.mean(dat_hor$theta_s[r_hor], dat_hor_sel$hor_depth[r_hor]/dat_hor_sel$soil_depth[r_hor])
          soil_dens_top <- dat_hor$soil_dens[r_hor][which(dat_hor$position[r_hor] == 1)]
          lati <- dat_sub$lat[r_s]
          r_veg <- which(dat_veg$pid == veg_id)
          r_rtcpar <- which(dat_rtc$tc_id == t & dat_rtc$svc_id == svc)
          out_dat <- cbind(obj_name, dat_rtc_sel[r_rtcpar,], dat_veg_sel[r_veg,], wc_sat_prof, soil_dens_top, lati)
          
          if(flag.col.svc==T)
            names(out_dat) <- c("object", names(dat_rtc_sel), names(dat_veg_sel), "wc_sat_prof", "soil_dens", "lat")
          
          write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_svc, sep="/"),
                      col.names=flag.col.svc, row.names=F, append=T, quote=F, sep="\t")
          
          flag.col.svc <- F
          
          
          # INITIALS #
          out_dat <- data.frame(object=obj_name,
                                variable="v_inter",
                                value=0)
          
          write.table(out_dat, file=paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"),
                      col.names=flag.col.hor, row.names=F, append=T, quote=F, sep="\t")
          
          
          # LINK #
          tar_name <- rep(obj_name,9)
          tar_var <- c("r_latsur_in", "r_latsub_in", "wc_vol_top", "wc_vol_root",
                       "wc_prof", "wc_sat", "wc_res", "bubble", "pores_ind")
          
          sour_name <- c(rep(paste("tc", s, l, t, sep="_"), 2), 
                         paste("hor", s, l, t, svc, 1, sep="_"),
                         paste("node_svc_wcroot", s, l, t, svc, sep="_"),
                         paste("node_svc_wcprof", s, l, t, svc, sep="_"),
                         paste("node_svc_wcsat", s, l, t, svc, sep="_"),
                         paste("node_svc_wcres", s, l, t, svc, sep="_"),
                         paste("node_svc_bubb", s, l, t, svc, sep="_"),
                         paste("node_svc_poresi", s, l, t, svc, sep="_"))
          sour_var <- c("r_latsur_svc_out", "r_latsub_svc_out", "wc_top_out", rep("out", 6))
          
          feedb <- rep(F, length(sour_var))
          
          write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
                x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
          
          
          for(h in sort(dat_hor$position[r_hor])) {
            
            
            # DECL #
            obj_name <- paste("hor", s, l, t, svc, h, sep="_")
            
            write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
                       x=paste(obj_name, "WASA_horizon", sep="\t"))
            
            
            # PARAMETERS #
            r_horord <- order(dat_hor$position[r_hor])
            dat_hor_sel$depth_cum[r_hor][r_horord][h] <- sum(dat_hor$thickness[r_hor][r_horord][1:h])/1000 # mm -> m
            
            r_soil <- which(dat_soil$pid == soil_id)
            r_lupar <- which(dat_lu$pid == l)
            r_tcpar <- which(dat_tc$pid == t)
            r_rtcpar <- which(dat_rtc$tc_id == t & dat_rtc$svc_id == svc)
            
            dat_lu_sel$slopelength[r_lupar] <- dat_lu_sel$slopelength[r_lupar] * dat_rlu$fraction[r_lu][r_lu_order][r_tclu]
            
            out_dat <- cbind(obj_name, dat_hor_sel[r_hor,][r_horord,][h,], dat_soil_sel[r_soil,], dat_lu_sel[r_lupar,],
                             dat_tc_sel[r_tcpar,], dat_rtc_sel[r_rtcpar,], 1)
            
            if(flag.col.hor==T)
              names(out_dat) <- c("object", names(dat_hor_sel), names(dat_soil_sel), names(dat_lu_sel),
                                names(dat_tc_sel), names(dat_rtc_sel), "k_scal")
            
            write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_hor, sep="/"),
                        col.names=flag.col.hor, row.names=F, append=T, quote=F, sep="\t")
            
            flag.col.hor <- F
            
            
            # INITIALS #
            out_dat <- data.frame(object=obj_name,
                                  variable="wc",
                                  value=out_dat$wc_pwp + (out_dat$wc_fc-out_dat$wc_pwp)/2)
            
            write.table(out_dat, file=paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"),
                        col.names=flag.col.hor, row.names=F, append=T, quote=F, sep="\t")
                                    
            
            # LINK #
            # current horizon is target object
            tar_name <- rep(obj_name, 7)
            
            # target variables are those of type inputSim from declaration file WASA_horizon.txt
            tar_var <- c("r_evap_in", "r_transp_in", "r_latflow_in", "r_prec_inf", 
                         "r_percol_in", "v_inf_in", "wc_plant_root")
            
            # source object either is above horizon or SVC for top horizon
            if(h==1) {
              sour_name <- c(rep(paste("svc", s, l, t, svc, sep="_"),4),
                             rep("dummy",2),
                             paste("node_svc_wcplant", s, l, t, svc, sep="_"))
            } else {
              sour_name <- c(rep(paste("svc", s, l, t, svc, sep="_"),3),
                             rep(paste("hor", s, l, t, svc, h-1, sep="_"),3),
                             paste("node_svc_wcplant", s, l, t, svc, sep="_"))
            }
            
            # source variable are those of type output of the source object as defined in declaration file (engine's class declaration)
            # order compliant to order of tar_var
            if(h==1) {
              sour_var <- c("r_evap_out", "r_transp_out", "r_soilsub", "r_soilsur",
                            rep("dummy",2), "out")
            } else {
              sour_var <- c("r_evap_out", "r_transp_out", "r_soilsub", "r_prec_inf_out",
                            "r_percol_out", "v_inf_out", "out")
            }
            
            # feedback types
            feedb <- c(rep(T,6), F)
            
            # write to link file
            write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
                  x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
            
            
          } # horizon loop
          
          
          # node object to combine horizon fluxes of specific svc
          obj_name <- c(paste("node_svc_latsursvc", s, l, t, svc, sep="_"),
                        paste("node_svc_latsurtc", s, l, t, svc, sep="_"),
                        paste("node_svc_latsubtc", s, l, t, svc, sep="_"),
                        paste("node_svc_latsubsvc", s, l, t, svc, sep="_"),
                        paste("node_svc_wcprof", s, l, t, svc, sep="_"),
                        paste("node_svc_wcplant", s, l, t, svc, sep="_"),
                        paste("node_svc_wcroot", s, l, t, svc, sep="_"),
                        paste("node_svc_wcsat", s, l, t, svc, sep="_"),
                        paste("node_svc_wcres", s, l, t, svc, sep="_"),
                        paste("node_svc_bubb", s, l, t, svc, sep="_"),
                        paste("node_svc_poresi", s, l, t, svc, sep="_"))
          
          # append to object declaration file
          decl_str <- paste(obj_name, paste0("WASA_node_n", length(r_hor)), sep="\t")
          decl_str[1:2] <- paste(obj_name[1:2], paste0("WASA_node_n", length(r_hor)+1), sep="\t")
          write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
                x=decl_str)
          
          # LINK #
          tar_name <- c(rep(obj_name, length(r_hor)), obj_name[1:2])
          tar_var <- c(rep(paste0("in", 1:length(r_hor)), each=length(obj_name)), rep(paste0("in", length(r_hor)+1), 2))
          
          sour_name <- c(rep(paste("hor", s, l, t, svc, 1:length(r_hor), sep="_"), each=length(obj_name)),
                         rep(paste("svc", s, l, t, svc, sep="_"), 2))
          sour_var <- c(rep(c("r_latsur_svc", "r_latsur_tc", "r_latsub_tc", "r_latsub_svc",
                        "wc_prof_out", "wc_plant_out", "wc_root_out", "wc_sat_root",
                        "wc_res_root", "bubble_root", "pores_ind_root"), length(r_hor)),
                        "r_latsur_svc", "r_latsur_tc")

          feedb <- rep(T,length(tar_name))
          
          # write to link file
          write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
                x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
          
        } # svc loop
        
        
        # NODE object to combine svc fluxes of specific TC
        obj_name <- c(paste("node_tc_latsurtc", s, l, t, sep="_"),
                      paste("node_tc_latsubtc", s, l, t, sep="_"),
                      paste("node_tc_latsursvc", s, l, t, sep="_"),
                      paste("node_tc_latsubsvc", s, l, t, sep="_"),
                      paste("node_tc_percol", s, l, t, sep="_"))
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
              x=paste(obj_name, paste0("WASA_node_n", length(r_tc)), sep="\t"))
        
        # Link
        tar_name <- rep(obj_name, each=length(r_tc))
        tar_var <- rep(paste0("in", 1:length(r_tc)), length(obj_name))
        
        s_id <- dat_svc$soil_id[dat_svc$pid %in% dat_rtc$svc_id[r_tc]]
        hors <- unlist(lapply(s_id, function(x) max(dat_hor$position[dat_hor$soil_id == x])))
        sour_name <- c(paste("node_svc_latsurtc", s, l, t, dat_rtc$svc_id[r_tc], sep="_"),
                      paste("node_svc_latsubtc", s, l, t, dat_rtc$svc_id[r_tc], sep="_"),
                      paste("node_svc_latsursvc", s, l, t, dat_rtc$svc_id[r_tc], sep="_"),
                      paste("node_svc_latsubsvc", s, l, t, dat_rtc$svc_id[r_tc], sep="_"),
                      paste("hor", s, l, t, dat_rtc$svc_id[r_tc], hors, sep="_"))
        sour_var <- c(rep("out", 4*length(r_tc)), rep("r_percol_out", length(r_tc)))
        
        feedb <- rep(T,length(tar_name))
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
              x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
        
        
        # TC OBJECT #
        # DECL #
        obj_name <- paste("tc", s, l, t, sep="_")
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
              x=paste(obj_name, "WASA_tc", sep="\t"))
        
        # PARAMETERS #
        # numeric
        out_dat <- cbind(obj_name, dat_rlu$position[r_lu][r_lu_order][r_tclu], length(r_lu))
        
        if(flag.col.tc==T)
          dimnames(out_dat) <- list(NULL, c("object", "position", "no_tc"))
        
        write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_tc, sep="/"),
                    col.names=flag.col.tc, row.names=F, append=T, quote=F, sep="\t")
        
        # parameter function
        if(r_tclu == 1) {
          parfun_file <- paste(proj_dir, proj_name, "data", "parameter", pos2area_dir, paste0("pos2area_LU",l, ".dat"), sep="/")
          out_dat <- data.frame(position=dat_rlu$position[r_lu], area_frac=dat_rlu$fraction[r_lu])
          write.table(out_dat, file=parfun_file, quote=F, sep="\t", row.names=F)
        }
          
        out_dat <- data.frame(object=obj_name, "function"="pos2area", col_arg="position", col_val="area_frac",
                              file=parfun_file)
        names(out_dat)[2] <- "function"
        
        write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", parfun_tc, sep="/"),
                    col.names=flag.col.tc, row.names=F, append=T, quote=F, sep="\t")
        
        flag.col.tc <- F
        
        # LINK #
        if(r_tclu == 1) { # upslope TC
          sour_name <- c(rep("dummy", 2), paste("node_tc_latsurtc", s, l, t, sep="_"),
                         paste("node_tc_latsubtc", s, l, t, sep="_"),
                         paste("node_tc_latsursvc", s, l, t, sep="_"),
                         paste("node_tc_latsubsvc", s, l, t, sep="_"))
          sour_var <- c(rep("dummy", 2), rep("out", 4))
        } else { # downslope TC
          sour_name <- c(rep(obj_tc_last, 2), paste("node_tc_latsurtc", s, l, t, sep="_"),
                         paste("node_tc_latsubtc", s, l, t, sep="_"),
                         paste("node_tc_latsursvc", s, l, t, sep="_"),
                         paste("node_tc_latsubsvc", s, l, t, sep="_"))
          sour_var <- c("r_latsur_tc_out", "r_latsub_tc_out", rep("out", 4))
        }
        
        tar_name <- rep(obj_name, length(sour_var))
        tar_var <- c("r_latsur_up", "r_latsub_up", "r_latsur_tc", "r_latsub_tc",
                     "r_latsur_svc", "r_latsub_svc")
        
        feedb <- rep(T,length(tar_name))
        
        write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
              x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
        
        # save object name for next loop
        obj_tc_last <- obj_name
        
      } # tc loop
      
      
      # node object to combine tc fluxes of specific lu
      obj_name <- c(paste("node_lu_river", s, l, sep="_"),
                    paste("node_lu_gw", s, l, sep="_"))
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
            x=paste(obj_name, paste0("WASA_node_n", length(r_lu)), sep="\t"))
      
      # link
      tar_name <- rep(obj_name, each=length(r_lu))
      tar_var <- rep(paste0("in", 1:length(r_lu)), length(obj_name))
      
      sour_name <- c(paste("tc", s, l, dat_rlu$tc_id[r_lu], sep="_"),
                     paste("node_tc_percol", s, l, dat_rlu$tc_id[r_lu], sep="_"))
      sour_var <- c(rep("r_river_out", length(r_lu)), rep("out", length(r_lu)))
        
      feedb <- rep(T, length(sour_name))
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
            x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
      
      
      # lu object
      obj_name <- paste("lu", s, l, sep="_")
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
            x=paste(obj_name, "WASA_lu", sep="\t"))
      
      # PARAMETERS #
      out_dat <- cbind(obj_name, dat_lu$gw_flag[r_lupar], dat_lu$frgw_delay[r_lupar])
      
      if(flag.col.lu==T)
        dimnames(out_dat) <- list(NULL, c("object", "gw_flag", "gw_delay"))
      
      write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_lu, sep="/"),
                  col.names=flag.col.lu, row.names=F, append=T, quote=F, sep="\t")
      
      flag.col.lu <- F
      
      # INITIALS #
      out_dat <- data.frame(object=obj_name,
                            variable="v_gw",
                            value=0)
      
      write.table(out_dat, file=paste(proj_dir, proj_name, "data", "initials", initScal, sep="/"),
                  col.names=flag.col.hor, row.names=F, append=T, quote=F, sep="\t")
      
      # Link
      tar_name <- obj_name
      tar_var <- "r_gw_rch"
      
      sour_name <- paste("node_lu_gw", s, l, sep="_")
      sour_var <- "out"
      
      feedb <- T
      
      write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
            x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
      
    } # lu loop
    
    
    # node object to combine lu fluxes of specific sub
    obj_name <- c(paste("node_su_quick", s, sep="_"),
                  paste("node_su_base", s, sep="_"))
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
          x=paste(obj_name, paste0("WASA_node_n", length(r_sub)), sep="\t"))
    
    # link
    tar_name <- rep(obj_name, each=length(r_sub))
    tar_var <- rep(paste0("in", 1:length(r_sub)), length(obj_name))
    
    sour_name <- c(paste("node_lu_river", s, dat_rsub$lu_id[r_sub], sep="_"),
                   paste("lu", s, dat_rsub$lu_id[r_sub], sep="_"))
    sour_var <- c(rep("out", length(r_sub)), rep("r_gw_out", length(r_sub)))
    
    feedb <- rep(T, length(sour_name))
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
          x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
    
    # sub object
    obj_name <- paste("sub", s, sep="_")
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objdecl, sep="/"), append = T,
          x=paste(obj_name, "WASA_sub", sep="\t"))
    
    # link
    tar_name <- rep(obj_name,2)
    tar_var <- c("r_river_quick", "r_river_base")
    
    sour_name <- c(paste("node_su_quick", s, sep="_"), paste("node_su_base", s, sep="_"))
    sour_var <- rep("out",2)
    
    feedb <- c(T, T)
    
    write(file=paste(proj_dir, proj_name, "data", "catchment", objlink, sep="/"), append = T,
          x=paste(tar_name, tar_var, sour_name, sour_var, feedb, sep="\t", collapse="\n"))
    
  } # sub loop
  
  if(verbose)
    print("OK.")
  
  
  
### GROUP SPECIFIC (SHARED) PARAMETERS ###
  
  if(verbose)
    print(paste0("Calculate group-specific (shared) parameters ..."))
  
  out_dat <- data.frame(parameter=c("h_tempMeas", "h_windMeas", "h_humMeas", "choice_etp",
                                    "choice_eta", "choice_evap", "choice_transp",
                                    "emis_a", "emis_b", "fcorr_a", "fcorr_b", "radex_a", "radex_b",
                                    "ext", "glo_half", "res_b", "choice_rcs", "choice_rsa", "choice_raa",
                                    "drag_coef", "rough_bare", "eddy_decay", "rss_a", "rss_b",
                                    "par_stressHum", "par_satVar1", "par_satVar2", "par_satFrac1",
                                    "par_satFrac2", "par_satFrac3", "par_satFrac4", "par_satFrac5"),
                        value=c(2, 6, 2, 4, 
                                1, 1, 1,
                                0.52, -0.065, 1.35, -0.35, 0.18, 0.55,
                                0.5, 100, 25, 2, 2, 2,
                                0.07, 0.01, 2.5, 26, -1,
                                0.03, 0.05, 0.1, 0,
                                0.1, 0.5, 0.9, 1))
  
  sharedpar_svc <- "sharedParamNum_WASA_svc.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/"))
  } else {
    stop(paste0("File ", sharedpar_svc, " exists!"))
  }
  
  write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", sharedpar_svc, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t")
  
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
  
  write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t")
  
  
  out_dat <- data.frame(object="NODATA", 'function'="NODATA", col_arg=0, col_val=0, file="NODATA")
  names(out_dat)[2] <- "function"
  
  objpar_dummy <- "dummy_fun.dat"
  if(!file.exists(paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/")) | overwrite){
    file.create(paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"))
  } else {
    stop(paste0("File ", objpar_dummy, " exists!"))
  }
  
  write.table(out_dat, file=paste(proj_dir, proj_name, "data", "parameter", objpar_dummy, sep="/"),
              col.names=T, row.names=F, append=T, quote=F, sep="\t")
  
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
  # write external location links; include day of year
  veg_vars <- c("height", "rootd", "lai", "alb")
  veg_vars_hor <- c("rootd", "rootgr")
  veg_vars_svc <- c("height", "lai", "alb")
  dat_rainy_expand <- NULL
  for (s in unique(dat_rsub$subbas_id)) {
    # get svc objects
    svc <- grep(paste0("^svc_", s, "_*"), objDecl_dat$object, value=T)
    
    # all/unique svcs
    svc_un <- unlist(strsplit(svc, "_"))
    svc_all <- svc_un[seq(5,length(svc_un), by=5)]
    svc_un <- unique(svc_un[seq(5,length(svc_un), by=5)])
    
    # horizons
    hor <- grep(paste0("^hor_", s, "_*"), objDecl_dat$object, value=T)
    hor_all <- unlist(strsplit(hor, "_"))
    hor_all <- hor_all[seq(5,length(hor_all), by=6)]
    
    # get all/unique vegetation IDs corresponding to SVCs
    veg <- unique(dat_svc$veg_id[dat_svc$pid %in% svc_un])
    veg_all <- unlist(lapply(svc_all, function(x) dat_svc$veg_id[dat_svc$pid == x]))
    veg_all_hor <- unlist(lapply(hor_all, function(x) dat_svc$veg_id[dat_svc$pid == x]))
    
    # expand dat_rainy for each vegetation type within subbasin s
    r_rainy <- which(dat_rainy_in$subbas_id == s)
    dat_rainy_expand_t <- merge(paste(s, veg, sep="_"), dat_rainy_in[r_rainy,])
    dat_rainy_expand_t <- dat_rainy_expand_t[,-2]
    dat_rainy_expand <- rbind(dat_rainy_expand, dat_rainy_expand_t)
    
    # external location linkage
    extLink_svc <- data.frame(object=rep(svc, length(veg_vars_svc)+1), 
                               variable=c(rep(veg_vars_svc, each=length(svc)), rep("doy", length(svc))),
                               location=c(rep(paste(s, veg_all, sep="_"), length(veg_vars_svc)), rep("any", length(svc))),
                               weight=rep(1, (length(veg_vars_svc)+1)*length(svc)))
    extLink_hor <- data.frame(object=rep(hor, length(veg_vars_hor)), 
                              variable=rep(veg_vars_hor, each=length(hor)),
                              location=rep(paste(s, veg_all_hor, sep="_"), length(veg_vars_hor)),
                              weight=rep(1, length(veg_vars_hor)*length(hor)))
    
    # write to file
    extLink_out <- rbind(extLink_svc, extLink_hor)
    write.table(extLink_out, paste(proj_dir, proj_name, "data", ts_dir, extloc_file, sep="/"), 
                sep="\t", append=T, row.names = F, col.names = F, quote = F)
    
  }
  
  # loop over vegetation parameters and create time series
  colnames(dat_veg) <- gsub("root_depth", "rootd", colnames(dat_veg))
  for(v in veg_vars) {
    
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
    
    # if rootd calculate also increments between time steps (i.e. root growth)
    if (v == "rootd") {
      # initialise xts object
      rootgr <- veg_ts
      # set first values to zero
      rootgr[1,] <- 0
    
      for (i in 2:length(index(veg_ts)))
        rootgr[i,] <- as.numeric(veg_ts[i,]) - as.numeric(veg_ts[i-1,])
      
      # write output file
      write(c("end_of_interval", colnames(rootgr)), paste(proj_dir, proj_name, "data", ts_dir, paste0("veg_rootgr_data.dat"), sep="/"), sep="\t", ncolumns=ncol(rootgr)+1) 
      write.table(rootgr, paste(proj_dir, proj_name, "data", ts_dir, paste0("veg_rootgr_data.dat"), sep="/"),
                  sep="\t", quote=F, col.names=F, row.names=format(index(rootgr), '%Y-%m-%d %H:%M:%S'), append=T)
    }
    
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
  
 
  
  print("All outputs written. You still need to prepare the meteorological forcing (consider ECHSE tools, expand 'input_ext_locs.dat' accordingly)! Adjust sharedParamNum_WASA_svc.dat to your needs!")

} # EOF
