#' Wrapper function performing a complete landscape discretisation
#' 
#' @return The function returns nothing. A number of plots, text files, and GRASS data
#' are produced.
#' 
#' @details This function is a wrapper function performing a complete landscape
#' discretisation by calling the functions \code{\link[lumpR]{calc_subbas}}, \code{\link[lumpR]{lump_grass_prep}},
#' \code{\link[lumpR]{area2catena}}, \code{\link[lumpR]{prof_class}}, \code{\link[lumpR]{lump_grass_post}},
#' \code{\link[lumpR]{lump_grass_prep}}, \code{\link[lumpR]{db_create}}, \code{\link[lumpR]{db_fill}},
#' and \code{\link[lumpR]{db_check}}. See function's documentations for information about
#' the arguments.
#' 
#' @note This function was created for automated landscape discretisation, e.g. within a
#' Monte Carlo analysis. Use only if you are already familiar with the lumpR package
#' and can be sure that the procedure works for your input data!
#' 
#' @references
#'      \bold{lumpR paper}\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#'      
#' @author Tobias Pilz
#' @export

disc_wrapper <- function(
  # input #
  dem = NULL,
  drain_points = NULL,
  river = NULL,
  flowaccum = NULL,
  drainage_dir = NULL,
  lcov = NULL,
  soil = NULL,
  watermask = NULL,
  imperviousmask = NULL,
  supp_quant = NULL,
  supp_qual = NULL,
  soil_depth = NULL,
  sdr = NULL,
  veg_path = NULL,
  soil_path = NULL,
  odbc_file = "~/.odbc.ini",
  dbname = NULL,
  db_driver = "SQLITE3",
  db_desc = "lumpR database automatically generated with lumpR::disc_wrapper()",
  db_server = "localhost",
  db_checks = NULL,
  db_option = NULL,
  # output: GRASS #
  subbas = NULL,
  stream = NULL,
  stream_eha = NULL,
  stream_horton = NULL,
  points_processed = NULL,
  eha = NULL,
  flowdir = NULL,
  flowacc = NULL,
  elevriv = NULL,
  distriv = NULL,
  svc = NULL,
  lu = NULL,
  # output: files #
  output_dir = getwd(),
  svc_file = "svc.dat",
  eha_2d_file = "catena.dat",
  eha_2d_head_file = "catena_header.dat",
  luoutfile = "lu_pars.dat",
  tcoutfile = "tc_pars.dat",
  lucontainstcoutfile = "lu_tc.dat",
  tccontainssvcoutfile = "tc_svc.dat",
  terraincomponentsoutfile = "tc_db.dat",
  sub_ofile = "sub.dat",
  sub_lu = "sub_lu.dat",
  lupar_ofile = "lu_db.dat",
  recl_lu = "reclass_lu.dat",
  # parameters: discretisation #
  thresh_sub = NULL,
  eha_thres = NULL,
  no_LUs = NULL,
  no_TCs = NULL,
  # parameters: runtime and others #
  disk_swap = FALSE,
  drainp_out_id = NULL,
  thresh_stream = NULL,
  snap_dist = NULL,
  rm_spurious = NULL,
  prep_things2do = c("eha", "river", "svc"),
  groundwater = TRUE,
  ridge_thresh = NULL,
  min_cell_in_slope = NULL,
  min_catena_length = NULL,
  max_riv_dist = NULL,
  grass_files = FALSE,
  plot_catena = TRUE,
  plot_profclass = TRUE,
  plot_silhouette = TRUE,
  keep_temp = FALSE,
  overwrite = FALSE,
  db_overwrite = NULL,
  silent = FALSE,
  ncores = 1,
  addon_path = NULL
) {
  
  
  # SUBBASIN DELINEATION #
  calc_subbas(
    # INPUT #
    dem=dem,
    drain_points=drain_points,
    river=river,
    flowaccum = flowaccum,
    drainage_dir = drainage_dir,
    # OUTPUT #
    basin_out=subbas,
    stream=stream,
    points_processed=points_processed,
    # PARAMETERS #
    disk_swap = disk_swap,
    outlet=ifelse(is.null(drainp_out_id), 1, ifelse(is.character(drainp_out_id), which(drain_points$id == drainp_out_id), drainp_out_id)),
    thresh_stream=thresh_stream,
    thresh_sub=thresh_sub,
    snap_dist=snap_dist,
    rm_spurious=rm_spurious,
    keep_temp=keep_temp,
    overwrite=overwrite,
    silent=silent
  )



  # PREPROCESSING AND HILLSLOPE DEVIATION #
  lump_grass_prep(
    # INPUT #
    mask = subbas,
    dem = dem,
    lcov = lcov,
    soil = soil,
    watermask = watermask,
    imperviousmask = imperviousmask,
    # OUTPUT #
    eha=eha,
    flowdir = flowdir,
    flowacc = flowacc,
    stream = stream_eha,
    stream_horton = stream_horton,
    elevriv = elevriv,
    distriv = distriv,
    svc = svc,
    dir_out = output_dir,
    svc_ofile = svc_file,
    # PARAMETERS #
    eha_thres = eha_thres,
    sizefilter = NULL, # use default
    growrad = NULL, # use default
    keep_temp=keep_temp,
    overwrite=overwrite,
    silent=silent,
    things2do = prep_things2do,
    addon_path = addon_path
  )



  # CALCULATE MEAN CATENA FOR HILLSLOPES #
  area2catena(
    # INPUT #
    mask=subbas,
    flowacc=flowacc,
    eha=eha,
    distriv=distriv,
    elevriv=elevriv,
    supp_quant=supp_quant,
    supp_qual=supp_qual,
    # OUTPUT #
    dir_out=output_dir,
    eha_2d_file=eha_2d_file,
    eha_2d_head_file=eha_2d_head_file,
    # PARAMETERS #
    ridge_thresh=ridge_thresh,
    min_cell_in_slope=min_cell_in_slope,
    min_catena_length=min_catena_length,
    max_riv_dist=max_riv_dist,
    plot_catena=plot_catena,
    grass_files=grass_files,
    ncores=ncores,
    eha_subset=NULL,
    overwrite=overwrite,
    silent=silent
  )

  # change header file according to rstats_header
  header_dat <- readLines(paste(output_dir, eha_2d_head_file, sep="/"))
  no_LUs[1] <- no_LUs[1] * -1
  header_dat[8] <- paste(no_LUs, "\t", sep="", collapse="")
  header_dat[9] <- paste(c(no_TCs, rep(0, length(no_LUs)-1)), "\t", sep="", collapse="")
  writeLines(header_dat,paste(output_dir, eha_2d_head_file, sep="/"))



  # CATENA CLASSIFICATION INTO LANDSCAPE UNITS AND TERRAIN COMPONENTS #
  # get resolution (mean between x and y resolution)
  res <- mean(gmeta()$nsres, gmeta()$ewres)

  prof_class(
    # INPUT #
    eha_2d_file=paste(output_dir, eha_2d_file, sep="/"),
    eha_2d_head_file=paste(output_dir, eha_2d_head_file, sep="/"),
    svc_column="svc",
    # OUTPUT #
    dir_out=output_dir,
    luoutfile=luoutfile,
    tcoutfile=tcoutfile,
    lucontainstcoutfile=lucontainstcoutfile,
    tccontainssvcoutfile=tccontainssvcoutfile,
    terraincomponentsoutfile=terraincomponentsoutfile,
    recl_lu=recl_lu,
    saved_clusters=NULL,
    # PARAMETERS #
    seed=1312,
    resolution=res,
    classify_type=' ',
    max_com_length=50,
    com_length=NULL,
    make_plots=plot_profclass,
    plot_silhouette = plot_silhouette,
    eha_subset=NULL,
    eha_blacklist = NULL,
    overwrite=overwrite,
    silent=silent
  )



  # POST PROCESSING #
  lump_grass_post(
    # INPUT #
    mask = subbas,
    dem = dem,
    recl_lu = paste(output_dir, recl_lu, sep="/"),
    lu = lu,
    subbasin = subbas,
    eha = eha,
    flowacc = flowacc,
    flowdir = flowdir,
    stream_horton = stream_horton,
    soil_depth = soil_depth,
    sdr = sdr,
    # OUTPUT #
    dir_out = output_dir,
    sub_ofile = sub_ofile,
    lu_ofile = sub_lu,
    lupar_ofile = lupar_ofile,
    # PARAMETER #
    fill_holes=T,
    groundwater=groundwater,
    keep_temp = keep_temp,
    overwrite = overwrite,
    silent = silent
  )



  # create information file for filling landscape_units into database (SIMPLEST POSSIBLE PARAMETER VALUES USED HEREIN)
  luout <- read.table(paste(output_dir, luoutfile, sep="/"), header=T)
  lupar <- read.table(paste(output_dir, lupar_ofile, sep="/"), header=T)
  lupar$slopelength <- luout$x_length
  lupar$soil_depth <- 1000 # groundwater option II.2.1 (WASA documentation)
  lupar$allu_depth <- 3000 # groundwater option II.2.1 (WASA documentation)
  lupar$riverbed_depth <- 1500 # riverbed in any case below soil (no information whether this is reasonable or not)
  lupar$kf_bedrock <- 10
  lupar$frgw_delay <- 20
  write.table(lupar, paste(output_dir, "lu_db_edit.dat", sep="/"), quote = F, row.names = F, sep="\t")

  # copy soil and vegetation parameter files into output_dir
  file.copy(paste(veg_path, "vegetation.dat", sep="/"), paste(output_dir, "vegetation.dat", sep="/"), overwrite=T)
  file.copy(paste(veg_path, "rainy_season.dat", sep="/"), paste(output_dir, "rainy_season.dat", sep="/"), overwrite=T)
  file.copy(paste(soil_path, "soil.dat", sep="/"), paste(output_dir, "soil.dat", sep="/"), overwrite=T)
  file.copy(paste(soil_path, "horizons.dat", sep="/"), paste(output_dir, "horizons.dat", sep="/"), overwrite=T)
  file.copy(paste(soil_path, "particle_classes.dat", sep="/"), paste(output_dir, "particle_classes.dat", sep="/"), overwrite=T)
  file.copy(paste(soil_path, "r_soil_contains_particles.dat", sep="/"), paste(output_dir, "r_soil_contains_particles.dat", sep="/"), overwrite=T)


  
  # DATABASE #
  
  # LINUX ONLY:
  # register database (write into .odbc.ini in your $HOME)
  if(grepl("linux", Sys.info()["sysname"], ignore.case = T)) {
    odbc_dat <- readLines(odbc_file)
    if(any(grepl(paste0("[", dbname, "]"), odbc_dat, fixed = T))) {
      
      if(!overwrite) stop(paste0("Database entry already exists in ", odbc_file, "!"))
      
      # remove entry
      entry <- grep(paste0("[", dbname, "]"), odbc_dat, fixed = T)
      entry_end <- NULL
      for (e in entry) {
        entry_t <- which(odbc_dat[e:length(odbc_dat)] == "")[1]
        entry_end <- c(entry_end, e+entry_t-1)
      }
      r_del <- do.call(c, mapply(seq, entry, entry_end, SIMPLIFY = FALSE))
      odbc_dat <- odbc_dat[-r_del]
      write(odbc_dat, file=odbc_file, ncolumns=1, sep="\n")
    }
    str_odbc <- c(paste0("[", dbname, "]"), # adjust as you like
                  paste0("Description = ", db_desc),
                  paste0("Driver = ", db_driver), # INSTALL AND FILL IN HERE YOUR ODBC DRIVER!
                  paste0("ServerName = ", db_server), # as needed
                  paste0("Database = ",output_dir, "/dbase.db"), # adjust as you like
                  "")
    write(str_odbc, file=odbc_file, ncolumns=1, append=T, sep="\n")
  }
  
  # create database
  db_create(dbname, overwrite = db_overwrite)
  
  # fill database
  db_fill(dbname=dbname,
          tables = c("r_subbas_contains_lu", "subbasins",
                     "landscape_units", "r_lu_contains_tc", "terrain_components", "r_tc_contains_svc",
                     "vegetation", "soils", "horizons", "soil_veg_components",
                     "particle_classes", "r_soil_contains_particles",
                     "rainy_season"),
          dat_files=c(sub_lu, sub_ofile,
                      "lu_db_edit.dat", lucontainstcoutfile, terraincomponentsoutfile, tccontainssvcoutfile,
                      "vegetation.dat", "soil.dat","horizons.dat", svc_file,
                      "particle_classes.dat", "r_soil_contains_particles.dat",
                      "rainy_season.dat"),
          dat_dir=output_dir,
          overwrite=overwrite, verbose= !silent)
  
  # checks
  db_check(dbname, check=db_checks, option=db_option, fix=T, verbose= !silent)
  
  
} # EOF