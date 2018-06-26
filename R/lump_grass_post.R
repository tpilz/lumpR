# lumpR/lump_grass_post.R
# Copyright (C) 2014-2018 Tobias Pilz, Till Francke
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


#' Post-processing of Landscape Unit deviation using GRASS GIS
#' 
#' Creates raster map of Landscape Units and files containing information of
#' and parameter estimation for Subbasins and Landscape Units in the catchment
#' using outputs of \code{\link[lumpR]{lump_grass_prep}} and \code{\link[lumpR]{prof_class}}.
#' 
#' @param mask Name of mask raster map masking the study area. E.g. output \code{basin_out}
#'      of \code{\link[lumpR]{calc_subbas}}.
#' @param dem DEM raster map in GRASS location as used in \code{\link[lumpR]{lump_grass_prep}}.
#' @param subbasin Subbasin raster map in GRASS location as used in \code{\link[lumpR]{lump_grass_prep}}
#'      and/or created by \code{\link[lumpR]{calc_subbas}}.
#' @param recl_lu Name of GRASS reclassification file: EHA -> LU. Output of
#'      \code{\link[lumpR]{prof_class}}. If omitted, the existing raster layer \code{lu} is used.
#' @param lu Input or Output: Name of Landscape Units (LU) raster map already existing in
#'      GRASS location or to be generated using \code{recl_lu}.
#' @param eha Name of Elementary Hillslope Areas (EHA) raster map in GRASS
#'      location. Output of \code{\link[lumpR]{lump_grass_prep}}.
#' @param fill_holes TRUE: fill any holes in map \code{eha} (e.g. as result of skipped/non-classified
#'      EHAs) by growing
#' @param flowacc Name of flow accumulation raster map in GRASS location. Can
#'      be created with \code{\link[lumpR]{lump_grass_prep}}.
#' @param flowdir Name of flow direction raster map in GRASS location. Can
#'      be created with \code{\link[lumpR]{lump_grass_prep}}.
#' @param stream_horton Name of Horton stream order raster map in GRASS location. Can
#'      be created with \code{\link[lumpR]{lump_grass_prep}}. If left empty, the channel length,
#'      slope and retention times are set to \code{NA}.
#' @param soil_depth Name of soil depth [mm] raster map in GRASS location. If \code{NULL}
#'      (default), \code{na_val} is used.
#' @param sdr Name of sediment delivery ratio [-] raster map in GRASS location. If empty,
#'      this optional column is omitted.
#' @param dir_out Character string specifying output directory (will be created;
#'      any overwriting will be prompted).
#' @param sub_ofile Output: Name of subbasin statistics file containing subbasin
#'      parameters. See \code{Details} below. If \code{NULL} (default) this file
#'      will not be generated.
#' @param lu_ofile Output: Name of file containing subbasins and the corresponding
#'      LUs with their fraction of area in the subbasin. If \code{NULL} (default)
#'      this file will not be generated.
#' @param lupar_ofile Output: Name of file containing LUs and related parameters.
#'      See \code{Details} below. If \code{NULL} (default) this file will not be generated.
#' @param groundwater Flag: 1: respect groundwater and infer parameters. 0 (default):
#'      Ignore groundwater and associated parameters.
#' @param na_val Value used to indicate \code{NA} values in output files. Default: \code{NA}.
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#' 
#' @return Function returns nothing. Output files (\code{sub_ofile, lupar_ofile, lu_ofile})
#'      are written into output directory and raster map (\code{lu}) exported into GRASS
#'      location as specified in arguments.
#'      
#' @note Prepare GRASS location and necessary raster files in advance (e.g. using
#'      \code{\link[lumpR]{lump_grass_prep}}) and start GRASS session in R using 
#'      \code{\link[rgrass7]{initGRASS}}.
#'      
#'      TODO:\cr
#'        - check empirical formulas for channel width and channel depth\cr
#'        - LU parameter estimation\cr
#'        - include options to add parameters manually in case data are available\cr
#'        - include option to make function more efficient regarding RAM usage (e.g. by
#'          wrting/reading temporary raster data to/from disk) at the cost of higher computational burden
#'        
#'        
#' @details
#'      \bold{Subbasin parameters}\cr
#'      Subbasin parameter estimation given in \code{sub_ofile} contains:
#'      
#'      \emph{pid}\cr
#'      Subbasin identifier.
#'      
#'      \emph{description}\cr
#'      Subbasin description you can fill in manually if needed.
#'      
#'      \emph{lat}\cr
#'      Latitude of subbasin centroid in \emph{decimal degrees} (negative values for southern hemisphere).
#'      
#'      \emph{lon}\cr
#'      Longitude of subbasin centroid in \emph{decimal degrees west of Greenwhich}, e.g.
#'      Greenwich: 0°, New York: 75°, Berlin: 345°.
#'      
#'      \emph{elev}\cr
#'      Average elevation above sea level of subbasin \emph{m}.
#'      
#'      \emph{area}\cr
#'      Subbasin area in \emph{km^2}.
#'      
#'      \emph{x}\cr
#'      X-coordinate of subbasin centroid in units of GRASS location.
#'      
#'      \emph{y}\cr
#'      Y-coordinate of subbasin centroid in units of GRASS location.
#'      
#'      \emph{drains_to}\cr
#'      Pid of subbasin the current subbasin drains to. It is determined by identifying
#'      the cell with the highest flow accumulation value (= subbasin outlet). By
#'      deriving the drainage direction value of that raster cell the next downstream
#'      cell is identified along with corresponding subbasin number.
#'      
#'      \emph{lag_time}\cr
#'      Time in \emph{days} a runoff signal in the current subbasin needs to be directed
#'      from the subbasin input to the outlet. Estimated from channel geometry (see below).
#'      
#'      \emph{retention}\cr
#'      Maximum time period in \emph{days} over which a runoff signal is distributed by
#'      the routing process. Estimated from channel geometry (see below).
#'      
#'      \emph{chan_len}\cr
#'      Length of the main channel of the respective subbasin in \emph{m}. Estimated
#'      from channel geometry (see below).
#'      
#'      \emph{channel geometry}\cr
#'      Main channel length: For each subbasin the main channel is determined from Horton
#'      stream raster map. Its length is then calculated depending on raster resolution
#'      and flow direction.\cr
#'      Channel slope: Minimum (= inflow) and maximum (= outflow) flow accumulation and
#'      elevation of the corresponding raster cells are determined. The difference in
#'      elevation is divided by main channel length.\cr
#'      Channel width: Maximum flow accumulation is determined and the corresponding
#'      drainage area calculated by resolution of raster cells. Channel width is then
#'      calculated from the empirical formula: width[m] = 1.29 * darea[km2] ^ 0.6.\cr
#'      Channel depth: Empirical formula based on drainage area: depth[m] = 0.13 * darea[km2] ^ 0.4.\cr
#'      Flow velocity: Is calculated using Mannings equation with an n-value of 0.075.
#'      Flow velocities are calculated to derive flow travel times (velocity / channel
#'      length) for bankfull (= high flow condition), 2/3 (= average conditions) and
#'      1/10 (= low flow conditions) water levels to derive lag time (travel time for
#'      average conditions) and retention time (max - min travel time).
#'      
#'      
#'      \bold{Landscape Units in Subbasins}\cr
#'      \code{lu_ofile} contains:
#'            
#'      \emph{subbas_id}\cr
#'      Subbasin identifier.
#'      
#'      \emph{lu_id}\cr
#'      Landscape Unit identifier.
#'      
#'      \emph{fraction}\cr
#'      Areal fraction of Landscape Unit within corresponding Subbasin.
#'      
#'      
#'      \bold{Landscape Unit parameters}\cr
#'      Landscape Unit parameter estimation given in \code{lupar_ofile} contains:
#'      
#'      \emph{pid}\cr
#'      Landscape Unit identifier.
#'      
#'      \emph{description}\cr
#'      Description for this Landscape Unit. Can be adjusted manually if you want.
#'      Generally set to \code{na_val}.
#'      
#'      \emph{kf_bedrock}\cr
#'      Hydraulic conductivity of bedrock. Fill in values manually. Generally set
#'      to \code{na_val}. Use in WASA model is optional (see WASA documentation
#'      -> 'kfsu' and notes on 'bedrock').
#'      
#'      \emph{slopelength}\cr
#'      Slope length of Landscape Unit [m]. Value can be obtained from output of
#'      \code{\link[lumpR]{prof_class}}. Herein set to \code{na_val}.
#'      
#'      \emph{soil_depth}\cr
#'      Soil depth in \emph{mm} averaged over respective landscape unit. Use in
#'      WASA model is optional (see WASA documentation -> 'meandep' and notes on
#'      'bedrock'). Herein set to \code{na_val}.
#'      
#'      \emph{allu_depth}\cr
#'      Depth of alluvial soils in \emph{mm}. Use in WASA model is optional (see
#'      WASA documentation -> 'maxdep' and notes on 'bedrock'). Herein set to \code{na_val}.
#'      
#'      \emph{riverbed_depth}\cr
#'      Depth of river bed below Terrain Component in \emph{mm}. Use in WASA model
#'      is optional (see WASA documentation -> 'riverbed' and notes on 'bedrock').
#'      Herein set to \code{na_val}.
#'      
#'      \emph{gw_flag}\cr
#'      Groundwater flag: 0: no groundwater in this LU. 1: LU contains groundwater.
#'      At the moment set to 0 by default for every LU. Use in WASA model is optional
#'      (see WASA documentation -> 'gw_flag' and notes on groundwater).
#'      
#'      \emph{gw_dist}\cr
#'      Initial depth of groundwater below surface in \emph{mm}. At the moment set to
#'      1000 by default for every LU if \code{groundwater = 1}. Use in WASA model is optional
#'      (see WASA documentation -> 'gw_dist' and notes on groundwater).
#'      
#'      \emph{frgw_delay}\cr
#'      Storage coefficient for groundwater outflow in \emph{days}. At the moment set to
#'      200 by default for every LU if \code{groundwater = 1}. Use in WASA model is optional
#'      (see WASA documentation -> 'frgw_delay' and notes on groundwater).
#'      
#'      \emph{sdr_lu} (optional)\cr
#'      sediment delivery ratio from raster map \code{sdr}, if specified
#'      
#'      
#' @references Source code based on \code{SHELL} and \code{MATLAB} scripts of Till Francke.
#' 
#'      lumpR package introduction with literature study and sensitivity analysis:\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#' 
#'      Theory of LUMP:\cr
#'      Francke, T.; Guentner, A.; Mamede, G.; Mueller, E. N. and Bronstert, A (2008):
#'      Automated catena-based discretization of landscapes for the derivation of
#'      hydrological modelling units. \emph{International Journal of Geographical
#'      Information Science, Informa UK Limited}, 22(2), 111-132, DOI: 10.1080/13658810701300873
#'      
#'      Subbasin Parameters:\cr
#'      Bronstert, A., Guentner, A., Jaeger, A., Krol, M. & Krywkow, J. (1999): Grossraeumige
#'      hydrologische Parametrisierung und Modellierung als Teil der integrierten 
#'      Modellierung. In: Fohrer, N. & Doell, P. (Eds.): Modellierung des Wasser- und
#'      Stofftransports in grossen Einzugsgebieten. \emph{Kassel University Press}, Kassel,
#'      Germany, 31-40.
#'      
#'      Guentner, A. (2002): Large-scale hydrological modelling in the semi-arid 
#'      North-East of Brazil. \emph{PIK Report 77}, Potsdam Institute for Climate
#'      Impact Research, Potsdam, Germany.
#'      

#' 
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}

lump_grass_post <- function(
  ### INPUT ###
  mask=NULL,
  dem=NULL,
  recl_lu=NULL,
  lu=NULL,
  subbasin=NULL,
  eha=NULL,
  flowacc=NULL,
  flowdir=NULL,
  stream_horton=NULL,
  soil_depth=NULL,
  sdr=NULL,
  
  ### OUTPUT ###
  dir_out="./",
  sub_ofile=NULL,
  lu_ofile=NULL,
  lupar_ofile=NULL,
  
  ### OPTIONS ###
  fill_holes=T,
  groundwater=0,
  na_val=NA,
  keep_temp=F,
  overwrite=F,
  silent=F
  
) {
  
  
### PREPROCESSING ###----------------------------------------------------------
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START lump_grass_post()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")
  
# checks #---------------------------------------------------------------------
  tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  if(is.null(mask))
    stop("The name of a raster within the mapset of your initialised GRASS session to be used as catchment MASK in GRASS has to be given!")
  if(!is.character(mask))
    stop("The name of a raster to be used as catchment MASK in GRASS must be a string (argument 'mask')!")
  if(is.null(dem))
    stop("The name of a DEM within the mapset of your initialised GRASS session has to be given!")
  if(is.null(subbasin))
    stop("A name for needed subbasin raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(eha))
    stop("A name for needed EHA raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(flowdir))
    stop("A name for needed flow direction raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(flowacc))
    stop("A name for needed flow accumulation raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(lu))
    stop("A name for the landscape units raster map to be generated (recl_lu = NULL) or already within the mapset of your initialised GRASS session has to be given")
  if(!is.null(recl_lu) && !file.exists(recl_lu))
    stop(paste0("Could not find file '",recl_lu,"' (argument 'recl_lu')"))
  
  
  # suppress annoying GRASS outputs
  tmp_file <- file(tempfile(), open="wt")
  sink(tmp_file, type="output")
  
  # also supress warnings in silent mode
  if(silent){
    tmp_file2 <- file(tempfile(), open="wt")
    sink(tmp_file2, type="message")
    oldw <- getOption("warn")
    options(warn = -1)
  }
  
  # create output dir
  dir.create(dir_out, recursive=T, showWarnings=F)
  
  # check output directory
  if (!overwrite & (file.exists(paste(dir_out,sub_ofile,sep="/")) |
                    file.exists(paste(dir_out,lu_ofile,sep="/")) |
                    file.exists(paste(dir_out,lupar_ofile,sep="/"))) )
    stop(paste0("Output file(s) ", sub_ofile, ", ",lu_ofile, ", and/or ", lupar_ofile, " already exist(s) in ", dir_out, "!"))
  
  # remove mask if there is any (and ignore error in case there is no mask)
  tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
  cmd_out <- execGRASS("r.mask", raster=mask, intern = T)
  
  # remove output of previous function calls if overwrite=T
  if (overwrite) {
    cmd_out <- execGRASS("g.remove", type="raster", pattern=paste("*_t,", lu, sep=","), flags=c("f", "b"), intern=T)
  } else {
    # remove temporary maps in any case
    cmd_out <- execGRASS("g.remove", type="raster", pattern="*_t", flags=c("f", "b"), intern=T)
  }
  

  if(!silent) message("% OK")
  
  
  
### CALCULATIONS ###-----------------------------------------------------------
  
# prepare output directory, read GRASS data #-----------------------------
  tryCatch({
    if(!silent) message("%")
    if(!silent) message("% Prepare output and import GRASS data...")
    
    #check existence of raster maps
    check_raster(mask,"mask")
    check_raster(flowacc,"flowacc")
    check_raster(flowdir,"flowdir")
    check_raster(subbasin,"subbasin")
    check_raster(stream_horton,"stream_horton")
    
    # load rasters into R
    dem_rast <- read_raster(dem)
    accum_rast <- read_raster(flowacc)
    dir_rast <- read_raster(flowdir)
    cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
    
    sub_rast <- read_raster(subbasin)
    horton_rast <- read_raster(stream_horton)
    # ... raster values as matrix
    dem_mat <- getValues(dem_rast, format="matrix")
    rm(dem_rast)
    sub_mat <- getValues(sub_rast, format="matrix")
    coords <- xyFromCell(sub_rast, 1:length(sub_rast))
    rm(sub_rast)
    accum_mat <- getValues(accum_rast, format="matrix")
    rm(accum_rast)
    dir_mat <- getValues(dir_rast, format="matrix")
    rm(dir_rast)
    horton_mat <- getValues(horton_rast, format="matrix")
    rm(horton_rast)
    gc(); gc()
    
    # resolution
    g_meta <- gmeta()
    resol <- mean(g_meta$nsres, g_meta$ewres)
    
    if(!silent) message("% OK")
    
  }, error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    # remove mask
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    stop(paste(e))
  })
  
  
  
# generate lu raster #---------------------------------------------------------
  tryCatch({
    if(!silent) message("%")
      
    if (is.null(recl_lu)) #use existing LU-map
    {
      if(!silent) message(paste0("% NOTE: Use existing raster map ", lu, " as specified."))
      
    } else {
      
      if(!silent) message(paste0("% Generate landscape units raster map '", lu, "'..."))
      
      #reclass EHA according to reclass file generated by prof_class to get LU
      cmd_out=execGRASS("r.reclass", input=eha, output=lu, rules=recl_lu, intern = TRUE)
      if(!is.null(attr(cmd_out, "status")))
         stop(paste0("Could not reclass '",eha,"' into '",lu,"'"))

      # growing radius (parameter for r.grow)
      GROWRAD <- 20

      if (fill_holes)        
      while (TRUE)
      {        
        # look for empty patches
        cmd_out=execGRASS("r.mapcalc", expression=paste0("grow_eval_t = if(isnull(", lu, "), 1, 0)"), flags = "overwrite", intern=TRUE)
        
        na_eval <- execGRASS("r.stats", input=paste0("grow_eval_t"), flags=c("n", "p"), intern=TRUE, ignore.stderr = T)
        
        if (length(na_eval)==0 ) break #no empty patches found  
        
        # if (grepl(pattern="[0-9]+.*[\b]+",x=tail(na_eval, n=1)))
        #   na_eval = na_eval[-length(na_eval)] #last line contains progress indicator, remove
         
        na_line=grepl(na_eval, pattern = "^1 .*\\%$")
        
        if (!any(na_line)) break #no empty patches found  
        
        na_res <- strsplit(x=na_eval[na_line], split=" +")  
        if(!silent) message(paste0("% -> ", tail(unlist(na_res),1), " NAs in LU-map '", lu,"'"))
        
        # otherwise grow LU map to fill gaps 
        if(!silent) message(paste0("% -> Filling empty patches in ",lu,", iterating..."))
        cmd_out=execGRASS("r.grow",   input=lu, output="lu_t", radius=GROWRAD, flags="overwrite", intern=TRUE)
        if(!is.null(attr(cmd_out, "status")))
          stop(paste0("Could not grow '",lu,"'"))
        
        # r.grow converts type CELL to type DCELL; convert back to CELL
        cmd_out <- execGRASS("r.mapcalc", expression=paste0(lu," = int(lu_t)"), flags = "overwrite", intern = T)
        
        cmd_out <- execGRASS("g.remove", type="raster", pattern="lu_t", flags=c("f", "b"), intern=T)
        # x=execGRASS("r.mapcalc", expression = paste0(lu, " = ", mask, " * tt_eha_grown"), flags="overwrite", intern=TRUE)
        # 
        # x=execGRASS("g.remove", type="raster", name="tt_eha_grown", intern=TRUE)
      } # while true (grow-loop)
      
      cmd_out=execGRASS("g.remove", type="raster", name="grow_eval_t", intern=TRUE)
      
      if(!silent) message("% OK")
      
    } # recl_lu was given
    
    
  },
  error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    # remove mask
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    # remove output so far
    if(!keep_temp)
      x=execGRASS("g.remove", type="raster", pattern=paste0("grow_eval_t,", lu), flags=c("f", "b"), intern=TRUE)
    
    stop(paste(e))
  }
  )
  
    
    
# subbasin statistics #--------------------------------------------------------
  tryCatch({
    if(!is.null(sub_ofile)) {
      if(!silent) message("%")
      if(!silent) message("% Calculate subbasin statistics...")
      
      sub_stats <- execGRASS("r.stats", input=subbasin, flags=c("a", "n"), intern=TRUE, ignore.stderr = T)
      if (grepl(pattern="[0-9]+.*[\b]+",x=tail(sub_stats, n=1)))
        sub_stats <- sub_stats[-length(sub_stats)] #last line contains progress indicator, remove
      
      sub_stats <- matrix(as.numeric(gsub("%", "", unlist(strsplit(sub_stats, split=" +")))), ncol=2, byrow=T,
                          dimnames=list(NULL,c("pid", "area")))
      
      # convert m2 to km2
      sub_stats[,"area"] <- round(sub_stats[,"area"]/1e6, 2)
        
      # calculate stats of LUs in each subbasin and subbasin drainage ("drains_to")
      sub_stats <- cbind(sub_stats, na_val, na_val, na_val, na_val, na_val, na_val, na_val, na_val, na_val, na_val, na_val)
      colnames(sub_stats)[c(3:13)] <- c("x", "y", "lon", "lat", "elev", "drains_to", "lag_time", "retention", "description", "a_stream_order", "chan_len")
      sub_lu_stats <- NULL
      s_row <- 0
      for (SUB in sub_stats[,1]) {
        
        s_row <- s_row +1
        
        # bounding box of current subbasin
        sub_arrind <- which(sub_mat == SUB, arr.ind = T)
        rmin <- min(sub_arrind[,"row"])
        rmax <- max(sub_arrind[,"row"])
        cmin <- min(sub_arrind[,"col"])
        cmax <- max(sub_arrind[,"col"])
        
        # crop raster matrices
        dem_crop <- dem_mat[rmin:rmax, cmin:cmax]
        horton_crop <- horton_mat[rmin:rmax, cmin:cmax]
        sub_crop <- sub_mat[rmin:rmax, cmin:cmax]
        dir_crop <- dir_mat[rmin:rmax, cmin:cmax]
        accum_crop <- accum_mat[rmin:rmax, cmin:cmax]
        
        # get coordinates of cells
        coords_crop <- coords[which(t(sub_mat) == SUB),]
        
        # set all values not overlapping with the current subbasin to NA
        na_cells <- which(sub_crop != SUB)
        sub_crop[na_cells] <- NA
        dem_crop[na_cells] <- NA
        horton_crop[na_cells] <- NA
        dir_crop[na_cells] <- NA
        accum_crop[na_cells] <- NA
        
    # COORDINATES OF SUBBASIN centroids in GRASS units #
        sub_centr <- centroid(coords_crop)
        
        sub_stats[s_row, "x"] <- round(sub_centr[,"x"])
        sub_stats[s_row, "y"] <- round(sub_centr[,"y"])
        
    		# convert to decimal degree
        sub_centr <- data.frame(x = sub_centr[,"x"], y = sub_centr[,"y"])
        coordinates(sub_centr) <- c("x", "y")
        proj4string(sub_centr) <- getLocationProj()
    		longlat <- spTransform(sub_centr, "+proj=longlat")
    		sub_stats[s_row, "lon"] <- round(coordinates(longlat)[,"x"], 4)
    		sub_stats[s_row, "lat"] <- round(coordinates(longlat)[,"y"], 4)
    		
    		# longitude as decimal degrees west of Greenwich
    		if(sub_stats[s_row, "lon"] < 0)
    			sub_stats[s_row, "lon"] <- -1*sub_stats[s_row, "lon"]
    		else
    			sub_stats[s_row, "lon"] <- 360 - sub_stats[s_row, "lon"]
  			
  	# AVERAGE SUBBASIN ELEVATION #
    		sub_stats[s_row, "elev"] <- round(mean(dem_crop, na.rm=T))
        
    # SUBBASIN drainage #
        sub_stats[s_row,"drains_to"] <- sub_route(SUB,sub_mat,accum_mat,dir_mat) # internal function, see below
    
    # SUBBASIN PARAMETERS #
        # calc main channel length
        chan_len <- channel_length(horton_crop, dir_crop, resol)
        
        # calc main channel average slope
        chan_slope <- channel_slope(horton_crop, accum_crop, dem_crop, chan_len)
    
        # calc main channel width
        chan_width <- channel_width(accum_crop, resol)
    
        # calc main channel depth
        chan_depth <- channel_depth(accum_crop, resol)
    
        # calc flow velocites for bankful, 2/3 and 1/10 filling
        # Manning's n = 0.075 (very weedy reaches, deep pools, or floodways with heavy stand of timber and underbrush) 
        # http://www.fsl.orst.edu/geowater/FX3/help/8_Hydraulic_Reference/Mannings_n_Tables.htm
        chan_velo_full <- flow_velocity(chan_width, chan_depth, chan_slope, n=0.075)
        chan_velo_med <- flow_velocity(chan_width, 2/3 * chan_depth, chan_slope, n=0.075)
        chan_velo_low <- flow_velocity(chan_width, 1/10 * chan_depth, chan_slope, n=0.075)
    
        # calc travel times (translation and retention) [d]
        # approach according to Bronstert et al. (1999), Guentner (2002)
        flowtime_min <- chan_len / chan_velo_full / 86400
        flowtime_med <- chan_len / chan_velo_med / 86400
        flowtime_max <- chan_len / chan_velo_low / 86400
        
        # check results
        retention <- flowtime_max - flowtime_min
        
        if(!is.finite(flowtime_med) || !is.finite(retention))
          warning(paste0("Could not calculate finite subbasin parameters for subbasin ", SUB))
    
        # save
        sub_stats[s_row, "lag_time"] <- round(flowtime_med, 3)
        sub_stats[s_row, "retention"] <- round(retention, 3)
        sub_stats[s_row, "chan_len"] <- round(chan_len)
      } # subbasin loop
      
      # write output file
      write.table(sub_stats, paste(dir_out, sub_ofile, sep="/"), quote=F, row.names=F, sep="\t")
      
      if(!silent) message("% OK")
      
    } # sub_ofile given?
  
  
  }, error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    # remove mask
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    # remove output so far
    if(!keep_temp)
      x=execGRASS("g.remove", type="raster", pattern=paste0("*_t,", lu), flags=c("f", "b"), intern=TRUE)
    
    stop(paste(e))
  })



# lu statistics #--------------------------------------------------------------
  tryCatch({
    if(!is.null(lu_ofile)) {
      if(!silent) message("%")
      if(!silent) message("% Calculate LU statistics...")
      
      sub_lu_stats_t <- execGRASS("r.stats", input=paste0(subbasin,",",lu), flags=c("n", "c"), intern=TRUE, ignore.stderr = T)
      if(!is.null(attr(sub_lu_stats_t, "status")))
        stop(paste0("Could not access lu-map '",lu,"'."))
      
      if (grepl(pattern="[0-9]+.*[\b]+",x=tail(sub_lu_stats_t, n=1))) #check if last line contains progress indicator, remove
      sub_lu_stats_t = sub_lu_stats_t[-length(sub_lu_stats_t)] 
      
      sub_lu_stats_t = sub_lu_stats_t[grepl(x=sub_lu_stats_t, pattern = "^[0-9]")]  #remove non-number lines
      sub_lu_stats_t = as.numeric(gsub("%", "", unlist(strsplit(sub_lu_stats_t, split=" +")))) #convert to numbers and remove %
      sub_lu_stats_t2 =data.frame(matrix(sub_lu_stats_t, ncol=3, byrow=T))
      names(sub_lu_stats_t2)=c("subbas_id","lu_id","cells")
      subbasin_cells=aggregate(x = sub_lu_stats_t2$cells, by = list(subbas_id=sub_lu_stats_t2$subbas_id), FUN=sum)
      
      sub_lu_stats_t2=merge(sub_lu_stats_t2,subbasin_cells)
      sub_lu_stats_t2$fraction=sub_lu_stats_t2$cells / sub_lu_stats_t2$x #compute fraction
      write.table(sub_lu_stats_t2[,c("subbas_id", "lu_id", "fraction")], paste(dir_out, lu_ofile, sep="/"), quote=F, row.names=F, sep="\t")
      
      if(!silent) message("% OK")
      
    } # lu_ofile given?
    
  }, error = function(e) {
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    # remove mask
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    # remove output so far
    if(!keep_temp)
      x=execGRASS("g.remove", type="raster", pattern=paste0("*_t,", lu), flags=c("f", "b"), intern=TRUE)
    
    stop(paste(e))
  })




    
# lu parameters #--------------------------------------------------------------
  tryCatch({
    if(!is.null(lupar_ofile)) {
      if(!silent) message("%")
      if(!silent) message("% Calculate LU parameters")
       
      # identify LUs
      lu_ids <- execGRASS("r.stats", input=lu, flags=c("n"), intern=TRUE, ignore.stderr = T)
      if (grepl(pattern="[0-9]+.*[\b]+",x=tail(lu_ids, n=1)))
        lu_ids <- as.numeric(lu_ids[-length(lu_ids)]) #last line contains progress indicator, remove
      
      # initialise LU parameter output object
      lu_par <- matrix(na_val, ncol=11, nrow=length(lu_ids), 
                       dimnames=list(NULL, c("pid", "description", "kf_bedrock", "slopelength",
                                             "soil_depth", "allu_depth", "riverbed_depth",
                                             "gw_flag", "gw_dist", "frgw_delay", "sdr_lu")))
      
      # calculate mean soil depth for every LU
      if (!is.null(soil_depth) && soil_depth!="")
        {
          cmd_out <- execGRASS("r.univar", zones=lu, map=soil_depth, fs=",", flags=c("t"),intern=T, ignore.stderr = T)
          if (grepl(pattern="[0-9]+.*[\b]+",x=tail(cmd_out, n=1)))
            cmd_out <- cmd_out[-length(cmd_out)] #last line contains progress indicator, remove
          cmd_out <- strsplit(cmd_out, ",")
          cmd_out <- matrix(unlist(cmd_out[-1]), ncol=length(cmd_out[[1]]), byrow=T,
                            dimnames=list(NULL, cmd_out[[1]]))
          lu_depth <- as.numeric(cmd_out[,"mean"])
          lu_ids = as.numeric(cmd_out[,"zone"])
          # quick check of soil depths
          if(any(lu_depth > 10000))
            warning("There are average LU soil depths of more than 10 m which is a large (but not impossible) value. Check your input data and units ('soil_depth' in [mm])!")
          if(any(lu_depth < 100))
            warning("There are average LU soil depths of less than 10 cm which is a small (but not impossible) value. Check your input data and units ('soil_depth' in [mm])!")
        } else lu_depth=na_val
      
      lu_par[,"pid"] <- lu_ids
      lu_par[,"soil_depth"] <- lu_depth
    
      
      if (!is.null(sdr) && sdr!="")
      {
        cmd_out <- execGRASS("r.univar", zones=lu, map=sdr, fs=",", flags=c("t"),intern=T, ignore.stderr = T)
        if (grepl(pattern="[0-9]+.*[\b]+",x=tail(cmd_out, n=1)))
          cmd_out <- cmd_out[-length(cmd_out)] #last line contains progress indicator, remove
        cmd_out <- strsplit(cmd_out, ",")
        cmd_out <- matrix(unlist(cmd_out[-1]), ncol=length(cmd_out[[1]]), byrow=T,
                          dimnames=list(NULL, cmd_out[[1]]))
        sdr_vals <- as.numeric(cmd_out[,"mean"]) 
        lu_ids = as.numeric(cmd_out[,"zone"])
        sdr_lu <- sdr_vals[match(lu_par[,"pid"], lu_ids)]
        lu_par[,"sdr_lu"] <- sdr_lu
      }
      
    
      # groundwater parameters (so far only default values)
      # TODO: alternative approaches?
      # groundwater for every LU
      if(groundwater) {
        lu_par[,"gw_flag"] <- rep(1, length(lu_depth))
        # initial depth of groundwater below surface: 1000 mm
        lu_par[,"gw_dist"] <- rep(1000, length(lu_depth))
        # storage coefficient for groundwater outflow [days]; TODO: estimate from baseflow analysis?!
        lu_par[,"frgw_delay"] <- rep(200, length(lu_depth))
      } else {
        lu_par[,"gw_flag"] <- rep(0, length(lu_depth))
        # initial depth of groundwater below surface: 1000 mm
        lu_par[,"gw_dist"] <- rep(na_val, length(lu_depth))
        # storage coefficient for groundwater outflow [days]; TODO: estimate from baseflow analysis?!
        lu_par[,"frgw_delay"] <- rep(na_val, length(lu_depth))
      }
    
    
      # write output
      write.table(lu_par, paste(dir_out, lupar_ofile, sep="/"), quote=F, row.names=F, sep="\t")
      
      if(!silent) message("% OK")
    
    } # lupar_ofile fiven?

  }, error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    # remove mask
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    # remove output so far
    if(!keep_temp)
      x=execGRASS("g.remove", type="raster", pattern=paste0("*_t,", lu), flags=c("f", "b"), intern=TRUE)
    
    stop(paste(e))
  })
  
  
  
  
  
  # remove temp files
  if(!keep_temp)
    x=execGRASS("g.remove", type="raster", pattern="*_t", flags=c("f", "b"), intern=TRUE)
  
  if(!silent) message("%")
  if(!silent) message("% DONE!")
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
  
  # stop sinking
  closeAllConnections()
  
  # restore original warning mode
  if(silent)
    options(warn = oldw)
  

} # EOF




### INTERNAL FUNCTIONS ###-----------------------------------------------------

# subbasin centroids #---------------------------------------------------------
# returns a matrix with x and y value of the centroid of the object given in coords_sub
centroid <- function(coords_sub) {
  # convert so sp class
  coords_sub <- as.data.frame(coords_sub)
  coordinates(coords_sub) <- c("x", "y")
  
  # calculate centroid
  centr <- gCentroid(coords_sub)
  centr <- centr@coords
  return(centr)
}

# subbasin routing #-----------------------------------------------------------
# returns ID of downstream subbasin for the current subbasin 'sub_no'
# determined from flow accumulation and flow direction map
sub_route <- function(sub_no,sub_mat,accum_mat,dir_mat) {
  
  # extract highest flowacc in subbasin sub_no
  sub_ids <- which(sub_mat == sub_no)
  accum_sub <- accum_mat[sub_ids]
  accum_sub_max <- which.max(accum_sub)
  
  # extract corresp. flowdir
  dir_sub_out <- dir_mat[sub_ids[accum_sub_max]]
  
  # determine row and col no in sub_mat
  sub_rowcol <- arrayInd(sub_ids[accum_sub_max], dim(sub_mat))

  # determine value of sub_mat (subbasin no.) of neighbour cell according to dir_sub_out
  if (dir_sub_out < 0) { # outlet of catchment
    return(9999)
    
  } else {
    
    if (dir_sub_out == 1) { # NE
      sub_rowcol_out <- sub_rowcol + c(-1,1)
    } else if (dir_sub_out == 2) { # N
      sub_rowcol_out <- sub_rowcol + c(-1,0)
    } else if (dir_sub_out == 3) { # NW
      sub_rowcol_out <- sub_rowcol + c(-1,-1)
    } else if (dir_sub_out == 4) { # W
      sub_rowcol_out <- sub_rowcol + c(0,-1)
    } else if (dir_sub_out == 5) { # SW
      sub_rowcol_out <- sub_rowcol + c(+1,-1)
    } else if (dir_sub_out == 6) { # S
      sub_rowcol_out <- sub_rowcol + c(+1,0)
    } else if (dir_sub_out == 7) { # SE
      sub_rowcol_out <- sub_rowcol + c(+1,+1)
    } else if (dir_sub_out == 8) { # E
      sub_rowcol_out <- sub_rowcol + c(0,+1)  
    } else {
      stop(paste("While determining subbasin drainage: Determined flow direction at outlet
              of subbasin ", sub_no, " has value ", dir_sub_out, " but should be one of {1,2,3,4,5,6,7,8} or a negative number.", sep=""))
    }
    
    # determine subbasin the current subbasin drains to
    sub_out <- sub_mat[sub_rowcol_out]
    
    # update stats table
    if (is.na(sub_out)) { # catchment outlet
      return(9999)
    } else {
      if (sub_out == sub_no)
        stop(paste0("In subbasin no. ", sub_no, " the determined downstream subbasin is this subbasin!"))
      
      return(sub_out)
    }  
  } 
} # EOF


# main channel length #--------------------------------------------------------
# returns length of the main channel (largest value in Horton order) in [m]
# computes main stream temporary raster used in further calculations
channel_length <- function(horton, flowdir, resol) {
  
  # determine main channel (largest value in Horton stream order)
  main_chan <- na.exclude(unique(c(horton)))
  
  # if there is no main channel (e.g. in very small reservoir subbasins) assume one cell of main stream
  if (length(main_chan) == 0) {
    chan_len <- resol
    # accmax <- max(as.numeric(execGRASS("r.stats", input=flowacc, flags=c("n", "1"), intern=T, ignore.stderr = T))) # max flowacc
    # execGRASS("r.mapcalculator", amap=flowacc, outfile="stream_main_t", 
    #           formula=paste0("if(A == ", sprintf(accmax, fmt="%d"), ", 1, null())"))
    warning(paste("Subbasin ", sub_no, " has no main channel. Assume at least one raster cell.", sep=""))

  } else {
    
    # diagonal raster cell length
    dia <- sqrt(2*resol^2)
    
    # determine raster cells of main channel
    max_val <- max(main_chan)
    r_main <- which(horton==max_val)
    
    # calculate length of the main channel
    chanlen_t <- flowdir[r_main] %% 2 == 0 # horizontal/vertical stream
    chan_len <- length(which(chanlen_t))*resol + length(which(!chanlen_t))*dia
  }
  
  return(chan_len)
} # EOF


# main channel slope #---------------------------------------------------------
# returns average slope of main channel [m/m]
channel_slope <- function(horton, accum, dem, chan_len) {  
  if (is.na(chan_len) ) return(NA)

  # determine main channel (largest value in Horton stream order)
  main_chan <- na.exclude(unique(c(horton)))
  
  # determine raster cells of main channel
  max_val <- max(main_chan)
  r_main <- which(horton==max_val)
  
  # min and max values of flow accumulation within main channel to determin flow direction
  acc_min <- which.min(accum[r_main])
  acc_max <- which.max(accum[r_main])
  
  # get dem values for respective raster cells of min and max flowacc
  dem_vals <- dem[r_main][c(acc_max, acc_min)]
  
  # compute slope [m/m]
  chan_slope <- diff(dem_vals) / chan_len
  
  # slope zero not allowed -> at least 0.00001
  chan_slope <- max(chan_slope, 0.00001)
  
  return(chan_slope)
}


# main channel width #---------------------------------------------------------
# calculate main channel width based on empirical formula: width[m] = 1.29 * darea[km2] ^ 0.6
# darea = drainage area determined from maximum flow accumulation and resolution
channel_width <- function(accum, resol) {
  # determine maximum flow accumulation
  max_acc <- max(accum, na.rm = T)
  
  # calculate drainage area [km^2]
  drain_area <- max_acc * resol^2 / 1e6
  
  # calculate width according to empirical function
  chan_width <- 1.29 * drain_area^(0.6)
  
  if(chan_width > 3000)
    stop(paste("Calculated channel width is ", chan_width, "m which seems unrealistic!", sep=""))
  
  return(chan_width)
}


# main channel depth #---------------------------------------------------------
# calculate main channel width based on empirical formula: width[m] = 0.13 * darea[km2] ^ 0.4
# darea = drainage area determined from maximum flow accumulation and resolution
channel_depth <- function(accum, resol) {
  # determine maximum flow accumulation
  max_acc <- max(accum, na.rm = T)
  
  # calculate drainage area [km^2]
  drain_area <- max_acc * resol^2 / 1e6
  
  # calculate depth according to empirical function
  chan_depth <- 0.13 * drain_area^(0.4)
  
  if(chan_depth > 50)
    stop(paste("Calculated channel depth is ", chan_depth, "m which seems unrealistic!", sep=""))
  
  return(chan_depth)
}


# flow velocity #--------------------------------------------------------------
# function returns flow velocity of main channel [m/s]
# calc by Gauckler-Manning-Strickler
# assume simple rectangle cross section as width >> depth for main channels
flow_velocity <- function(chan_width, chan_depth, chan_slope, n) {
  # calculate cross section area (assume simple rectangle as width >> depth for main channels)
  cross_area <- chan_width * chan_depth
  
  # calculate hydraulic radius (wetted perimeter = width + 2 * depth)
  hyd_radius <- cross_area / (chan_width + 2 * chan_depth)
  
  # calculate flow velocity
  chan_velo <- 1/n * hyd_radius^(2/3) * chan_slope^(1/2)
  
  return(chan_velo)
}

