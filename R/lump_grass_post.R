#' Post-processing of Landscape Unit deviation using GRASS GIS
#' 
#' Creates raster map of Landscape Units and files containing information of
#' and parameter estimation for Subbasins and Landscape Units in the catchment
#' using outputs of \code{\link[LUMP]{lump_grass_prep}} and \code{\link[LUMP]{prof_class}}.
#' 
#' @param dem DEM raster map in GRASS location as used in \code{\link[LUMP]{lump_grass_prep}}.
#' @param subbasin Subbasin raster map in GRASS location as used in \code{\link[LUMP]{lump_grass_prep}}
#'      and/or created by \code{\link[LUMP]{calc_subbas}}.
#' @param recl_lu Name of GRASS reclassification file: EHA -> LU. Output of
#'      \code{\link[LUMP]{prof_class}}.
#' @param eha Name of Elementary Hillslope Areas (EHA) raster map in GRASS
#'      location. Output of \code{\link[LUMP]{lump_grass_prep}}.
#' @param mask Name of mask raster map masking the study area. Use output \code{mask_corr}
#'      of \code{\link[LUMP]{lump_grass_prep}}.
#' @param flowacc Name of flow accumulation raster map in GRASS location. Can
#'      be created with \code{\link[LUMP]{lump_grass_prep}}.
#' @param flowdir Name of flow direction raster map in GRASS location. Can
#'      be created with \code{\link[LUMP]{lump_grass_prep}}.
#' @param stream_horton Name of Horton stream order raster map in GRASS location. Can
#'      be created with \code{\link[LUMP]{lump_grass_prep}}. If left empty, the channel length, slope and retention times are set to NA.
#' @param soil_depth Name of soil depth [cm] raster map in GRASS location. If empty, NA is used.
#' @param sdr Name of sediment delivery ratio [-] raster map in GRASS location. If empty, this optional column is omitted.
#' @param lu Output: Name of Landscape Units (LU) raster map exported into
#'      GRASS location.
#' @param dir_out Character string specifying output directory (will be created;
#'      any overwriting will be prompted).
#' @param sub_ofile Output: Name of subbasin statistics file containing subbasin
#'      parameters. See \code{Details} below.
#' @param lu_ofile Output: Name of file containing subbasins and the corresponding
#'      LUs with their fraction of area in the subbasin.
#' @param lupar_ofile Output: Name of file containing LUs and related parameters.
#'      See \code{Details} below.
#' 
#' @return Function returns nothing. Output files (\code{sub_ofile, lupar_ofile}) are written into output directory
#'      and raster maps (\code{lu}, temporary rasters stream_main_t, cell_len_t, flowacc_minmax_t, MASK_t) exported into GRASS location as specified in arguments.
#'      
#' @note Prepare GRASS location and necessary raster files in advance (e.g. using
#'      \code{\link[LUMP]{lump_grass_prep}}) and start GRASS session in R using 
#'      \code{\link[spgrass6]{initGRASS}}.
#'      
#'      TODO:\cr
#'        - check arguments\cr
#'        - check empirical formulas for channel width and channel depth\cr
#'        - LU parameter estimation\cr
#'        - include options to add parameters manually in case data are available
#'        
#'        
#' @details
#'      \bold{Subbasin parameters}\cr
#'      Subbasin parameter estimation given in \code{sub_ofile} contains:
#'      
#'      \emph{pid}\cr
#'      Subbasin identifier.
#'      
#'      \emph{area}\cr
#'      Subbasin area in \emph{km^2}.
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
#'      \bold{Landscape Unit parameters}\cr
#'      Landscape Unit parameter estimation given in \code{lupar_ofile} contains:
#'      
#'      \emph{pid}\cr
#'      Landscape Unit identifier.
#'      
#'      \emph{soil_depth}\cr
#'      Soil depth in \emph{mm} averaged over respective landscape unit.
#'      
#'      \emph{gw_flag}\cr
#'      Groundwater flag: 0: no groundwater in this LU. 1: LU contains groundwater.
#'      At the moment set to 1 by default for every LU.
#'      
#'      \emph{gw_dist}\cr
#'      Initial depth of groundwater below surface in \emph{mm}. At the moment set to
#'      1000 by default for every LU.
#'      
#'      \emph{frgw_delay}\cr
#'      Storage coefficient for groundwater outflow in \emph{days}. At the moment set to
#'      200 by default for every LU.
#'      
#'      \emph{sdr_lu} (optional)\cr
#'      sediment delivery ratio from raster map \code{sdr}, if specified
#'      
#'      
#' @references Source code based on \code{SHELL} and \code{MATLAB} scripts of Till Francke.
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
#'
#' @export 
lump_grass_post <- function(
  ### INPUT ###
  dem,
  recl_lu,
  eha,
  subbasin,
  mask,
  flowacc,
  flowdir,
  stream_horton,
  soil_depth,
  sdr,
  
  ### OUTPUT ###
  lu,
  dir_out="./",
  sub_ofile="",
  lu_ofile="",
  lupar_ofile=""
  
) {
  tryCatch({
  #reclass EHA according to reclass file generated by prof_class to get LU
  x=execGRASS("r.reclass", input=eha, output=lu, rules=recl_lu, flags="overwrite")
  })
  
  ### CALCULATIONS ###
  tryCatch({
    # output dir
    dir.create(dir_out, recursive=T, showWarnings=F)
    if (file.exists(paste(dir_out,sub_ofile,sep="/")) | file.exists(paste(dir_out,lu_ofile,sep="/")))
    {  
      print(paste0("Output file(s) ", sub_ofile, " or ",lu_ofile, " already exist(s) in ", dir_out, "!, type 'o' to overwrite, all else to abort."))
      flush.console()
      ch=readline()
      if (ch!='o') stop('... aborted.')
    }  
      
    
    # set basin-wide mask
    execGRASS("r.mask", input=mask, flags=c("o"))
    
    # load rasters into R
    accum_rast <- raster(readRAST6(flowacc))
    dir_rast <- raster(readRAST6(flowdir))
    sub_rast <- raster(readRAST6(subbasin))
    # ... raster values as matrix
    sub_mat <- getValues(sub_rast, format="matrix")
    accum_mat <- getValues(accum_rast, format="matrix")
    dir_mat <- getValues(dir_rast, format="matrix")
    rm(list="accum_rast","dir_rast","sub_rast")
  }, error = function(e) {
    # set basin-wide mask again
    execGRASS("r.mask", input=mask, flags=c("o"))
    # remove all output
    execGRASS("g.remove", rast="lu,MASK_t,cell_len_t,stream_main_t,flowacc_minmax_t")
    stop(paste(e))
  })
    
    
  ### subbasin statistics ###
  if ((sub_ofile != "") || (lu_ofile != ""))
  tryCatch({
    sub_stats <- execGRASS("r.stats", input=subbasin, flags=c("a", "n"), intern=TRUE) ##windows version
    if (grepl(pattern="[0-9]+.*[\b]+",x=tail(sub_stats, n=1)))
      sub_stats <- sub_stats[-length(sub_stats)] #last line contains progress indicator, remove
    
    sub_stats <- matrix(as.numeric(gsub("%", "", unlist(strsplit(sub_stats, split=" +")))), ncol=2, byrow=T,
                        dimnames=list(NULL,c("pid", "area")))
    
    #sub_stats[,"area"] <- sub_stats[,"areal_fraction"]/100 #convert % to fraction
    sub_stats[,"area"] <- sub_stats[,"area"]/1e6 #convert m? to km?
      
    # calculate stats of LUs in each subbasin and subbasin drainage ("drains_to")
    sub_stats <- cbind(sub_stats, NA, NA, NA)
    colnames(sub_stats)[c(3:5)] <- c("drains_to", "lag_time", "retention")
    sub_lu_stats <- NULL
    execGRASS("g.remove", rast="MASK_t,MASK", flags=c("f"))  
    for (SUB in sub_stats[,1]) {
      
      # create temp mask masking all but subbasin SUB
      execGRASS("r.mapcalculator", amap=subbasin, outfile="MASK_t", formula=paste("if(A==", SUB,",1,null())", sep=""), flags=c("overwrite"))
      # set temp mask
      execGRASS("r.mask", input="MASK_t", flags=c("o"))
      
  ### SUBBASIN drainage ###
      s_row <- which(sub_stats[,"pid"] == SUB)
      sub_stats[s_row,"drains_to"] <- sub_route(SUB,sub_mat,accum_mat,dir_mat) # internal function, see below
  
  ### SUBBASIN PARAMETERS ###
      # calc main channel length
      chan_len <- channel_length(SUB,stream_horton, flowdir, flowacc)
      
      # calc main channel average slope
      chan_slope <- channel_slope("stream_main_t", flowacc, dem, chan_len)
  
      # calc main channel width
      chan_width <- channel_width(flowacc)
  
      # calc main channel depth
      chan_depth <- channel_depth(flowacc)
  
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
      sub_stats[s_row, "lag_time"] <- flowtime_med
      sub_stats[s_row, "retention"] <- retention
  
      write.table(sub_stats, paste(dir_out, sub_ofile, sep="/"), quote=F, row.names=F, sep="\t")
      
    
  
  
  ### Landscape Unit statistics ###
      # calculate statistics of LUs in subbasin SUB
  if (lu_ofile != "")
    
      sub_lu_stats_t <- execGRASS("r.stats", input=lu, flags=c("n", "p"), intern=TRUE) ##windows version
      if (grepl(pattern="[0-9]+.*[\b]+",x=tail(sub_lu_stats_t, n=1))) #check if last line contains progress indicator, remove
        sub_lu_stats_t = sub_lu_stats_t[-length(sub_lu_stats_t)] 
      sub_lu_stats_t <- matrix(as.numeric(gsub("%", "", unlist(strsplit(sub_lu_stats_t, split=" +")))), ncol=2, byrow=T)
      sub_lu_stats_t[,2] <- sub_lu_stats_t[,2] / 100
      sub_lu_stats <- rbind(sub_lu_stats, cbind(rep(SUB,nrow(sub_lu_stats_t)), sub_lu_stats_t) )
      # set basin-wide mask again
      execGRASS("r.mask", input=mask, flags=c("o"))
  
      # remove temp rasters
      execGRASS("g.remove", rast="cell_len_t,stream_main_t,flowacc_minmax_t")
    }
    execGRASS("g.remove", rast="MASK_t,MASK", flags=c("f"))
    
    
    dimnames(sub_lu_stats) <- list(NULL, c("subbas_id", "lu_id", "fraction"))
    write.table(sub_lu_stats, paste(dir_out, lu_ofile, sep="/"), quote=F, row.names=F, sep="\t")
  
    }, error = function(e) {
    # set basin-wide mask again
    execGRASS("r.mask", input=mask, flags=c("o"))
    # remove all output
    execGRASS("g.remove", rast="lu,MASK_t,cell_len_t,stream_main_t,flowacc_minmax_t")
    stop(paste(e))
  })

    
  ### LANDSCAPE UNIT PARAMETERS ###
  if (lupar_ofile != "")
    tryCatch({
# load lu file
    #lu_dat <- read.table(paste(dir_out,luoutfile,sep="/"), header=T)
    
    # calculate mean soil depth for every LU
  lu_ids <- execGRASS("r.stats", input=lu, flags=c("n"), intern=TRUE)
  if (grepl(pattern="[0-9]+.*[\b]+",x=tail(lu_ids, n=1)))
    lu_ids <- as.numeric(lu_ids[-length(lu_ids)]) #last line contains progress indicator, remove
  
  
  if (!is.null(soil_depth) && soil_depth!="")
    {
      cmd_out <- execGRASS("r.univar", zones=lu, map=soil_depth, fs=",", flags=c("t"),intern=T)
      if (grepl(pattern="[0-9]+.*[\b]+",x=tail(cmd_out, n=1)))
        cmd_out <- cmd_out[-length(cmd_out)] #last line contains progress indicator, remove
      cmd_out <- strsplit(cmd_out, ",")
      cmd_out <- matrix(unlist(cmd_out[-1]), ncol=length(cmd_out[[1]]), byrow=T,
                        dimnames=list(NULL, cmd_out[[1]]))
      lu_depth <- as.numeric(cmd_out[,"mean"]) * 10
      lu_ids = as.numeric(cmd_out[,"zone"])
    } else lu_depth=NA
    lu_par <- cbind(lu_ids, lu_depth)
    colnames(lu_par) <- c("pid", "soil_depth")
  
  if (!is.null(sdr) && sdr!="")
  {
    cmd_out <- execGRASS("r.univar", zones=lu, map=sdr, fs=",", flags=c("t"),intern=T)
    if (grepl(pattern="[0-9]+.*[\b]+",x=tail(cmd_out, n=1)))
      cmd_out <- cmd_out[-length(cmd_out)] #last line contains progress indicator, remove
    cmd_out <- strsplit(cmd_out, ",")
    cmd_out <- matrix(unlist(cmd_out[-1]), ncol=length(cmd_out[[1]]), byrow=T,
                      dimnames=list(NULL, cmd_out[[1]]))
    sdr_vals <- as.numeric(cmd_out[,"mean"]) 
    lu_ids = as.numeric(cmd_out[,"zone"])
    lu_par <- cbind(lu_par, sdr_lu=sdr_vals[match(lu_par[,"pid"], lu_ids)])
  }
  
  
    # groundwater parameters (so far only default values)
    # groundwater for every LU
    gw_flag <- rep(1, length(lu_depth))
    # initial depth of groundwater below surface: 1000 mm
    gw_dist <- rep(1000, length(lu_depth))
    # storage coefficient for groundwater outflow [days]
    frgw_delay <- rep(200, length(lu_depth))
  
    # combine
    lu_par <- cbind(lu_par,gw_flag,gw_dist,frgw_delay)
    
  
    # write output
    write.table(lu_par, paste(dir_out, lupar_ofile, sep="/"), quote=F, row.names=F, sep="\t")
  
  
  }, error = function(e) {
    # set basin-wide mask again
    execGRASS("r.mask", input=mask, flags=c("o"))
    # remove all output
    execGRASS("g.remove", rast="lu,MASK_t,cell_len_t,stream_main_t,flowacc_minmax_t")
    stop(paste(e))
  })
  

} # EOF




### internal functions ###

# SUBBASIN ROUTING #
# returns ID of downstream subbasin for the current subbasin 'sub_no'
# determined from flow accumulation and flow direction map
sub_route <- function(sub_no,sub_mat,accum_mat,dir_mat) {
  
  # extract highest flowacc in subbasin sub_no
  sub_ids <- which(sub_mat == sub_no)
  accum_sub <- accum_mat[sub_ids]
  accum_sub_max <- which(accum_sub == max(accum_sub))
  
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
      stop(paste("During determining subbasin drainage: Determined flow direction at outlet
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


# MAIN CHANNEL LENGTH #
# returns length of the main channel (largest value in Horton order) in [m]
# computes main stream temporary raster used in further calculations
channel_length <- function(sub_no, stream, flowdir, flowacc) {
  
  if (is.null(stream) || stream=="") return(NA)
  # determine resolution of horton stream raster map
  cmd_out <- execGRASS("r.info", map=stream, flags=c("s"), intern=T)
  if (identical(attr(cmd_out, "status"), as.integer(1))) return(NA)
  resol <- as.numeric(unlist(strsplit(cmd_out, "="))[c(2,4)])
  names(resol) <- unlist(strsplit(cmd_out, "="))[c(1,3)]
  
  # determine main channel (largest value in Horton stream order)
  main_chan <- as.numeric(execGRASS("r.stats", input=stream, flags=c("n"), intern=T))
  
  # if there is no main channel (e.g. in very small reservoir subbasins) assume one cell of main stream
  if (length(main_chan) == 0) {
    chan_len <- mean(resol)
    execGRASS("r.mapcalculator", amap=flowacc, outfile="stream_main_t", 
              formula="if(A == max(A), 1, null())")
    warning(paste("Subbasin ", sub_no, " has no main channel. Assume at least one raster cell.", sep=""))

  } else {
    
    max_val <- max(main_chan)
    expr <- paste0("if(A == ", max_val, ", A, null())")
    expr <- gsub("\n|\\s","",expr)
    execGRASS("r.mapcalculator", amap=stream, outfile="stream_main_t", formula=expr)
    
    # calculate lengths of main stream raster cells
    dia <- sqrt(sum(resol^2))
    expr <- paste0("if(isnull(A), null(),
                    if(B == 4 || B == 8, ",resol["ewres"],", 0)+
                    if(B == 2 || B == 6, ",resol["nsres"],", 0)+
                    if(B == 1 || B == 3 || B == 5 || B == 7, ",dia,", 0))")
    expr <- gsub("\n|\\s","",expr)
    execGRASS("r.mapcalculator", amap="stream_main_t", bmap=flowdir, outfile="cell_len_t", formula=expr)
    
    # calculate total length of main stream channel
    cmd_out <- execGRASS("r.univar", map="cell_len_t", fs=",", flags=c("t"),intern=T)
    cmd_out <- strsplit(cmd_out, ",")
    chan_len <- as.numeric(cmd_out[[2]][grep("sum$", cmd_out[[1]])])
  }
  
  return(chan_len)
} # EOF


# MAIN CHANNEL SLOPE #
# returns average slope of main channel [m/m]
channel_slope <- function(stream_main, flowacc, dem, chan_len) {  
  if (is.na(chan_len) ) return(NA)
  # calc min and max flow accumulation of main channel rasters
  cmd_out <- execGRASS("r.univar", zones=stream_main, map=flowacc, fs=",", flags=c("t"),intern=T)
  cmd_out <- strsplit(cmd_out, ",")
  accum_vals <- as.numeric(cmd_out[[2]][grep("min$|max$", cmd_out[[1]])])
  
  # get dem values for respective raster cells of min and max flowacc
  expr <- paste("if(A, if(B == ", accum_vals[1], " || B == ", accum_vals[2], ", 1, null())
               , null())", sep="")
  expr <- gsub("\n|\\s","",expr)
  execGRASS("r.mapcalculator", amap=stream_main, bmap=flowacc, outfile="flowacc_minmax_t",
            formula=expr)
  cmd_out <- execGRASS("r.univar", zones="flowacc_minmax_t", map=dem, fs=",", flags=c("t"),intern=T)
  cmd_out <- strsplit(cmd_out, ",")
  dem_vals <- as.numeric(cmd_out[[2]][grep("min$|max$", cmd_out[[1]])])
  
  # compute slope [m/m]
  chan_slope <- diff(dem_vals) / chan_len
  
  # slope zero not allowed -> at least 0.00001
  chan_slope <- max(chan_slope, 0.00001)
  
  return(chan_slope)
}


# MAIN CHANNEL WIDTH #
# calculate main channel width based on empirical formula: width[m] = 1.29 * darea[km2] ^ 0.6
# darea = drainage area determined from maximum flow accumulation and resolution
channel_width <- function(flowacc) {
  # determine maximum flow accumulation
  cmd_out <- execGRASS("r.univar", map=flowacc, fs=",", flags=c("t"),intern=T)
  cmd_out <- strsplit(cmd_out, ",")
  max_acc <- as.numeric(cmd_out[[2]][grep("max$", cmd_out[[1]])])
  
  # calculate drainage area [km^2]
  cmd_out <- execGRASS("r.info", map=flowacc, flags=c("s"), intern=T)
  resol <- as.numeric(unlist(strsplit(cmd_out, "="))[c(2,4)])
  names(resol) <- unlist(strsplit(cmd_out, "="))[c(1,3)]
  drain_area <- max_acc * prod(resol) / 1e6
  
  # calculate width according to empirical function
  chan_width <- 1.29 * drain_area^(0.6)
  
  if(chan_width > 3000)
    stop(paste("Calculated channel width is ", chan_width, "m which seems unrealistic!", sep=""))
  
  return(chan_width)
}


# MAIN CHANNEL DEPTH #
# calculate main channel width based on empirical formula: width[m] = 0.13 * darea[km2] ^ 0.4
# darea = drainage area determined from maximum flow accumulation and resolution
channel_depth <- function(flowacc) {
  # determine maximum flow accumulation
  cmd_out <- execGRASS("r.univar", map=flowacc, fs=",", flags=c("t"),intern=T)
  cmd_out <- strsplit(cmd_out, ",")
  max_acc <- as.numeric(cmd_out[[2]][grep("max$", cmd_out[[1]])])
  
  # calculate drainage area [km^2]
  cmd_out <- execGRASS("r.info", map=flowacc, flags=c("s"), intern=T)
  resol <- as.numeric(unlist(strsplit(cmd_out, "="))[c(2,4)])
  names(resol) <- unlist(strsplit(cmd_out, "="))[c(1,3)]
  drain_area <- max_acc * prod(resol) / 1e6
  
  # calculate depth according to empirical function
  chan_depth <- 0.13 * drain_area^(0.4)
  
  if(chan_depth > 50)
    stop(paste("Calculated channel depth is ", chan_depth, "m which seems unrealistic!", sep=""))
  
  return(chan_depth)
}


# FLOW VELOCITY #
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

