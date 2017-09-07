# lumpR/subbasin.R
# Copyright (C) 2014-2017 Tobias Pilz
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


#' Calculation of hydrological subbasins using GRASS GIS
#' 
#' Takes DEM from a GRASS location and a file of drainage locations to calculate 
#' hydrological subbasins for each drainage point using GRASS functions.
#' 
#' @param dem Digital elevation model in GRASS location used for delineation of
#'      subbasins. Should be larger than the expected catchment, otherwise artefacts
#'      close to boundaries may occur.
#' @param drain_points \code{SpatialPoints} object containing drainage locations in
#'      units of and compliant with the projection of your respective GRASS location.
#'      At least the watershed drainage point has to be given.
#' @param river River vector map in GRASS location if available. If set to \code{NULL}
#'      (default value) river network will be calculation by GRASS function \emph{r.watershed}.
#' @param basin_out Output: Name of subbasins raster map exported into GRASS location.
#' @param stream Output: Prefix of calculated stream segments vector (<stream>_vect) and
#'      raster (<stream>_rast) maps exported into GRASS location. Only generated if
#'      \code{river} is not set. Default: \code{NULL}.
#' @param points_processed Output: Prefix of point vector files exported to GRASS location.
#'      \code{<points_processed>_snap} are given \code{drain_points} snapped to river.
#'      \code{<points_processed>_calc} are internally calculated drain points (only if
#'      parameter \code{thresh_sub} is not \code{NULL}, see below).
#' @param outlet Integer (row number) defining the catchment outlet in \code{drain_points}.
#'      If there are \code{drain_points} outside the watershed delineated for the
#'      outlet point these will be omitted. If \code{NULL} (default) and \code{drain_points}
#'      contains only one point this will be assumed as catchment outlet.
#' @param thresh_stream Integer defining threshold for stream calculation. Raster
#'      cells in accumulation map with values greater than thresh_stream are
#'      considered as streams. Needs to be set only if \code{river} is not set.
#'      Default: \code{NULL}.
#' @param thresh_sub Integer defining threshold for subbasin calculation. Parameter for
#'      GRASS function \emph{r.watershed} defining the minimum size of an exterior
#'      watershed basin in number of grid cells. If \code{NULL} (default) only the
#'      given drainage points are used for subbasin delineation.
#' @param snap_dist Integer defining maximum distance for snapping of \code{drain_points}
#'      to stream segments in units of your GRASS location.
#' @param rm_spurious \code{numeric}. If greater zero, spurious subbasins will
#'      be removed, i.e. those subbasins being smaller than \code{rm_spurious} times \code{thresh_sub}.
#'      Spurious subbasins are 'interior' watersheds created by GRASS function
#'      \emph{r.watershed} around stream segments below multiple tributaries. If they
#'      are very small they induce unnecessary computational burden when used within a
#'      hydrological model. If removed, these areas will be related to the next upstream
#'      subbasins, respectively. If \code{thresh_sub = NULL} (default) \code{rm_spurious}
#'      will be automatically set to \code{0}. Default: 0.01.
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#'      
#'      
#' @note Prepare GRASS location and necessary raster files in advance and start
#'      GRASS session in R using \code{\link[spgrass6]{initGRASS}}. Location
#'      should not contain any maps ending on *_t as these will be removed by
#'      calling the function to remove temporary maps.
#'      
#'      You should select your DEM sufficiently large. Otherwise the resulting
#'      catchment might be truncated or boundaries influence the calculation
#'      of stream segments.
#'      
#'      Check the results (subbasins and snapped points). In case points have been snapped
#'      to the wrong stream segment, adjust point locations manually in GRASS and re-run
#'      the function with the updated locations (use \code{\link[spgrass6]{readVECT6}}
#'      to import the updated drainage points). Also check that calculated subbasins and
#'      those delineated based on given \code{drain_points} do not interfere (e.g.
#'      resulting in very small spurious subbasins).
#'      
#'      Generated raster and vector stream maps might slightly deviate from each other
#'      as the raster map is thinned (GRASS function \emph{r.thin}) prior to conversion
#'      to a vector map to ensure strictly linear features.
#' 
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}

calc_subbas <- function(
  ### INPUT ###
  dem=NULL,
  drain_points=NULL,
  river=NULL,
  
  ### OUTPUT ###
  basin_out=NULL,
  stream=NULL,
  points_processed=NULL,
  
  ### PARAMETER ###
  outlet=NULL,
  thresh_stream=NULL,
  thresh_sub=NULL,
  snap_dist=NULL,
  rm_spurious=0.01,
  keep_temp=F,
  overwrite=F,
  silent=F
) {

### PREPROCESSING ###
  
  # CHECKS #
  if(is.null(dem))
    stop("The name of a DEM within the mapset of your initialised GRASS session has to be given!")
  if(is.null(drain_points) | !grepl("SpatialPoints", class(drain_points)))
    stop("drain_points has to be given as SpatialPoints* object with at least one catchment outlet point!")
  if(is.null(river) & (is.null(thresh_stream | is.null(stream))))
    stop("If no river object is given, stream as name prefix for the generated stream maps and the parameter thresh_stream have to be specified for internal calculation of the river network!")
  if(is.null(basin_out))
    stop("You have to specify basin_out as name for the subbasin map to be generated!")
  if(is.null(points_processed))
    stop("You have to specify points_processed!")
  if(!is.numeric(snap_dist))
    stop("You have to specify snap_dist as a number!")
  if(is.null(outlet)) {
    if(nrow(drain_points@coords) > 1)
      stop("You have to give 'outlet' if the given number of drain_points is greater than one!")
    outlet <- 1
  }
  if(is.null(thresh_sub))
    rm_spurious <- 0
  if(!is.numeric(rm_spurious))
    stop("Argument 'rm_spurious' has to be numeric (behaviour changed in version 2.0.4)!")
  
  
  # CLEAN UP AND RUNTIME OPTIONS #  
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
  

### CALCULATIONS ###
  tryCatch({
    
    message("\nInitialise function...\n")
    
    # remove mask if there is any
    suppressWarnings(execGRASS("r.mask", flags=c("r")))
    
    # remove output of previous function calls if overwrite=T
    if (overwrite) {
      execGRASS("g.mremove", rast=paste0("*_t,",stream,"_rast,", basin_out), vect=paste0(stream,"_vect,*_t,", points_processed, "_snap,", points_processed, "_calc"), flags=c("f", "b"))
    } else {
      # remove temporary maps in any case
      execGRASS("g.mremove", rast="*_t", vect="*_t", flags=c("f", "b"))
    }
    
    
  ### calc stream segments or use user defined input
    if(is.null(river)) {
      message("\nCalculate drainage and river network...\n")
      # GRASS watershed calculation #
      execGRASS("r.watershed", elevation=dem, accumulation="accum_t", drainage="drain_t")
      # check thresh_stream parameter
      cmd_out <- execGRASS("r.univar", map="accum_t", fs="comma", flags=c("t"), intern=T, ignore.stderr = T)
      cmd_out <- strsplit(cmd_out, ",")
      cmd_cols <- grep("^max$", cmd_out[[1]])
      max_acc <- as.numeric(cmd_out[[2]][cmd_cols])
      if(thresh_stream > max_acc)
        stop(paste0("Parameter 'thresh_stream' (", thresh_stream, ") is larger than the maximum flow accumulation within the study area (", max_acc, "). Choose a smaller parameter value!"))
      # calculate stream segments (don't use output of r.watershed as streams should be finer than generated therein)
      execGRASS("r.mapcalculator", amap="accum_t", outfile=paste0(stream, "_rast"), 
                formula=paste("if(abs(A)>", format(thresh_stream, scientific = F), ",1,0)", sep=""))
      # thin
      execGRASS("r.thin", input=paste0(stream, "_rast"), output=paste0(stream, "_thin_t"))
      # convert to vector
      execGRASS("r.to.vect", input=paste0(stream, "_thin_t"), output=paste0(stream, "_vect"), feature="line")
      river <- paste0(stream, "_vect")
      
    } else {
      message("\nCalculate drainage...\n")
      execGRASS("r.watershed", elevation=dem, drainage="drain_t")
    }
    
    
    
  ### Calculate subbasins without given drainage points (optional)
    if(is.numeric(thresh_sub)) {
      message("\nCalculate subbasins based on given area threshold...\n")
      
      # calculate subbasins
      execGRASS("r.watershed", elevation=dem, basin="basin_calc_t", threshold=thresh_sub)
    }
    
  ### snap given drainage points to streams
    message("\nSnap given drainage points to streams...\n")
    
    # read stream vector
    streams_vect <- readVECT6(river)
    # WINDOWS PROBLEM: delete temporary file otherwise an error occurs when calling writeVECT or readVECT again with the same (or a similar) file name 
    if(.Platform$OS.type == "windows") {
      dir_del <- dirname(execGRASS("g.tempfile", pid=1, intern=TRUE, ignore.stderr=T))
      files_del <- grep(substr(river, 1, 8), dir(dir_del), value = T)
      file.remove(paste(dir_del, files_del, sep="/"))
    }
  
    # snap points to streams
    drain_points_snap <- suppressWarnings(snapPointsToLines(drain_points, streams_vect, maxDist=snap_dist))
    if (length(drain_points_snap) < length(drain_points)) stop("Less points after snaping than in drain_points input!\nComputed stream segmets probably are too coarse. Try different parameter values.")
    
    # df should only contain a cat column
    drain_points_snap@data <- data.frame(cat=1:nrow(drain_points_snap@data))
    
    # export drain_points_snap to GRASS
    suppressWarnings(writeVECT6(drain_points_snap, paste0(points_processed, "_snap")))
    # WINDOWS PROBLEM: delete temporary file otherwise an error occurs when calling writeVECT or readVECT again with the same (or a similar) file name 
    if(.Platform$OS.type == "windows") {
      dir_del <- dirname(execGRASS("g.tempfile", pid=1, intern=TRUE, ignore.stderr=T))
      files_del <- grep(substr(paste0(points_processed, "_snap"), 1, 8), dir(dir_del), value = T)
      file.remove(paste(dir_del, files_del, sep="/"))
    }
    
    
  ### calculate catchments for every drainage point
    message("\nCalculate catchments for every drainage point...\n")
    
    # watershed for the defined outlet
    outlet_coords <- coordinates(drain_points_snap)[outlet,]
    execGRASS("r.water.outlet", drainage="drain_t", basin=paste0("basin_outlet_t"),
          easting=as.character(outlet_coords[["X"]]), northing=as.character(outlet_coords[["Y"]]))
    
    
    # get drainage points of calculated subbasins (optional)
    if(is.numeric(thresh_sub)) {
      
      # set watershed of outlet point as mask
      execGRASS("r.mask", input="basin_outlet_t")
      
      # the following calculations only make sense if thresh_sub is small enough to produce more than more subbasin
      no_catch_calc <- length(as.numeric(execGRASS("r.stats", input="basin_calc_t", flags=c("n"), intern=T, ignore.stderr = T)))
      if(no_catch_calc > 1) {
        
        # read raster data from GRASS for processing
        basins <- raster(readRAST6("basin_calc_t", ignore.stderr = T))
        accum <- raster(readRAST6("accum_t", ignore.stderr = T))
        
        # calculate zonal statistics: Maximum accumulation for every subbasin (=outlet)
        stats <- zonal(accum, basins, fun="max")
        
        # remove calculated watershed outlet (point of maximum flow accumulation) as this has been given as input
        stats <- stats[-which(stats[2] == max(stats[2])), ]
        
        # get coordinates of outlets
        outs <- apply(stats, 1, function(x) {
          cell_no <- Which(accum==x[2], cells=T)
          
          # if there is more than one cell, get the one in the right subbasin
          if(length(cell_no) > 1) {
            cell_bas <- Which(basins==x[1], cells=T)
            cell_no <- cell_no[which(cell_no %in% cell_bas)]
          }
          
          # get coordinates of cell_no
          res <- round(xyFromCell(accum, cell_no),0)
          res <- cbind(res, x[1])
          return(res)
        })
        
        # delete raster objects
        rm(accum, basins)
        gc(verbose = F); gc(verbose = F)
        
        # re-arrange data
        outs <- t(outs)
        colnames(outs) <- c("x", "y", "cat")
        outs <- as.data.frame(outs)
        coordinates(outs) <- c("x", "y")
        
        # as SPDF
        drain_points_calc <- SpatialPointsDataFrame(coordinates(outs), outs@data, proj4string = CRS(getLocationProj()))
        
        # write to GRASS location
        writeVECT6(drain_points_calc, paste0(points_processed, "_calc"), ignore.stderr = T)
        
        # merge with existing drain points object (snapped points first as there the outlet is identified)
        drain_points_snap <- rbind(drain_points_snap, drain_points_calc)
        
        # remove mask
        execGRASS("r.mask", flags=c("r"))
      }
    }
    

    # loop over drainage points TODO: This step is slow!
    for (p in 1:length(drain_points_snap)) {

      # outlet coordinates
      outlet_coords <- coordinates(drain_points_snap)[p,]
      
      # basin
      execGRASS("r.water.outlet", drainage="drain_t", basin=paste0("basin_", p, "_t"),
                easting=as.character(outlet_coords[["X"]]), northing=as.character(outlet_coords[["Y"]]))

      # reclass (subbasins gets number of i for crossing later)
      execGRASS("r.mapcalculator", amap=paste0("basin_", p, "_t"), outfile=paste0("basin_recl_", p, "_t"),
                formula=paste0("if(A,",p, ")"))
      
    }

    no_catch <- p
  
    
  ### merge all sub-catchments
    message("\nMerge calculated catchments...\n")
    
    # put sub-catchments together
    subcatch_rasts <- execGRASS("g.mlist", type="rast", pattern=paste0("basin_recl_[0-9]*_t"), intern=T)

    # if more than one sub-catchment
    if(no_catch > 1) {
      
      # drain points needed as raster
      suppressWarnings(writeVECT6(drain_points_snap, paste0(points_processed, "_all_t"), v.in.ogr_flags = "overwrite"))
      # WINDOWS PROBLEM: delete temporary file otherwise an error occurs when calling writeVECT or readVECT again with the same (or a similar) file name 
      if(.Platform$OS.type == "windows") {
        dir_del <- dirname(execGRASS("g.tempfile", pid=1, intern=TRUE, ignore.stderr=T))
        files_del <- grep(substr(paste0(points_processed, "_all_t"), 1, 8), dir(dir_del), value = T)
        file.remove(paste(dir_del, files_del, sep="/"))
      }
      x <- execGRASS("v.to.rast", input=paste0(points_processed, "_all_t"), output=paste0(points_processed, "_all_t"), use="cat", flags="overwrite", intern=T)
      
      # iterate until configuration without 'spurious' sub-catchments is found (if rm_spurious > 0) TODO: This step is slow in case many iterations are needed!
      while (TRUE) {
      
        # max 30 maps at once, create multiple cross products if necessary
        x <- execGRASS("g.mremove", rast="basin_cross_*", flags="f", intern=T) # remove old basin_cross_*
        iterations <- ceiling(length(subcatch_rasts)/30)
        for (j in 1:iterations){
          if (j == iterations) {
            if(length(subcatch_rasts) %% 30 == 1) {
              x <- execGRASS("g.copy", rast=paste(subcatch_rasts[((j-1)*30+1):length(subcatch_rasts)], paste0("basin_cross_", j, "_t"), sep=","), 
                             intern=T, ignore.stderr=T)
            } else {
              x <- execGRASS("r.cross", input=paste(subcatch_rasts[((j-1)*30+1):length(subcatch_rasts)], collapse=","),
                             output=paste0("basin_cross_", j, "_t"), flags = c("overwrite"), intern=T, ignore.stderr=T)
            }
          } else {
            x <- execGRASS("r.cross", input=paste(subcatch_rasts[((j-1)*30+1):(j*30)], collapse=","),
                           output=paste0("basin_cross_", j, "_t"), flags = c("overwrite"), intern=T, ignore.stderr=T)
          }
        }
        
        # merge cross products
        cross_rasts <- execGRASS("g.mlist", type="rast", pattern=paste0("basin_cross_[0-9]*_t"), intern=T)
        if(length(cross_rasts) == 1) {
          x <- execGRASS("g.rename", rast=paste(cross_rasts, "basin_all_t", sep=","), intern=T, ignore.stderr=T)
        } else {
          x <- execGRASS("r.cross", input=paste(cross_rasts,collapse=","), output="basin_all_t",
                    flags = c("overwrite"), intern=T, ignore.stderr=T)
        }
        
        # check size of sub-catcments and identify and remove 'spurious' sub-catchments
        if(rm_spurious>0) {
          # get sub-catcments and sizes (cell counts) and identify spurious ones
          cmd_out <- execGRASS("r.stats", input="basin_all_t", flags=c("n", "c"), intern=T, ignore.stderr = T)
          sub_sizes <- matrix(as.numeric(unlist(strsplit(cmd_out, " "))), ncol=2, byrow=T)
          sub_sizes <- sub_sizes[-which(sub_sizes[,1] == 0),]
          sub_rm <- sub_sizes[which(sub_sizes[,2] < rm_spurious*thresh_sub),1]
          if(length(sub_rm)>0) {
            # get number of basin_recl_* to be romved (not identical with raster values of basin_all_t!)
            cmd_out <- execGRASS("r.univar", map=paste0(points_processed, "_all_t"), zones="basin_all_t", fs="comma", flags=c("t"), intern=T, ignore.stderr = T)
            cmd_out <- strsplit(cmd_out, ",")
            cmd_cols <- grep("zone|^mean$", cmd_out[[1]])
            basins_points <- do.call(rbind, cmd_out)[-1,cmd_cols, drop=F]
            sub_rm_f <- as.numeric(basins_points[which(as.numeric(basins_points[,1]) %in% sub_rm),2])
            # remove this temporary map from processing and try again (back to start of while loop)
            subcatch_rasts <- grep(paste0("basin_recl_", sub_rm_f, "_t", collapse="|"), subcatch_rasts, invert = T, value = T)
            x <- execGRASS("g.remove", rast="basin_all_t", intern=T)
            # update no_catch
            no_catch <- no_catch - length(sub_rm_f)
          } else {
            break
          }
        } else {
          break # exit while loop
        }
      
      } # end while-loop
      
      # constrain to catchment of outlet point
      execGRASS("r.mapcalculator", amap="basin_outlet_t", bmap="basin_all_t", outfile=basin_out,
            formula="A*B")
      
      
    } else { # only one sub-catchment
      
      if(no_catch == 0)
        stop("Number of identified sub-catchments is zero. Check input data!")
      
      # basin_outlet_t is basin_out
      execGRASS("g.rename", rast=paste("basin_outlet_t", basin_out, sep=","))
    }


    # set values of zero to NULL
    execGRASS("r.null", map=basin_out, setnull="0")
    
    no_cross <- length(execGRASS("r.stats", input=basin_out, flags=c("n"), intern=T, ignore.stderr = T))
    if(no_catch != no_cross) warning(paste0("\nNumber of categories in ", basin_out, " not equal to number of drainage points!\nThis might be because there are drainage points outside the catchment of the defined outlet or due to small inconsistencies between calculated and manually defined (and snapped) drainage points. However, you should check the output with the GRASS GUI and consider the help pages of this function!"))
    
    # remove temporary maps
    if(keep_temp == FALSE)
      execGRASS("g.mremove", rast="*_t", vect="*_t", flags=c("f"))
    
    
    
    message("\nFinished.\n\n", 
            "Check the results for plausibility (e.g. inaccuracies at snapping of drain_points to streams may occur). ",
            "If manual adjustments are necessary re-run the function for re-calculation of subbasins.\n")
    
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)


    
    
  # exception handling
  }, error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    execGRASS("r.mask", flags=c("r"))
    
    if(keep_temp == FALSE)
      execGRASS("g.mremove", rast=paste0("*_t,",stream,"_rast,", basin_out), vect=paste0(stream,"_vect,*_t,", points_processed, "_*"), flags=c("f", "b"))
    
    stop(paste(e))  
  })
  

} # EOF
