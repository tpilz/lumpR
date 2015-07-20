# LUMP/subbasin.R
# Copyright (C) 2014,2015 Tobias Pilz
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
#' hydrological subbasin for each drainage point using GRASS functions.
#' 
#' @param dem Digital elevation model in GRASS location used for delineation of
#'      subbasins. Should be larger than the expected catchment, otherwise artefacts
#'      close to boundaries may occur.
#' @param drain_points \code{SpatialPoints} object containing drainage locations in
#'      units of and compliant with the projection of your respective GRASS location.
#' @param river River vector map in GRASS location if available. If set to \code{NULL}
#'      (default value) river network will be calculation by GRASS function \emph{r.watershed}.
#' @param basin_out Output: Name of subbasins raster map exported into GRASS location.
#' @param stream Output: Prefix of calculated stream segments vector (<stream>_vect) and
#'      raster (<stream>_rast) maps exported into GRASS location. Only generated if
#'      \code{river} is not set. Default: \code{NULL}.
#' @param points_snap Output: Point vector file of \code{drain_points} snapped to river
#'      exported to GRASS location.
#' @param outlet Integer (row number) defining the catchment outlet in \code{drain_points}.
#'      If there are \code{drain_points} outside the watershed delineated for the
#'      outlet point these will be omitted.
#' @param thresh_stream Integer defining threshold for stream calculation. Raster
#'      cells in accumulation map with values greater than thresh_stream are
#'      considered as streams. Needs to be set only if \code{river} is not set.
#'      Default: \code{NULL}.
#' @param snap_dist Integer defining maximum distance for snapping of \code{drain_points}
#'      to stream segments in units of your GRASS location.
#' @param keep_temp \code{logical}. Set to \code{TRUE} if, in case of an runtime error,
#'      temporary files should be kept in the GRASS location, i.e. for debugging.
#'      Default: \code{FALSE}.
#'      
#' @note Prepare GRASS location and necessary raster files in advance and start
#'      GRASS session in R using \code{\link[spgrass6]{initGRASS}}. Location
#'      should not contain any maps ending on *_t as these will be removed by
#'      calling the function to remove temporary maps.
#'      
#'      You should select your DEM sufficiently large. Otherwise the resulting
#'      catchment might be truncated or boundary influences the calculation
#'      of stream segments etc.
#'      
#'      Check the results (subbasins and snapped points). In case points have been snapped
#'      to the wrong stream segment, adjust point locations manually in GRASS and re-run
#'      the function with the updated locations (use \code{\link[spgrass6]{readVECT6}}
#'      to import the updated drainage points).
#' 
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}
#' 
#' @export
calc_subbas <- function(
  ### INPUT ###
  dem,
  drain_points,
  river=NULL,
  
  ### OUTPUT ###
  basin_out,
  stream=NULL,
  points_snap,
  
  ### PARAMETER ###
  outlet,
  thresh_stream=NULL,
  snap_dist,
  keep_temp=F
  
) {

  tryCatch({
    # remove mask if there is any
    execGRASS("r.mask", flags=c("r"))
    
  ### calc stream segments oder use user defined input
    if(is.null(river)) {
      # GRASS watershed calculation #
      execGRASS("r.watershed", elevation=dem, accumulation="accum_t", drainage="drain_t")
      # calculate stream segments (don't use output of r.watershed as streams should be finer than generated therein)
      execGRASS("r.mapcalculator", amap="accum_t", outfile=paste0(stream, "_rast"), 
                formula=paste("if(abs(A)>", thresh_stream, ",1,0)", sep=""))
      # thin
      execGRASS("r.thin", input=paste0(stream, "_rast"), output=paste0(stream, "_thin_t"))
      # convert to vector
      execGRASS("r.to.vect", input=paste0(stream, "_thin_t"), output=paste0(stream, "_vect"), feature="line")
      river <- paste0(stream, "_vect")
    } else {
      execGRASS("r.watershed", elevation=dem, drainage="drain_t")
    }
    
    
  ### snap given drainage points to streams
    # read stream vector
    streams_vect <- readVECT6(river)
    
    # transform lonlat coordinates of drainage points to CRS of streams_vect
#     coordinates(drain_points) <- c("x", "y")
#     projection(drain_points) <- CRS(projection(river))
  
    # snap points to streams
    drain_points_snap <- snapPointsToLines(drain_points, streams_vect, maxDist=snap_dist)
    if (length(drain_points_snap) < length(drain_points)) stop("Less points after snaping than in drain_points input!\nComputed stream segmets probably are too coarse. Try different parameter values.")
    
    # export drain_points_snap to GRASS
    writeVECT6(drain_points_snap, points_snap)
    
  ### calculate catchments for every drainage point
    # watershed for the defined outlet
    outlet_coords <- coordinates(drain_points_snap)[outlet,]
    execGRASS("r.water.outlet", drainage="drain_t", basin=paste0("basin_outlet_t"),
          easting=as.character(outlet_coords[["X"]]), northing=as.character(outlet_coords[["Y"]]))

    # loop over drainage points
    i <- 0
    for (p in row.names(drain_points_snap)) {
      i <- i+1
      
      if (i == outlet) next
      
      # outlet coordinates
      outlet_coords <- coordinates(drain_points_snap)[as.numeric(p),]
      
      # basin
      execGRASS("r.water.outlet", drainage="drain_t", basin=paste0("basin_", i, "_t"),
                easting=as.character(outlet_coords[["X"]]), northing=as.character(outlet_coords[["Y"]]))
      
      # reclass (subbasins gets number of i for r.patch later)
      execGRASS("r.mapcalculator", amap=paste0("basin_", i, "_t"), outfile=paste0("basin_recl_", i, "_t"),
                formula=paste0("if(A,",i, ")"))
      
    }
    no_catch <- i
  
    
  ### merge all sub-catchments
    # put sub-catcments together
    subcatch_rasts <- execGRASS("g.mlist", type="rast", pattern=paste0("basin_recl_[0-9]*_t"), intern=T)
    subcatch_rasts <- c(subcatch_rasts, "basin_outlet_t")

    # max 30 maps at once, create multiple cross products if necessary
    iterations <- ceiling(length(subcatch_rasts)/30)
    for (j in 1:iterations){
      if (j == iterations) {
        execGRASS("r.cross", input=paste(subcatch_rasts[((j-1)*30+1):length(subcatch_rasts)], collapse=","), output=paste0("basin_cross_", j, "_t"))
        #print(paste0("Iteration ", j,": ", paste(subcatch_rasts[((j-1)*30+1):length(subcatch_rasts)], collapse=",")))
      } else {
        execGRASS("r.cross", input=paste(subcatch_rasts[((j-1)*30+1):(j*30)], collapse=","), output=paste0("basin_cross_", j, "_t"))
        #print(paste0("Iteration ", j,": ", paste(subcatch_rasts[((j-1)*30+1):(j*30)], collapse=",")))
      }
    }
    
    # merge cross products
    cross_rasts <- execGRASS("g.mlist", type="rast", pattern=paste0("basin_cross_[0-9]*_t"), intern=T)
    if(length(cross_rasts) == 1) {
      execGRASS("g.rename", rast=paste(cross_rasts, "basin_all_t", sep=","))
    } else {
      execGRASS("r.cross", input=paste(cross_rasts,collapse=","), output="basin_all_t")
    }
    
    # constrain to catchment of outlet point
    execGRASS("r.mapcalculator", amap="basin_outlet_t", bmap="basin_all_t", outfile=basin_out,
          formula="A*B") 

    # set values of zero to NULL
    execGRASS("r.null", map=basin_out, setnull="0")
    
    no_cross <- length(execGRASS("r.stats", input=basin_out, flags=c("n"), intern=T))
    if(no_catch != no_cross) warning(paste0("Number of categories in ", basin_out, " not equal to number of drainage points!\nThis might be because there are drainage points outside the catcment of the defined outlet. However, you should check the output!"))
    
    # remove temporary maps
    execGRASS("g.mremove", rast="*_t", flags=c("f"))
    

    warning("Finished. 
            Check the results for plausibility (e.g. inaccuracies at snapping of drain_points to streams may occur).
            If manual adjustments are necessary re-run the function for re-calculation of subbasins.")


  # exception handling
  }, error = function(e) {
    
    if(keep_temp == FALSE)
      execGRASS("g.mremove", rast=paste0("*_t,",stream,"_rast"), vect=paste0(stream,"_vect,"), flags=c("f"))
    
    stop(paste(e))  
  })

} # EOF
