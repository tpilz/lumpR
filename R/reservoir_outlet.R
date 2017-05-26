# lumpR/reservoir_outlet.R
# Copyright (C) 2014,2015,2017 Tobias Pilz
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


#' Determine outlet coordinates of reservoirs
#' 
#' Takes a flow accumulation raster (or DEM for computation) and reservoir vector
#' file from a GRASS location and estimates the outlet coordinates of each reservoir
#' polygon.
#' 
#' @param flowacc Flow accumulation raster in GRASS location used for determination
#'      of reservoir outlets (= highest accumulation value). Set to \code{NULL} if
#'      it should be calculated based on a DEM. Default: \code{NULL}.
#' 
#' @param dem Digital elevation model in GRASS location used for calculation of
#'      flow accumulation if no flow accumulation raster is given. Default: \code{NULL}.
#'  
#' @param res_vct Reservoir vector file in GRASS location. For each polygon coordinates
#'      of the outlet (cell with highest flow accumulation) are determined. Needs
#'      at least the column \code{name} as reservoir identifier.
#'      
#' @param outlets_vect Output: Name of vector file of outlet locations to be exported
#'      to GRASS location. If \code{NULL} (default), nothing is exported to GRASS.
#'      
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#'      
#' @return \code{SpatialPoints} object containing outlet position for each reservoir
#'      polygon and vector file \code{outlets_vect} exported to GRASS location.
#'      
#' @note Prepare GRASS location and necessary files in advance and start GRASS
#'      session in R using \code{\link[spgrass6]{initGRASS}}. Location should not
#'      contain any maps ending on *_t as these will be removed by calling the
#'      function to remove temporary maps.
#'      
#'      Check the results by investigating vector file with outlet points written
#'      to GRASS location. Due to small projection inaccuracies between the DEM / flow
#'      accumulation raster and the reservoir vector file, calculated outlet locations
#'      might more or less deviate from true locations!
#'
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}

reservoir_outlet <- function(
  ### INPUT ###
  flowacc = NULL,
  dem = NULL,
  res_vct = NULL,
  
  ### OUTPUT ###
  outlets_vect = NULL,
  
  ### PARAMETERS ###
  keep_temp = FALSE,
  overwrite = FALSE
  
) {
  
  tryCatch({
    
    # Check arguments
    if (is.null(flowacc) & is.null(dem))
      stop("'flowacc' and 'dem' are NULL. One of it must be specified (non-NULL)!")
    # remove mask if any
    x <- execGRASS("r.mask", flags=c("r"), intern = T)
    
    if(is.null(res_vct))
      stop("You have to specify res_vct, the name of the reservoir vector file in the GRASS location to be used as input!")

    
    # remove output of previous function calls if overwrite=T
    if (overwrite) {
      x <- execGRASS("g.mremove", rast=paste0("*_t,",flowacc), vect=paste0("*_t,", outlets_vect), flags=c("f", "b"), intern=T)
    } else {
      # remove temporary maps in any case
      x <- execGRASS("g.mremove", rast="*_t", vect="*_t", flags=c("f", "b"), intern=T)
    }
    
    
    
    # GRASS calculation of flow accumulation
    if (is.null(flowacc) & !is.null(dem)) {
      flowacc <- "accum_t"
      x <- execGRASS("r.watershed", elevation=dem, accumulation=flowacc, intern=T)
    }
    
    # convert reservoir vector to raster
    x <- execGRASS("v.to.rast", input=res_vct, type="area", output="res_rast_t", use="cat", intern=T)
    
    # calculate accumulation for reservoir zones
    x <- execGRASS("r.mapcalculator", amap=flowacc, bmap="res_rast_t", outfile="accum_res_t",
              formula="if(B, A)", intern=T)
    
    # crossproduct of accum_res_t and res_rast_t to have both cats as labels in one raster file
    x <- execGRASS("r.cross", input="res_rast_t,accum_res_t", output="cross_t", intern=T)
    x <- execGRASS("r.null", map="cross_t", setnull="0")
    
    # get statistics into R (accumulation with coordinates of pixels for every reservoir)
    cmd_out <- execGRASS("r.stats", input="cross_t", flags=c("n", "g", "l"), intern = T, ignore.stderr = T)
    cmd_out <- gsub("category |;", "", cmd_out, ignore.case = T)
    cmd_out <- strsplit(cmd_out, " ")
    accums <- do.call(rbind, cmd_out)[,-3]
    mode(accums) <- "numeric"
    colnames(accums) <- c("x", "y", "cat", "accum")
    accums <- as.data.frame(accums)
    
    # get highest accumulation value for every reservoir
    accum_max <- do.call(rbind,lapply(split(accums,accums$cat),function(chunk) chunk[which.max(chunk$accum),]))
    
    
    # to SPDF
    coordinates(accum_max) <- c("x", "y")
    accum_max_spdf <- SpatialPointsDataFrame(coordinates(accum_max), data = data.frame(cat=accum_max@data$cat), proj4string = CRS(getLocationProj()))
    
    # load reservoirs from GRASS
    res_polygon <- readVECT6(res_vct, ignore.stderr = T)
    
    # assign reservoir table to outlet locations table
    res_out <- sp::merge(accum_max_spdf, res_polygon@data, by="cat")
    
    # put into GRASS
    writeVECT6(res_out, outlets_vect, v.in.ogr_flags = "overwrite", ignore.stderr = T)
    
    
    # delete temp
    if(keep_temp == FALSE)
      x <- execGRASS("g.mremove", rast="*_t", vect="*_t", flags=c("f", "b"), intern=T)
    
    
    # return spatial object
    return(res_out)
    
  
  # exception handling
  }, error = function(e) {
    
    x <- execGRASS("r.mask", flags=c("r"), intern = T)
    
    if(keep_temp == FALSE)
      x <- execGRASS("g.mremove", rast=paste0("*_t,",flowacc), vect=paste0("*_t,", outlets_vect), flags=c("f", "b"), intern=T)
    
    stop(paste(e))  
  })
    
} # EOF
