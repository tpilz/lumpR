# lumpR/reservoir_outlet.R
# Copyright (C) 2014,2015,2017,2018 Tobias Pilz
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
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
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
  overwrite = FALSE,
  silent=F
) {
  
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START reservoir_outlet()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")
  
  
  tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  
  # CLEAN UP AND RUNTIME OPTIONS #  
  cmd_out <- execGRASS("g.version", intern=TRUE)
  if (cmd_out=="")
    stop("Couldn't connect to GRASS-session. Try removing any open sinks by calling 'sink()' repeatedly. Or restart R.")
  
  check_raster(dem,"dem")
  
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
  
  
  tryCatch({
    
    # Check arguments
    if (is.null(flowacc) & is.null(dem))
      stop("'flowacc' and 'dem' are NULL. One of it must be specified (non-NULL)!")
    # remove mask if any
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    if(is.null(res_vct))
      stop("You have to specify res_vct, the name of the reservoir vector file in the GRASS location to be used as input!")

    
    # remove output of previous function calls if overwrite=T
    if (overwrite) {
      cmd_out <- execGRASS("g.remove", type="raster,vector", pattern=paste("*_t", outlets_vect, sep=","), flags=c("f", "b"), intern=T)
    } else {
      # remove temporary maps in any case
      cmd_out <- execGRASS("g.remove", type="raster,vector", pattern="*_t", flags=c("f", "b"), intern=T)
    }
    
    
    if(!silent) message("% OK")
    if(!silent) message("%")
    if(!silent) message("% Determine highest flow accumulation value for each reservoir...")
    
    # GRASS calculation of flow accumulation
    if (is.null(flowacc) & !is.null(dem)) {
      flowacc <- "accum_t"
      x <- execGRASS("r.watershed", elevation=dem, accumulation=flowacc, flags="s", intern=T)
    } else
    check_raster(flowacc,"flowacc")
    
    # make sure flowacc is integer
    x <- execGRASS("r.mapcalc", expression=paste0("accum_t = round(", flowacc, ")"), flags=c("overwrite"), intern=T)
    
    # convert reservoir vector to raster
    x <- execGRASS("v.to.rast", input=res_vct, output="res_rast_t", use="cat", intern=T)
    
    # calculate accumulation for reservoir zones
    x <- execGRASS("r.mapcalc", expression=paste0("accum_res_t = if(res_rast_t,", flowacc, ")"), intern=T)
    
    
    # crossproduct of accum_res_t and res_rast_t to have both cats as labels in one raster file
    x <- execGRASS("r.cross", input="res_rast_t,accum_res_t", output="cross_t", intern=T)
    
    # NOTE: categories needed to fix r.cross bug
    cat_res <- execGRASS("r.stats", input="res_rast_t", flags=c("n"), intern=T, ignore.stderr = T)
    cat_acc <- execGRASS("r.stats", input="accum_res_t", flags=c("n"), intern=T, ignore.stderr = T)
    
    # check for and correct error in r.cross, see https://lists.osgeo.org/pipermail/grass-user/2018-February/077934.html
    cmd_out <- execGRASS("r.stats", input="cross_t", flags=c("n"), intern=T, ignore.stderr = T)
    if(any(as.numeric(cmd_out) == 0)) {
      # save category labels
      cat_labs <- execGRASS("r.category", map="cross_t", separator=":", intern=T)
      cat_labs <- strsplit(cat_labs[-1], ":")
      cat_labs <- lapply(cat_labs, function(x) c(as.numeric(x[1]) +1, x[2]))
      # add +1 to categories (destroys labels)
      cmd_out <- execGRASS("r.mapcalc", expression="cross_t=cross_t+1", flags=c("overwrite"), intern=T)
      # get missing category label
      cat_lab_miss <- c(1, paste(c("category"), c(cat_res[1], cat_acc[1]), sep=" ", collapse = "; "))
      # merge to stored labels
      cat_labs_mod <- sapply(c(list(cat_lab_miss), cat_labs), paste, collapse=":")
      # write to grass raster
      write.table(cat_labs_mod, paste(tempdir(), "recl_t.txt", sep="/"), sep="\t", quote=F, row.names = F, col.names = F)
      cmd_out <- execGRASS("r.category", map="cross_t", separator=":", rules=paste(tempdir(), "recl_t.txt", sep="/"), intern = T)
      file.remove(paste(tempdir(), "recl_t.txt", sep="/"))
    }
    
    
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
    
    
    if(!silent) message("% OK")
    if(!silent) message("%")
    if(!silent) message("% Compile output...")
    
    # to SPDF
    coordinates(accum_max) <- c("x", "y")
    accum_max_spdf <- SpatialPointsDataFrame(coordinates(accum_max), data = data.frame(cat=accum_max@data$cat), proj4string = CRS(getLocationProj()))
    
    # load reservoirs from GRASS
    suppressWarnings(res_polygon <- readVECT(res_vct, ignore.stderr = T))
    # WINDOWS PROBLEM: delete temporary file otherwise an error occurs when calling writeVECT or readVECT again with the same (or a similar) file name 
    if(.Platform$OS.type == "windows") {
      dir_del <- dirname(execGRASS("g.tempfile", pid=1, intern=TRUE, ignore.stderr=T))
      files_del <- grep(substr(res_vct, 1, 8), dir(dir_del), value = T)
      file.remove(paste(dir_del, files_del, sep="/"))
    }
    
    # assign reservoir table to outlet locations table
    res_out <- sp::merge(accum_max_spdf, res_polygon@data, by="cat")
    suppressWarnings(proj4string(res_out) <- CRS(getLocationProj()))
    
    # put into GRASS
    suppressWarnings(writeVECT(res_out, outlets_vect, v.in.ogr_flags = c("overwrite", "o"), ignore.stderr = T))
    # WINDOWS PROBLEM: delete temporary file otherwise an error occurs when calling writeVECT or readVECT again with the same (or a similar) file name 
    if(.Platform$OS.type == "windows") {
      dir_del <- dirname(execGRASS("g.tempfile", pid=1, intern=TRUE, ignore.stderr=T))
      files_del <- grep(substr(outlets_vect, 1, 8), dir(dir_del), value = T)
      file.remove(paste(dir_del, files_del, sep="/"))
    }
    
    
    # delete temp
    if(keep_temp == FALSE)
      x <- execGRASS("g.remove", type="raster,vector", pattern="*_t", flags=c("f", "b"), intern=T)
    
    
    if(!silent) message("% OK")
    if(!silent) message("%")
    if(!silent) message("% DONE!")
    if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    # return spatial object
    return(res_out)
    
  
  # exception handling
  }, error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    x <-tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"), intern = T)), error=function(e){})
    
    if(keep_temp == FALSE)
      x <- execGRASS("g.remove", type="raster,vector", pattern=paste("*_t",outlets_vect, sep=","), flags=c("f", "b"), intern=T)
    
    stop(paste(e))  
  })
    
} # EOF
