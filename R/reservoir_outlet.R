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
#'      at least the column \code{res_id} (integer) as reservoir identifier.
#'      
#' @param outlets_vect Output: Name of vector file of outlet locations to be exported
#'      to GRASS location.
#'      
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#'      
#' @return Function returns nothing. \bold{WARNING:} Up to version 2.5.0 this function
#' returned a \code{SpatialPoints} object with the outlet coordinates.
#'      
#' @note Prepare GRASS location and necessary files in advance and start GRASS
#'      session in R using \code{\link[rgrass7]{initGRASS}}. Location should not
#'      contain any maps ending on *_t as these will be removed by calling the
#'      function to remove temporary maps.
#'      
#'      Attribute table of \code{res_vct} must be stored in a SQL database to copy
#'      the attribute table to \code{outlets_vect}. Furthermore, \code{res_vct}
#'      must not contain a column \code{cat_new} as this will be the new identifier
#'      column for \code{outlets_vect}.
#'      
#'      Check the results by investigating vector file with outlet points written
#'      to GRASS location. Due to small projection inaccuracies between the DEM / flow
#'      accumulation raster and the reservoir vector file, calculated outlet locations
#'      might more or less deviate from true locations!
#'
#'      Can be run before \code{\link[lumpR]{reservoir_strategic}}. 

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
  
  # check arguments and input
  if(is.null(flowacc) & is.null(dem))
    stop("Either argument 'dem' or 'flowacc' has to be specified!")
  if(!is.null(dem)) check_raster(dem,"dem")
  if(!is.null(flowacc)) check_raster(flowacc,"flowacc")
  check_vector(res_vct, "res_vct")
  if(is.null(outlets_vect))
    stop("Argument 'outlets_vect' needs to be specified!")
  
  #check for necessary columns in vector file
  cmd_out <- execGRASS("v.info", map=res_vct, flags=c("c", "e"), intern=T)
  if (!any(grepl(cmd_out, pattern="INTEGER\\|res_id")))
    stop(paste0("Reservoir vector file '",res_vct,"' has no column 'res_id' (INTEGER), please add it."))
  # if (!any(grepl(cmd_out, pattern="CHARACTER\\|name")))
  #   stop(paste0("Reservoir vector file '",res_vct,"' has no column 'name' (CHARACTER), please add it."))
  
  
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
    
    # remove mask if any
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
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
    if (is.null(flowacc)) {
      flowacc <- "accum_t"
      x <- execGRASS("r.watershed", elevation=dem, accumulation=flowacc, flags="s", intern=T)
    }
    
    # check if flowacc is integer
    cmd_out <- execGRASS("r.info", map=flowacc, flags="g", intern=T)
    type_line=which(grepl(cmd_out, pattern = "datatype=")) #search for line holding data type
    data_type=strsplit(cmd_out[type_line], split = "=")[[1]][2]
    if (data_type=="CELL") #if flowaccum is already integer, use it. Otherwise, create interger variant
      x <- execGRASS("g.copy", raster=paste0(flowacc,",accum_t"), flags=c("overwrite"), intern=T) else
      x <- execGRASS("r.mapcalc", expression=paste0("accum_t = round(", flowacc, ")"), flags=c("overwrite"), intern=T)
    
    x <- execGRASS("g.copy", vector=paste0(res_vct,",resv_t"), flags=c("overwrite"), intern=T) #working copy of reservoir outlines

    #find maximum value of flowaccumulation for each reservoir - this should be the outlet
    x <- execGRASS("v.rast.stats", map="resv_t", raster=flowacc, method="max", column_prefix="fa", intern=TRUE)
    
    #convert map of max_flow_accum values to raster
    x <- execGRASS("v.to.rast", input="resv_t", output="max_flowaccum_t", use="attr", attribute_column="fa_maximum", flags="overwrite", intern=TRUE)
    
    #intersect map of flowaccum and max values of flowaccum to precisely locate cells
    x <- execGRASS("r.mapcalc",  expression="outlet_cells_t=if(accum_t==max_flowaccum_t,1,null())", flags="overwrite", intern=TRUE)
    
    #convert outlet points to vector
    x <- execGRASS("r.to.vect", input="outlet_cells_t", type="point", output=outlets_vect, flags=c("overwrite", "t"), intern=TRUE)
    
    #add subbasin-ID to outlet cells
    x <- execGRASS("v.db.addtable", map=outlets_vect, key = "cat_new", intern=TRUE) 
    x <- execGRASS("v.db.addcolumn", map=outlets_vect, columns="res_id int", intern=TRUE) 
    x <- execGRASS("v.what.vect", map=outlets_vect, column="res_id", query_map="resv_t", query_column="res_id", intern=TRUE)  
    #x <- execGRASS("v.what.vect", map=outlets_vect, column="name",   query_map="resv_t", query_column="name", intern=TRUE)  
    
    # preserve attribute table of res_vct
    x <- execGRASS("v.db.dropcolumn", map = "resv_t", columns = "fa_maximum", intern=T)
    x <- execGRASS("v.db.join", map = outlets_vect, column = "res_id", other_table = "resv_t", other_column = "res_id", intern=T)
    
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
