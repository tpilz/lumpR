# lumpR/prepare_snow_input.R
# Copyright (C) 2014-2026 Till Francke
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


#' Preparation of raster maps necessary for generating snow input for WASA-SED
#' 
#' Takes raster data from a GRASS location (i.e. DEM, subbasins, flowdirection, EHA-map) and calculates mean aspect and relative elevaation for each EHA
#' 
#' @param dem Name of DEM in GRASS location. 
#' @param subbas Name of subbasin raster map (e.g. from output of calc_subbas) in GRASS location.

#' @param eha Name of Elemental Hillslope Areas (EHA) raster map (e.g. output of lump_grass_prep) in GRASS location
#' @param flowdir Name of of flow direction raster map in GRASS
#'      location (e.g. output of lump_grass_prep). Should provide the "aspect" for each cell measured counterclockwise
#'      from East. Multiplying positive values by 45 will give the direction in
#'      degrees that the surface runoff will travel from that cell.
#' @param eha_1d_file Output: Name of file containing properties of \code{EHA}s to be used by \code{\link{prof_class()}}. 
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#' @return Function returns nothing. Output raster files as specified in arguments
#'      (see above) are written into GRASS location.
#'
#' @details This function performs preparatory steps that are necessary when using the snow module in WASA-SED. They can be
#'     skipped if no snow is to be modelled.
#' @note Prepare GRASS location and necessary raster files in advance and start
#'      GRASS session in R using \code{\link[rgrass]{initGRASS}}.
#' 
#'      See GRASS documentation for further information on GRASS functions and
#'      parameters.
#'      
#' 
#' @author Till Francke
#' @export

prepare_snow_input <- function(
  
  ### INPUT ###
  # GRASS raster #
  dem=NULL,
  subbas=NULL,
  eha=NULL,
  flowdir=NULL,
  eha_1d_file=NULL,
  keep_temp=FALSE,
  overwrite=FALSE,
  silent=FALSE
) {
  
  #names of rasters indicating aspect
  aspect.sin = "aspect.sin"
  aspect.cos = "aspect.cos"
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START prepare_snow_input()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")
  
# checks #---------------------------------------------------------------------
  #tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  test_grass()

    if(is.null(dem))
      stop("The name of a DEM within the mapset of your initialised GRASS session has to be given!")
    if(is.null(subbas))
      stop("A name for the subbasin raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(eha))
      stop("A name for the calculated EHA raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(flowdir))
      stop("A name for the calculated flow direction raster map within the mapset of your initialised GRASS session has to be given!")
    check_raster(dem, "dem")
    check_raster(subbas, "subbas")
    check_raster(eha, "eha")
    check_raster(flowdir, "flowdir")

    
# CLEAN UP AND RUNTIME OPTIONS #-----------------------------------------------
  # suppress annoying GRASS outputs
  tmp_file <- file(tempfile(), open="wt")
  sink(tmp_file, type="output")
  
  # also suppress warnings in silent mode
  if(silent){
    tmp_file2 <- file(tempfile(), open="wt")
    sink(tmp_file2, type="message")
    oldw <- getOption("warn")
    options(warn = -1)
  }
  
  options(error=cleanup) #in case of errors, clean up and reset to original warning and messaging state
  
  # remove output of previous function calls if overwrite=T (remove only relevant maps according to things2do)
  if (overwrite) {
      cmd_out <- execGRASS("g.remove", type="raster", pattern=paste("*_t,*_t1,*_t2,*_t3", aspect.sin, aspect.cos, sep=","), flags=c("f", "b"), intern=T)
  } else {
    # remove temporary maps in any case
    cmd_out <- execGRASS("g.remove", type="raster", pattern="*_t,*_t1,*_t2,*_t3", flags=c("f", "b"), intern=T)
    if (check_raster(map=aspect.cos, argument_name=aspect.cos, raiseerror=FALSE))
      stop(paste0("Raster map ", aspect.cos, " already existing. Use overwrite=TRUE to overwrite."))
    if (check_raster(map=aspect.sin, argument_name=aspect.sin, raiseerror=FALSE))
      stop(paste0("Raster map ", aspect.sin, " already existing. Use overwrite=TRUE to overwrite."))
  }

  if(!silent) message("% OK")


### CALCULATIONS ###-----------------------------------------------------------
  
# aspect #------------------------------------------------------------------------
  if (TRUE) {
      if(!silent) message("%")
      if(!silent) message("% Compute aspect-related rasters...")
      
      # remove mask if there is any (and ignore error in case there is no mask)
      tt=tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"), intern=TRUE)), error=function(e){})
      
      # set mask and region
      cmd_out <- execGRASS("r.mask", raster=subbas, intern = T)

      # compute aspect-related values for each EHA ####
      #generate reclass rules file
      reclass_rules = data.frame(from=seq(-8,8))
      reclass_rules$degrees = reclass_rules$from * 45
      tmp_file = tempfile()
      
      #aspect.sin
      reclass_rules$to = round(sin(reclass_rules$degrees/360*2*pi), digits=3)
      write.table(reclass_rules[,c("from","from","to","to")], file=tmp_file, sep=":", col.names = FALSE, row.names=FALSE, quote=FALSE)
      cmd_out <- execGRASS("r.recode", input=flowdir, output=aspect.sin, 
                           rules=tmp_file, flags="overwrite", intern=TRUE)
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.recode:", paste0(cmd_out, collapse="\n"))))

      #aspect.cos
      reclass_rules$to = round(cos(reclass_rules$degrees/360*2*pi), digits=3)
      write.table(reclass_rules[,c("from","from","to","to")], file=tmp_file, sep=":", col.names = FALSE, row.names=FALSE, quote=FALSE)
      cmd_out <- execGRASS("r.recode", input=flowdir, output=aspect.cos, 
                           rules=tmp_file, flags="overwrite", intern=TRUE)
      
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.recode:", paste0(cmd_out, collapse="\n"))))
      
      #perform zonal statistics using raster eha on the aspect-related  maps
      cmd_out <- execGRASS("r.univar", map=aspect.sin, zones=eha, 
                           separator="tab",
                           output=tmp_file, 
                           flags=c("overwrite", "t"), intern=TRUE)
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.univar:", paste0(cmd_out, collapse="\n"))))
      
      #read generated output file
      file_content = read.table(file=tmp_file, header=TRUE, sep="\t")[, c("zone", "mean")]
      colnames(file_content) = c("eha_id", "aspect.sin")
      eha_means = file_content
      
      cmd_out <- execGRASS("r.univar", map=aspect.cos, zones=eha, 
                           separator="tab",
                           output=tmp_file, 
                           flags=c("overwrite", "t"), intern=TRUE)
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.univar:", paste0(cmd_out, collapse="\n"))))
      file_content = read.table(file=tmp_file, header=TRUE, sep="\t")[, c("zone", "mean")]
      colnames(file_content) = c("eha_id", "aspect.cos")
      eha_means = merge(eha_means, file_content)
      
      #r.univar [-getr] map=name[,name,...] [zones=name] [output=name] [percentile=float[,float,...]] [nprocs=integer] [separator=character] [--overwrite] [--help] [--verbose] [--quiet] [--ui]
      #r.stats.zonal for spatial output
      
      # compute relative elevation values for each EHA ####
      if(!silent) message("% Compute relative elevation of EHAs...")
      
      #get subbasin for each EHA
      cmd_out <- execGRASS("r.stats", input=paste(eha, subbas, sep=","), 
                           separator="tab", output=tmp_file, flags=c("overwrite", "n"), intern=TRUE)
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.stats:", paste0(cmd_out, collapse="\n"))))
      file_content = read.table(file=tmp_file, header=FALSE, sep="\t")
      colnames(file_content) = c("eha_id", "subbas_id")
      eha_means = merge(eha_means, file_content)
      
      #compute "mean" elevation of each subbasin by extracting the elevation of its centroids
        # #compute centroids of subbasins
        # cmd_out <- execGRASS("r.centroids", input=subbas, output="subbas_centroids_t", 
        #                      flags=c("overwrite"), intern=TRUE)
        # if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.univar:", paste0(cmd_out, collapse="\n"))))
        # 
        # #extract raster values of DEM at centroid points
        # cmd_out <- execGRASS("r.what", map=dem, points="subbas_centroids_t", 
        #                      separator="tab", output=tmp_file, flags=c("n", "v", "overwrite"), intern=TRUE)
        # if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.what:", paste0(cmd_out, collapse="\n"))))
        # file_content = read.table(file=tmp_file, header=TRUE, sep="\t")[, c("cat", dem)]
      
      #use mean elevation of each subbasin (perform zonal statistics using raster subbas on the DEM)
      cmd_out <- execGRASS("r.univar", map=dem, zones=subbas, 
                           separator="tab",
                           output=tmp_file, 
                           flags=c("overwrite", "t"), intern=TRUE)
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.univar:", paste0(cmd_out, collapse="\n"))))
      file_content = read.table(file=tmp_file, header=TRUE, sep="\t")[, c("zone", "mean")]
      colnames(file_content) = c("subbas_id", "subbas_alt")
      eha_means = merge(eha_means, file_content)

      #perform zonal statistics using raster eha on the DEM
      cmd_out <- execGRASS("r.univar", map=dem, zones=eha, 
                           separator="tab",
                           output=tmp_file, 
                           flags=c("overwrite", "t"), intern=TRUE)
      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.univar:", paste0(cmd_out, collapse="\n"))))
      
      #read generated output file
      file_content = read.table(file=tmp_file, header=TRUE, sep="\t")[, c("zone", "mean")]
      colnames(file_content) = c("eha_id", "mean_alt")
      eha_means = merge(eha_means, file_content)
      #compute relative elevation
      eha_means$rel_alt = eha_means$mean_alt - eha_means$subbas_alt
      
      #write eha_means to file
      write.table(round(eha_means[, c("eha_id", "aspect.sin",	"aspect.cos", "rel_alt")], digits=2), file=file.path(eha_1d_file), sep="\t", row.names=FALSE, quote=FALSE)
      
      if(!silent) message(paste0("% Result file ", eha_1d_file," written. Finished."))
      # if an error occurs delete all temporary output
    
  } # 
     
  
 
  # remove temp files (e.g. "un-labelled" rasters)
  if(keep_temp == FALSE)
    cmd_out <- execGRASS("g.remove", type="raster", pattern=paste("*_t,*_t1,*_t2,*_t3", sep=","), flags=c("f", "b"), intern = T)
  
  if(!silent) message("%")
  if(!silent) message("% DONE!")
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  
  
  # stop sinking
  closeAllConnections()
  
  # restore original warning mode
  if(silent)
    options(warn = oldw)
    
} # EOF

