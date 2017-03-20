# lumpR/reservoir_lumped.R
# Copyright (C) 2016-2017 Tobias Pilz
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

#' Generation of WASA parameter files for simulation of lumped reservoirs
#' 
#' Function generates the WASA parameter files lake.dat and lake_number.dat from
#' a pre-processed reservoir vector and a subbasin raster file stored in a GRASS
#' location.
#' 
#' @param res_vect Name of reservoir vector map in GRASS location. Should be point
#'      instead of polygon feature (i.e. reservoir outlet locations; consider function
#'      \code{\link[lumpR]{reservoir_outlet}})! Needs at least
#'      either column 'volume' with information on volume in [m^3] or column 'area'
#'      with information on lake area in [m^2] in the attribute table.
#' @param sub_rast Name of subbasin raster map in GRASS location. Can be created with
#'      \code{\link[lumpR]{calc_subbas}}.
#' @param res_vect_class Output: Name for the vector reservoir map created in GRASS
#'      location containing information on reservoir size classes in the attribute
#'      table. If \code{NULL} (default) it will not be created.
#' @param dir_out Character string specifying output directory (will be created if
#'      not available and files will be overwritten if \code{overwrite = TRUE}.
#' @param lake_file Output: WASA file of parameters for the reservoir size classes.
#'      See \code{Details}.
#' @param lakenum_file Output: WASA file containing specification of total number
#'      of reservoirs in the size classes for a specific subbasin. See \code{Details}.
#' @param res_param A \code{data.frame} object containing parameters for the reservoir
#'      size classes. Given standard parameter set adjusted to semi-arid Brazil.
#'      See \code{Details}.
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#' 
#' @note Prepare GRASS location and necessary spatial objects in advance and start
#'      GRASS session in R using \code{\link[spgrass6]{initGRASS}}.
#'      
#' @details This function creates the mandatory WASA input files needed to run the model
#'      with option \code{doacudes}.
#'      
#'      The given standard parameter set was estimated by Molle (1989) for the semi-arid
#'      part of Brazil and needs to be adjusted if applied to some other region!
#'      
#'      \bold{lake_file} / \bold{res_param}\cr
#'      Specification of parameters for the reservoir size classes. Note same order for
#'      \code{lake_file} and \code{res_param} but different header names! If information
#'      on 'maxlake0' / 'vol_max' is not available, give 'area_max', i.e. the maximum area
#'      of reservoir size classes in \emph{m^2}. This is internally converted to volume
#'      by relationship of Molle (1989) using parameters 'alpha_Molle' and 'damk_Molle'.
#'      
#'      \emph{Reservoir_class-ID / class}\cr
#'      ID of reservoir size class.
#'      
#'      \emph{maxlake0 / vol_max}\cr
#'      Upper limit of reservoir size class in terms of volume in \emph{m^3}.
#'      
#'      \emph{lake_vol0_factor / f_vol_init}\cr
#'      Fraction of storage capacity that indicates the initial water volume in the
#'      reservoir size classes (\emph{dimensionless}).
#'      
#'      \emph{lake_change / class_change}\cr
#'      Factor that indicates yearly variation in the number of reservoirs of the size
#'      classes (\emph{dimensionless}).
#'      
#'      \emph{alpha_Molle, damk_Molle}\cr
#'      Parameters of the area-volume relationship in the reservoir size classes:
#'      Area = alpha_Molle * damk_Molle * (Volume / damk_Molle)^( (alpha_Molle - 1) / alpha_Molle).
#'      Unit of Area: \emph{m^2}, unit of Volume: \emph{m^3}.
#'      
#'      \emph{damc_hrr, damd_hrr}\cr
#'      Parameters of the spillway rating curve in the reservoir size classes:
#'      Overflow = damc_hrr * Height^(damd_hrr). 
#'      Unit of Overflow: \emph{m^3/s}, unit of Height (over spillway): \emph{m}. 
#'      
#'      
#'      \bold{lakenum_file}\cr
#'      Specification of total number of reservoirs for the size classes for a specific
#'      subbasin.
#'      
#'      \emph{Subasin-ID}\cr
#'      Subbasin ID.
#'      
#'      \emph{acud}\cr
#'      Total number of reservoirs in the size classes.
#'      
#'      
#' @references 
#'      WASA model in general:\cr
#'      Guentner, A. (2002): Large-scale hydrological modelling in the semi-arid 
#'      North-East of Brazil. \emph{PIK Report 77}, Potsdam Institute for Climate
#'      Impact Research, Potsdam, Germany.
#'      
#'      Reservoir module of the WASA model:\cr
#'      Mamede, G. L. (2008):  Reservoir sedimentation in dryland catchments: Modeling
#'      and management. PhD Thesis, University of Potsdam, Germany.
#'      
#'      Reservoir parameter set herein given as standard values:\cr
#'      Molle, F. (1989): Evaporation and infiltration losses in small reservoirs.
#'      \emph{Serie Hydrologia}, 25, SUDENE / ORSTOM, Recife, Brazil, in Portuguese.
#'      
#' 
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}
#'
#' @export

reservoir_lumped <- function(
  # INPUT #
  res_vect=NULL,
  sub_rast=NULL,
  # OUTPUT #
  res_vect_class=NULL,
  dir_out="./",
  lake_file="lake.dat",
  lakenum_file="lake_number.dat",
  # PARAMETERS #
  res_param=data.frame(class=1:5,
                       vol_max=c(5000,25000,50000,100000,250000),
                       f_vol_init=0.2,
                       class_change=0,
                       alpha_Molle=2.7,
                       damk_Molle=1500,
                       damc_hrr=c(7,14,21,28,35),
                       damd_hrr=1.5),
  keep_temp=F,
  overwrite=F,
  silent=F
) {
  
### PREPROCESSING ###
  
  # CHECKS #
  
  # spatial input from GRASS location
  if(is.null(res_vect))
    stop("The name of a reservoir vector file 'res_vect' within the mapset of your initialised GRASS session has to be given!")
  if(is.null(sub_rast))
    stop("The name of a subbasin raster file 'sub_rast' within the mapset of your initialised GRASS session has to be given!")
  if(is.null(res_vect_class))
    warning("Classified reservoir point vector file 'res_vect_class' will NOT be created!")
  
  # check 'res_param'
  if(!is.data.frame(res_param))
    stop("'res_param' has to be a data.frame!")
  if(is.null(res_param$area_max) & is.null(res_param$vol_max))
    stop("'res_param' needs column 'area_max' or 'vol_max' to be given!")
  if(is.null(res_param$class))
    stop("'res_param' needs column 'class' to be given!")
  if(is.null(res_param$f_vol_init))
    stop("'res_param' needs column 'f_vol_init' to be given!")
  if(is.null(res_param$class_change))
    stop("'res_param' needs column 'class_change' to be given!")
  if(is.null(res_param$alpha_Molle))
    stop("'res_param' needs column 'alpha_Molle' to be given!")
  if(is.null(res_param$damk_Molle))
    stop("'res_param' needs column 'damk_Molle' to be given!")
  if(is.null(res_param$damc_hrr))
    stop("'res_param' needs column 'damc_hrr' to be given!")
  if(is.null(res_param$damd_hrr))
    stop("'res_param' needs column 'damd_hrr' to be given!")
  
  # check that reservoir vector file has column 'volume' or 'area'
  cmd_out <- execGRASS("v.info", map=res_vect, flags=c("c"), intern=T, ignore.stderr = T)
  cmd_out <- unlist(strsplit(cmd_out, "|", fixed=T))
  cols <- grep("area|volume", cmd_out, value=T)
  if(length(cols) == 0)
    stop("Attribute table of input vector 'res_vect' needs column 'area' (in m^2) OR (preferrably) 'volume' (in m^3)!")
  
  # calculate vol_max for 'res_param' if not available
  if(is.null(res_param$vol_max))
    res_param$vol_max <- (res_param$area_max / (res_param$alpha_Molle * res_param$damk_Molle))^( res_param$alpha_Molle / (res_param$alpha_Molle - 1) ) * res_param$damk_Molle

  
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
    x <- suppressWarnings(execGRASS("r.mask", flags=c("r"), intern=T))
    
    # create output dir
    dir.create(dir_out, recursive=T, showWarnings=F)
    
    # check output directory
    if (!overwrite & (file.exists(paste(dir_out,lake_file,sep="/")) |
                      file.exists(paste(dir_out,lakenum_file,sep="/"))) )
      stop(paste0("Output file(s) ", lake_file, " and/or ",lakenum_file, " already exist(s) in ", dir_out, "!"))
    
    
    # remove output of previous function calls if overwrite=T
    if (overwrite) {
      execGRASS("g.mremove", rast=paste0("*_t"), vect=paste0("*_t,", res_vect_class), flags=c("f", "b"))
    } else {
      # remove temporary maps in any case
      execGRASS("g.mremove", rast="*_t", vect="*_t", flags=c("f", "b"))
    }
    
      
    
    # GROUP RESERVOIRS INTO SIZE CLASSES #
    message("\nGroup reservoirs into size classes...\n")
    
    if(any(grepl("volume", cols))) { # volume information is available
      
      # convert vector to raster data for processing
      execGRASS("v.to.rast", input=res_vect, output="reservoirs_lump_vol_t", column="volume")
      
      # loop over reservoir size classes to create formula string for r.mapcalculator
      res_param_t <- rbind(0, res_param)
      res_param_order <- order(res_param_t$vol_max)
      expr <- NULL
      for(s in 1:nrow(res_param)) {
        expr_t <- paste0("if( A > ", sprintf(res_param_t$vol_max[res_param_order[s]], fmt="%f"), " && A < ", sprintf(res_param_t$vol_max[res_param_order[s+1]], fmt="%f"), ", ", res_param_t$class[res_param_order[s+1]], ", ")
        expr <- paste0(expr, expr_t)
      }
      expr <- paste0(expr, "0", paste0(rep(")", nrow(res_param)), collapse = ""))
      
      # apply classification
      execGRASS("r.mapcalculator", amap="reservoirs_lump_vol_t", outfile="reservoirs_classes_t", formula=expr)
      
    } else { # only area information is available
      
      # convert vector to raster data for processing
      execGRASS("v.to.rast", input=res_vect, output="reservoirs_lump_area_t", column="area")
      
      # calculate maximum areas from volume in res_param
      res_param_area <- res_param$alpha_Molle * res_param$damk_Molle * (res_param$vol_max / res_param$damk_Molle)^( (res_param$alpha_Molle - 1) / res_param$alpha_Molle )
      
      # loop over reservoir size classes to create formula string for r.mapcalculator
      res_param_t <- cbind(res_param, area=res_param_area)
      res_param_t <- rbind(0, res_param_t)
      res_param_order <- order(res_param_t$area)
      expr <- NULL
      for(s in 1:nrow(res_param)) {
        expr_t <- paste0("if( A > ", sprintf(res_param_t$area[res_param_order[s]], fmt="%f"), " && A < ", sprintf(res_param_t$area[res_param_order[s+1]], fmt="%f"), ", ", res_param_t$class[res_param_order[s+1]], ", ")
        expr <- paste0(expr, expr_t)
      }
      expr <- paste0(expr, "0", paste0(rep(")", nrow(res_param)), collapse = ""))
      
      # apply classification
      execGRASS("r.mapcalculator", amap="reservoirs_lump_area_t", outfile="reservoirs_classes_t", formula=expr)
      
    }
    
    # check result for reservoir being larger than given maximum and give warning if necessary
    cmd_out <- execGRASS("r.stats", input="reservoirs_classes_t", fs=",", flags=c("n", "c"), intern=T, ignore.stderr = T)
    cmd_out <- matrix(as.numeric(unlist(strsplit(cmd_out, ","))), ncol=2, byrow = T)
    if(any(cmd_out[,1] == 0))
      warning(paste0("There are ", cmd_out[which(cmd_out[,1]==0),2], " reservoirs larger than largest given reservoir class which will be ignored!\n",
                     "These are candidates for strategic reservoir types. If you intended to handle them in a lumped manner as well you should adjust the size class difinitions in 'res_param' accordingly."))
    
    # output vector file (classified reservoirs)
    execGRASS("r.null", map="reservoirs_classes_t", setnull="0")
    if(!is.null(res_vect_class))
      execGRASS("r.to.vect", input="reservoirs_classes_t", output=res_vect_class, feature="point")
    
    
    
    # CREATE OUTPUT FILES #
    message("\nCreate output files...\n")
    
    # lake.dat from 'res_param'
    write("Specification of parameters for the reservoir size classes", paste(dir_out, lake_file, sep="/"))
    write("Reservoir_class-ID, maxlake0[m**3], lake_vol0_factor[-], lake_change[-], alpha_Molle[-], damk_Molle[-], damc_hrr[-], damd_hrr[-]", paste(dir_out, lake_file, sep="/"), append=T)
    dat_out <- data.frame(res_param$class, res_param$vol_max, res_param$f_vol_init, res_param$class_change,
                          res_param$alpha_Molle, res_param$damk_Molle, res_param$damc_hrr, res_param$damd_hrr)
    write.table(format(dat_out, scientific=F), paste(dir_out, lake_file, sep="/"), sep="\t", quote=F, append=T, row.names = F, col.names = F)
    
    
    # lake_number.dat from classified reservoirs
    # loop over subbasins
    subbas <- as.numeric(execGRASS("r.stats", input=sub_rast, fs=",", flags=c("n"), intern=T, ignore.stderr = T))
    lake_num_out <- matrix(0, nrow=length(subbas), ncol=nrow(res_param)+1)
    for(s in 1:length(subbas)) {
      
      # remove mask
      x <- suppressWarnings(execGRASS("r.mask", flags=c("r"), intern=T))
      
      # subbasin mask
      x <- execGRASS("r.mapcalculator", amap=sub_rast, outfile="sub_mask_t", formula=paste0("if(A==", subbas[s], ", 1, null())"), flags = c("overwrite"), intern=T)
      x <- suppressWarnings(execGRASS("r.mask", input="sub_mask_t", flags=c("o"), intern=T))
      
      # statistics of classified reservoirs in mask region
      cmd_out <- execGRASS("r.stats", input="reservoirs_classes_t", fs=",", flags=c("n", "c"), intern=T, ignore.stderr = T)
      res_stats <- matrix(as.numeric(unlist(strsplit(cmd_out, ","))), ncol=2, byrow = T)
      
      # output data object
      lake_num_out[s, c(1,res_stats[,1]+1)] <- c(subbas[s], res_stats[,2])
    }
    
    # remove mask
    x <- suppressWarnings(execGRASS("r.mask", flags=c("r"), intern=T))
    
    # write output
    write("Specification of total number of reservoirs in the size classes", paste(dir_out, lakenum_file, sep="/"))
    write("Sub-basin-ID, acud[-] (five reservoir size classes)", paste(dir_out, lakenum_file, sep="/"), append=T)
    write.table(lake_num_out, paste(dir_out, lakenum_file, sep="/"), append=T, quote=F, sep="\t", row.names = F, col.names = F)
    
    
    
    
    # remove temporary maps
    if(keep_temp == FALSE)
      execGRASS("g.mremove", rast="*_t", flags=c("f"))
    
    
    
    message("\nFinished.\n")
    
    
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
        execGRASS("g.mremove", rast=paste0("*_t"), vect=paste0(res_vect_class), flags=c("f", "b"))
    
    stop(paste(e))  
  })
    
} # EOF
