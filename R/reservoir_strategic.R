# lumpR/reservoir_strategic.R (built upon former LUMP/wasa_reservoir_par.R)
# Copyright (C) 2015-2018 Tobias Pilz
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


#' Generation of WASA parameter file reservoir.dat
#' 
#' Function generates the WASA parameter file reservoir.dat from a pre-processed
#' reservoir vector file stored in a GRASS location.
#' 
#' @param res_vect Name of reservoir vector map in GRASS location. Should be point
#'      instead of polygon feature (i.e. reservoir outlet locations; consider function
#'      \code{\link[lumpR]{reservoir_outlet}})! For mandatory columns in attribute table
#'      see \code{Details}.
#' @param subbasin Subbasin raster map in GRASS location. Can be created with
#'      \code{\link[lumpR]{calc_subbas}}.
#' @param dir_out Character string specifying output directory (will be created if it
#'      does not yet exist).
#' @param reservoir_file Output: WASA file of parameters for the strategic reservoirs
#'      assigned to subbasins.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#'      
#' @details For each reservoir that should be modelled explicitly within WASA the
#'      following information need to be collected and written into the vector file's
#'      attrbute table (order is not important):
#'      
#'      \emph{name}\cr
#'      Reservoir identifier, e.g. the name of the reservoir.
#' 
#'      \emph{minlevel}\cr
#'      Initial minimum level in the reservoir [m]. Value varies because of sediment
#'      accumulation.
#'      
#'      \emph{maxlevel}\cr
#'      Maximum water level in the reservoir [m].
#'      
#'      \emph{vol0}\cr
#'      Initial volume of the reservoir [10^3 m^3]. Value varies because of sediment
#'      accumulation. Set to '-999' if information is not available.
#'      
#'      \emph{storecap}\cr
#'      Initial storage capacity of the reservoir [10^3 m^3]. Value varies because of
#'      sediment accumulation.
#'      
#'      \emph{damflow}\cr
#'      Target outflow discharge of the reservoir (90 \% reliability) [m^3/s].
#'      
#'      \emph{damq_frac}\cr
#'      Fraction of Q90 released from the reservoir in regular years [-].
#'      
#'      \emph{withdrawal}\cr
#'      Water withdrawal discharge from the reservoir to supply the water use sectors 
#'      [m^3/s]. Outflow discharge through the dam is not considered.
#'      
#'      \emph{damyear}\cr
#'      Year of construction of the dam (YYYY).
#'      
#'      \emph{maxdamarea}\cr
#'      Initial maximum area of the reservoir [ha]. Value varies because of sediment
#'      accumulation.
#'      
#'      \emph{damdead}\cr
#'      Initial dead volume of the reservoir [10^3 m^3]. Value varies because of
#'      sediment accumulation.
#'      
#'      \emph{damalert}\cr
#'      Initial alert volume of the reservoir [10^3 m^3]. Value varies because of
#'      sediment accumulation.
#'      
#'      \emph{dama, damb}\cr
#'      Parameters of the area-volume relationship in the reservoir:
#'      area = dama * Vol^damb [-]. Values of reservoir area and volume are
#'      expressed in m^2 and m^3, respectively.
#'      
#'      \emph{q_outlet}\cr
#'      Maximum outflow discharge released through the bottom outlets of the
#'      reservoir [m^3/s].
#'      
#'      \emph{fvol_botm}\cr
#'      Fraction of storage capacity that indicates the minimum storage volume for
#'      sediment release through the bottom outlets of the reservoir [-].
#'      
#'      \emph{fvol_over}\cr
#'      Fraction of storage capacity that indicates the minimum storage volume for
#'      water release through the spillway of the reservoir [-].
#'      
#'      \emph{damc, damd}\cr
#'      Parameters of the spillway rating curve of the reservoir: Qout = damc * Hv^damd
#'      [-]. Values of water height over the spillway and overflow discharges are
#'      expressed in m and m^3/s, respectively.
#'      
#'      \emph{elevbottom}\cr
#'      Bottom outlet elevation of the reservoir [m].
#'      
#' @note In output file \code{reservoir_file} order of subbasin IDs needs to be compliant
#'      with order in hymo.dat, i.e. some manual post-processing might be necessary!
#'      Otherwise WASA will terminate with an error.
#'      
#'      If you applied \code{\link[lumpR]{reservoir_outlet}} using the outlet locations
#'      of strategic reservoirs as drainage points, it might be necessary to use
#'      locations of the function's output \code{points_processed} instead of the
#'      true reservoir outlet locations as otherwise the reservoirs might get assigned to
#'      the wrong subbasins!
#'      
#' @references 
#'      lumpR package introduction with literature study and sensitivity analysis:\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#'
#'      WASA model in general:\cr
#'      Guentner, A. (2002): Large-scale hydrological modelling in the semi-arid 
#'      North-East of Brazil. \emph{PIK Report 77}, Potsdam Institute for Climate
#'      Impact Research, Potsdam, Germany.
#'      
#'      Reservoir module of the WASA model:\cr
#'      Mamede, G. L. (2008):  Reservoir sedimentation in dryland catchments: Modeling
#'      and management. PhD Thesis, University of Potsdam, Germany.
#'      
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}

reservoir_strategic <- function(
  ### INPUT ###
  res_vect=NULL,
  subbasin=NULL,
  ### OUTPUT ###
  dir_out="./",
  reservoir_file="reservoir.dat",
  ### PARAMETER ###
  overwrite=F,
  silent=F
) {
  
### PREPROCESSING ###----------------------------------------------------------
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START reservoir_strategic()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")
  
  # CHECKS #
  
  tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  
  # spatial input from GRASS location
  if(is.null(res_vect))
    stop("The name of a reservoir vector file 'res_vect' within the mapset of your initialised GRASS session has to be given!")
  if(is.null(subbasin))
    stop("The name of a subbasin raster file 'subbasin' within the mapset of your initialised GRASS session has to be given!")
  
  
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
  
  
  
### CALCULATIONS ###-----------------------------------------------------------
  tryCatch({
    
    # remove mask if there is any
    tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"))), error=function(e){})
    
    # create output dir
    dir.create(dir_out, recursive=T, showWarnings=F)
    
    # check output directory
    if (!overwrite & (file.exists(paste(dir_out,reservoir_file,sep="/"))) )
      stop(paste0("Output file ", reservoir_file, " already exists in ", dir_out, "!"))
    
    # get reservoir data
    res <- readVECT(res_vect)
    
    # make sure column names are in in lowercase only
    colnames(res@data) <- tolower(colnames(res@data))
    
    # check existence of necessary columns in attribute table
    cols_mandatory <- c("name", "minlevel", "maxlevel", "vol0", "storecap", "damflow", "damq_frac", 
                        "withdrawal", "damyear", "maxdamarea", "damdead", "damalert", "dama", "damb", 
                        "q_outlet", "fvol_botm", "fvol_over", "damc", "damd", "elevbottom")
    chk_cols <- grepl(paste(colnames(res@data), collapse="|^"), cols_mandatory)
    if(any(!chk_cols))
      stop(paste0("Check attribute table of 'res_vect', column(s) ", paste(cols_mandatory[!chk_cols], collapse=", "), " could not be found!"))
    
    # get subbasin values
    sub_rast <- raster(readRAST(subbasin))
    
    
    
    
    if(!silent) message("% OK")
    if(!silent) message("%")
    if(!silent) message("% Assignment of reservoirs to subbasins...")

    # get subbasin no for each outlet point
    res_sub_all <- extract(sub_rast, res, df=T)
    names(res_sub_all)[2] <- "sub_id_new"
    
    # check for duplicates (multiple outlet points per subbasin)
    dupl <- duplicated(res_sub_all[[2]], incomparables=c(NA))
    if (any(dupl))
      stop(paste0("Subbasin(s) no. ", paste(unique(res_sub_all[dupl,"sub_id_new"]), collapse=", "), " contain(s) multiple strategic reservoirs!"))
    
    # combine subbasin information with reservoir parameter data
    res_dat <- res@data
    outlet_dat <- cbind(res@data$name, res_sub_all)
    outlet_dat <- na.omit(outlet_dat) # remove reservoirs outside study area
    res_dat_all <- merge(res_dat, outlet_dat, by.x="name", by.y=1)
    
    
    
    if(!silent) message("% OK")
    if(!silent) message("%")
    if(!silent) message("% Prepare and write output...")
    # sort data for writing output
    res_dat_sort <- data.frame(res_dat_all$sub_id_new,
                      res_dat_all$minlevel,
                      res_dat_all$maxlevel,
                      res_dat_all$vol0,
                      res_dat_all$storecap,
                      res_dat_all$damflow,
                      res_dat_all$damq_frac,
                      res_dat_all$withdrawal,
                      res_dat_all$damyear,
                      res_dat_all$maxdamarea,
                      res_dat_all$damdead,
                      res_dat_all$damalert,
                      res_dat_all$dama,
                      res_dat_all$damb,
                      res_dat_all$q_outlet,
                      res_dat_all$fvol_botm,
                      res_dat_all$fvol_over,
                      res_dat_all$damc,
                      res_dat_all$damd,
                      res_dat_all$elevbottom)
    
    # prepare output file
    header_str <- "Subasin-ID, minlevel[m], maxlevel[m], vol0([1000m**3]; unknown=-999), storcap[1000m**3], damflow[m**3/s], damq_frac[-], withdrawal[m**3/s], damyear[YYYY], maxdamarea[ha], damdead[1000m**3], damalert[1000m**3], dama[-], damb[-], qoutlet[m**3/s], fvol_bottom[-], fvol_over[-], damc[-], damd[-], elevbottom[m]"
    
    write(file=paste(dir_out,reservoir_file,sep="/"),
          x=c("Specification of reservoir parameters", header_str))
    
    # write data
    write.table(res_dat_sort, paste(dir_out,reservoir_file,sep="/"), append=T, quote=F,
                sep="\t", row.names=F, col.names=F)
    
    
    
    
    
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
    
    stop(paste(e))  
  })
    
} # EOF
