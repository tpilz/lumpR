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


#' Assemble of reservoir parameter file for WASA for import into database with \code{\link[lumpR]{db_fill}} 
#' 
#' Function generates the reservoir parameter file from a pre-processed
#' reservoir vector map and optional supplemental parameter file.
#' 
#' @param res_vect Name of reservoir vector map in GRASS location. Should be point
#'      instead of polygon feature (i.e. reservoir outlet locations; consider function
#'      \code{\link[lumpR]{reservoir_outlet}})! For mandatory columns in attribute table
#'      see \code{Details}. In any case, column \code{subbas_id} containing the subbasin
#'      ID derived from \code{subbasin} will be added to the attribute table.
#' @param subbasin Subbasin raster map in GRASS location. Can be created with
#'      \code{\link[lumpR]{calc_subbas}}.
#' @param res_file tab-delimited file containing reservoir properties (fields see details) and key \code{res_id}. 
#'       If set to NULL, these attributes must be contained in the attribute table of \code{res_vect}. If they are missing,
#'       they will be set to deafult values (see Details).
#' @param dir_out Character string specifying output directory (will be created if it
#'      does not yet exist).
#' @param reservoir_file Output: File of parameters for the strategic reservoirs
#'      assigned to subbasins. To be filled into a database using \code{\link[lumpR]{db_fill}}.
#'      This file is \bold{NOT} directly compatible with WASA-SED!
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#'      
#' @details For each reservoir that should be modelled explicitly within WASA the
#'      following information need to be collected and written into the vector file's
#'      attribute table or \code{res_file}. Column order is not important. Additional
#'      columns can be given but will be ignored:
#'      
#'      \emph{res_id}\cr
#'      Unique numeric reservoir identifier (if \code{res_file} is given, it also needs to be
#'      defined in the vector file's attribute table!).
#'      
#'      \emph{name}\cr
#'      OPTIONAL: name of the reservoir. Will be filled with \code{<NA>} if not given.
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
#'      The output file \code{reservoir_file} contains the additional column \emph{pid}
#'      which is the corresponding subbasin ID determined from input \code{subbasin}.
#'      
#' @note 
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
  res_file=NULL,
  ### OUTPUT ###
  dir_out="./",
  reservoir_file="reservoir.txt",
  ### PARAMETER ###
  overwrite=F,
  silent=F
) {
  
### PREPROCESSING ###----------------------------------------------------------
  
  tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  check_raster(subbasin,"subbasin")
  check_vector(res_vect,"res_vect")
  
  if (!is.null(res_file) && !file.exists(res_file)) stop(paste0("Input file ", res_file, " not found."))
  
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
    
    if(!silent) message("%")
    if(!silent) message("% Assignment of reservoirs to subbasins...")
    
    #  #add subbasin-ID to reservoirs 
    x <- execGRASS("v.db.addcolumn", map=res_vect, columns="subbas_id int", intern=TRUE) 
    x <- execGRASS("v.what.rast", map=res_vect, column="subbas_id", raster=subbasin, intern=TRUE)  
    
    
    # get reservoir data
    res <- readVECT(res_vect)
    
    # make sure column names are in in lowercase only
    colnames(res@data) <- tolower(colnames(res@data))
    if (!any(names(res@data)  =="res_id")) stop(paste0("Reservoir vector ", res_vect, " must contain the column 'res_id'."))
    
    
    # check existence of necessary columns in attribute table
    cols_mandatory <- c("minlevel", "maxlevel", "vol0", "storecap", "damflow", "damq_frac", 
                        "withdrawal", "damyear", "maxdamarea", "damdead", "damalert", "dama", "damb", 
                        "q_outlet", "fvol_botm", "fvol_over", "damc", "damd", "elevbottom")
    
    # merge data from res_file if available
    if (!is.null(res_file))
    {  
      res_params = read.table(res_file, header=TRUE, sep="\t")
      colnames(res_params) <- tolower(colnames(res_params))
      if (!any(names(res_params)=="res_id")) stop(paste0("Reservoir file ", res_file, " must contain the column 'res_id'."))
      res@data = merge(res@data, res_params, by="res_id")
    } else #no file for reservoir parameters given
    {
      warning("Reservoir parameters not specified, dummy default values will be used. Please refine these in the database!")
      
      set2default = setdiff(cols_mandatory, colnames(res@data)) #find missing colnames that need to be set to default values

      if ("name" %in% colnames(res@data))
              res@data$name=paste0("res_",res@data$res_id, ifelse(length(set2default)>0,"_default",""))
      
      # if ("maxdamarea" %in% set2default)
      # {  
      #   get.area <- function(polygon) {
      #     row <- data.frame(id=polygon@ID, area=polygon@area, stringsAsFactors=FALSE)
      #     return(row)
      #   }
      #   areas <- do.call(rbind,lapply(res@polygons, get.area))
      #   res@data  <- merge(res@data, areas, by="res_id")  # append area column
      #   names(res@data)[names(res@data)=="area"] = "maxdamarea" #rename column
      # }  
      
      if ("maxdamarea" %in% set2default)
      {  
        res@data$maxdamarea=res@data$area
        res@data$area=NULL
      }
      
      if ("maxlevel" %in% set2default)
        res@data$maxlevel=0
      
      if ("minlevel" %in% set2default)
              res@data$minlevel=0
      
 
      if ("vol0" %in% set2default)
        res@data$vol0=molle_v(alpha = 2.7, k = 1500, A = res@data$maxdamarea*1e4)
      
      if ("storecap" %in% set2default)
        res@data$storecap=res@data$vol0
      
      if ("damflow" %in% set2default)
        res@data$damflow = 9.99
      
      if ("damq_frac" %in% set2default)
        res@data$damq_frac = 1
      
      if ("withdrawal" %in% set2default)
        res@data$withdrawal = 0
      
      if ("damyear" %in% set2default)
        res@data$damyear = 1900
      
      if ("damdead" %in% set2default)
        res@data$damdead = 0.01 * res@data$storecap
      
      if ("damalert" %in% set2default)
        res@data$damalert = 0.01 * res@data$storecap
      
      if ("dama" %in% set2default)
        res@data$dama=1500
      
      if ("damb" %in% set2default)
        res@data$damb=2.7
      
      if ("q_outlet" %in% set2default)
        res@data$q_outlet=0.999
      
      if ("fvol_botm" %in% set2default)
        res@data$fvol_botm = 0.01
      
      if ("fvol_over" %in% set2default)
        res@data$fvol_over= 1
      
      if ("damc" %in% set2default)
        res@data$damc=99.99
      
      if ("damd" %in% set2default)
        res@data$damd = 1.5
      
      if ("elevbottom" %in% set2default)
        res@data$elevbottom=99
      
    }

    chk_cols <- grepl(paste(colnames(res@data), collapse="|^"), cols_mandatory)
    if(any(!chk_cols))
      stop(paste0("Check attribute table of 'res_vect' or provide 'res_file', column(s) ", paste(cols_mandatory[!chk_cols], collapse=", "), " could not be found!"))
    
    # check for duplicates (multiple reservoirs  per subbasin)
    dupl <- duplicated(res@data$subbas_id, incomparables=c(NA))
    if (any(dupl))
      stop(paste0("Subbasin(s) no. ", paste(unique(res@data$subbas_id[dupl]), collapse=", "), " contain(s) multiple strategic reservoirs! Remove reservoirs, refine their outlet points or redefine subcatchments."))
    
    outside <- is.na(res@data$subbas_id)
    if (any(outside))
      warning(paste0("Subbasin(s) no. ", paste(unique(res@data$subbas_id[outside]), collapse=", "), " are outside the defined subcatchments."))
    
    # add name columns if it does not yet exist
    if(!("name" %in% colnames(res@data))) {
      res@data <- cbind(res@data, name=NA)
    }

    # combine subbasin information with reservoir parameter data
    res_dat <- res@data
    res_dat <- res_dat[!is.na(res_dat$subbas_id),] # remove reservoirs outside study area
    

    if(!silent) message("% OK")
    if(!silent) message("%")
    if(!silent) message("% Prepare and write output...")
    # sort data for writing output
    res_dat = res_dat[sort.int(res_dat$subbas_id, index.return = TRUE)$ix,] #sort by subbasin-id
    
    names(res_dat)[names(res_dat)=="subbas_id"]="pid"
    #order and select columns
    res_dat = res_dat[, c("pid", "res_id", "name",
    "minlevel",
    "maxlevel",
    "vol0",
    "storecap",
    "damflow",
    "damq_frac",
    "withdrawal",
    "damyear",
    "maxdamarea",
    "damdead",
    "damalert",
    "dama",
    "damb",
    "q_outlet",
    "fvol_botm",
    "fvol_over",
    "damc",
    "damd",
    "elevbottom")]
    
    write.table(res_dat, paste(dir_out,reservoir_file,sep="/"), append=F, quote=F,
                sep="\t", row.names=F, col.names=T)
    

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
