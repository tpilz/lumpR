# lumpR/lump_grass_prep.R
# Copyright (C) 2014-2018 Tobias Pilz
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


#' Pre-processing for Landscape Unit deviation using GRASS GIS
#' 
#' Takes raster data from a GRASS location and calculates elementary hillslopes,
#' soil-vegetation-components, and Horton stream order using GRASS functions
#' needed for further Landscape Unit deviation.
#' 
#' @param mask Mask in GRASS location defining the catchment area (e.g. subbasin raster map).
#' @param dem Digital elevation model in GRASS location used for delineation of
#'      environmental hillslope areas. Should be larger than the expected catchment,
#'      otherwise artefacts close to boundaries may occur.
#' @param lcov Landcover / vegetation raster map in GRASS location.
#' @param soil Soil raster map in GRASS location.
#' @param watermask Raster in GRASS location masking water surfaces (value '1') from
#'      other areas (value '0'). Map is used for \code{svc} creation such that a
#'      \code{svc} is completely covered with water ('special_area' in \code{svc_ofile}
#'      equal to 1) or contains no water surface. The map must NOT contain NULL
#'      values within \code{mask}! Default: \code{NULL}.
#' @param imperviousmask The same as for \code{watermask} but for impervious (e.g. urban
#'      and/or rocky) areas. 'special_area' flag in \code{svc_ofile} equal to 2. The map
#'      must NOT contain NULL values within \code{mask}!
#' @param eha Output: Name of Environmental Hillslope Areas (EHA) raster map
#'      exported into GRASS location.
#' @param flowdir Output: Name of flow direction raster map exported into GRASS
#'      location. Provides the "aspect" for each cell measured counterclockwise
#'      from East. Multiplying positive values by 45 will give the direction in
#'      degrees that the surface runoff will travel from that cell; zero indicates
#'      a depression; negative values that surface runoff is leaving the defined region.
#' @param flowacc Output: Name of flow accumulation raster map exported into GRASS
#'      location. Gives the number of upslope cells plus one. Negative values indicate
#'      surface runoff from outside the defined region.
#' @param stream Output: Name of stream segments raster map exported into GRASS
#'      location. If you want to convert it into a vector map apply GRASS function
#'      \emph{r.thin} beforehand!
#' @param disk_swap (optional, default: FALSE) Only needed if memory requirements exceed available RAM (large DEMs): If set to \code{TRUE}, \code{r.watershed} uses the "-m"-flag (slow)      
#' @param stream_horton Output: Name of stream segments raster map in Horton stream
#'      order exported into GRASS location.
#' @param elevriv Output: Name of relative elevation raster map exported into GRASS
#'      location. Provides the elevations above stream node in units of \code{dem}.
#' @param distriv Output: Name of distance to river raster map exported into GRASS
#'      location. Provides distances to stream node in number of grid cells.
#' @param mask_corr DEPRECATED! Argument kept for backwards compatibility.
#' @param svc Output: Name of Soil Vegetation Components raster map exported into
#'      GRASS location; cross product of categories of \code{soil} and \code{lcov}.
#' @param dir_out Character string specifying output directory (will be created;
#'      nothing will be overwritten).
#' @param svc_ofile Output: Name of file containing properties of \code{svc}s. For
#'      'special_area' flag values of 1 for water areas, 2 for impervious areas and
#'      0 in case it is an ordinary SVC are defined.
#' @param eha_thres Integer specifying threshold for delineation of \emph{EHA} in cells;
#'      parameter for GRASS function \emph{r.watershed}. This is a crucial parameter
#'      affecting the size of delineated hillslopes and the degree of detail of the
#'      landscape discretisation! As a rule of thumb, a value 10-100 times smaller
#'      than \code{thresh_sub} of function \code{\link[lumpR]{calc_subbas}} is usually
#'      a good choice.
#' @param sizefilter Integer specifying the minimum size of EHAs in hectares.
#'      Smaller EHAs (possibly artefacts) will be removed. Parameter for
#'      GRASS function \emph{r.reclass.area}. If set to \code{NULL} (default), 
#'      it will be automatically set to a value equivalent to 50 grid cells (ATTENTION:
#'      meters will be assumed as unit of the GRASS projection!).
#' @param growrad Integer specifying growing radius (in raster cells) to remove
#'      holes in the EHA raster resulting from cleaning of artefacts; parameter for
#'      GRASS function \emph{r.grow}. If set to \code{NULL} (default), it will be
#'      automatically set to 25 (should be but less than 100 due to long computation times!).
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#' @param addon_path Character string giving the path to your locally installed
#'      GRASS add-ons. Must only be given if necessary, see \code{Note}.
#' @param things2do \code{c("eha","river","svc")}. Enables the specification of sub-tasks only. \code{"eha"}: do EHA generation, 
#' \code{"river"}: calculate river network and morphological parameters, \code{"svc"}: generate SVC map 
#' 
#' @return Function returns nothing. Output raster files as specified in arguments
#'      (see above) are written into GRASS location.
#'
#' @note Prepare GRASS location and necessary raster files in advance and start
#'      GRASS session in R using \code{\link[rgrass]{initGRASS}}.
#' 
#'      Make sure that the GRASS functions \emph{r.stream.distance} and \emph{r.stream.order}
#'      are available to your GRASS installation. If not, consider \emph{g.extension} to
#'      install add-ons. If you installed add-ons locally it might occur that from within R
#'      the path to add-ons is not recognised. In such a case locate the local installation
#'      path (in a GRASS terminal check \code{g.extension -a}, \code{echo $GRASS_ADDON_BASE} and \code{which r.stream.distance} / \code{where r.stream.distance}, and specify the
#'      absolute path to add-ons via argument \code{addon_path}. In Windows, replace backslashes for slashes. For more information, see also
#'      \url{http://grasswiki.osgeo.org/wiki/AddOns/GRASS_7}.
#'      
#'      See GRASS documentation for further information on GRASS functions and
#'      parameters.
#'      
#'      If you run into \bold{memory issues}, consider argument \code{disk_swap} (see also 
#'      \link[GRASS homepage]{https://grass.osgeo.org/grass74/manuals/r.watershed.html#in-memory-mode-and-disk-swap-mode})
#'      and see discussion on \link[lumpR's github page]{https://github.com/tpilz/lumpR/issues/16}.
#'        
#' @references Source code based on \code{SHELL} and \code{MATLAB} scripts of Till Francke.
#' 
#'      lumpR package introduction with literature study and sensitivity analysis:\cr
#'      Pilz, T.; Francke, T.; Bronstert, A. (2017): lumpR 2.0.0: an R package facilitating
#'      landscape discretisation for hillslope-based hydrological models.
#'      \emph{Geosci. Model Dev.}, 10, 3001-3023, doi: 10.5194/gmd-10-3001-2017
#' 
#'      Theory of LUMP:\cr
#'      Francke, T.; Guentner, A.; Mamede, G.; Mueller, E. N. and Bronstert, A (2008):
#'      Automated catena-based discretization of landscapes for the derivation of
#'      hydrological modelling units. \emph{International Journal of Geographical
#'      Information Science, Informa UK Limited}, 22(2), 111-132, DOI: 10.1080/13658810701300873
#' 
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}

lump_grass_prep <- function(
  
  ### INPUT ###
  # GRASS raster #
  mask=NULL,
  dem=NULL,
  lcov=NULL,
  soil=NULL,
  watermask=NULL,
  imperviousmask=NULL,
  
  # OUTPUT #
  eha=NULL,
  flowdir=NULL,
  flowacc=NULL,
  stream=NULL,
  disk_swap=FALSE,
  stream_horton=NULL,
  elevriv=NULL,
  distriv=NULL,
  mask_corr=NULL,
  svc=NULL,
  dir_out=NULL,
  svc_ofile=NULL,
  
  # PARAMETERS #
  eha_thres=NULL,
  sizefilter=NULL,
  growrad=NULL,
  keep_temp=F,
  overwrite=F,
  silent=F,
  addon_path=NULL,
  things2do=c("eha","river","svc")
) {
  
### PREPROCESSING ###----------------------------------------------------------
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START lump_grass_prep()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")
  
# checks #---------------------------------------------------------------------
  tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  if(is.null(mask))
    stop("The name of a raster within the mapset of your initialised GRASS session to be used as catchment MASK in GRASS has to be given!")
  check_raster(mask, "mask")
  
  if(disk_swap) {
    ws_flags <- c("overwrite","m", "a", "b")
  } else {
    ws_flags <- c("overwrite", "a", "b")
  }
  
  if ("eha" %in% things2do)
  {
    if(is.null(dem))
      stop("The name of a DEM within the mapset of your initialised GRASS session has to be given!")
    check_raster(dem, "dem")
    if(is.null(eha))
      stop("A name for the calculated EHA raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(flowdir))
      stop("A name for the calculated flow direction raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(flowacc))
      stop("A name for the calculated flow accumulation raster map within the mapset of your initialised GRASS session has to be given!")
    if(!is.numeric(eha_thres))
      stop("You have to specify eha_thres as a number!")
    if(!is.numeric(sizefilter))
      sizefilter <- 50 * gmeta()$nsres * gmeta()$ewres / 1e4
    if(!is.numeric(growrad))
      growrad <- 25
  }
  if ("river" %in% things2do)
  {
    if(is.null(stream))
      stop("A name for the calculated stream segments raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(stream_horton))
      stop("A name for the calculated Horton stream order raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(elevriv))
      stop("A name for the calculated relative elevation raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(distriv))
      stop("A name for the calculated distance to river raster map within the mapset of your initialised GRASS session has to be given!")
    if(!is.null(mask_corr))
      message("% WARNING: Argument 'mask_corr' is deprecated since GRASS 7 integration and will be ignored!")
  }
  
  if ("svc" %in% things2do)
  {
    if(is.null(lcov))
      stop("The name of a landcover / vegetation raster map within the mapset of your initialised GRASS session has to be given!")
    check_raster(lcov, "lcov")
    if(is.null(soil))
      stop("The name of a soil raster map within the mapset of your initialised GRASS session has to be given!")
    check_raster(soil, "soil")
    if(is.null(svc))
      stop("A name for the calculated soil-vegetation-components raster map within the mapset of your initialised GRASS session has to be given!")
    if(is.null(svc_ofile))
      stop("A name for the file containing the calculated SVC parameters has to be given!")
    if(is.null(dir_out))
      stop("An output directory has to be specified!")
  }


  
  # add slash to end of addon_path if necessary
  addontest = try(execGRASS("g.extension", flags="a", intern=TRUE), silent = TRUE)
  if (class(addontest)=="try-error")
    stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?")
  
  #doesn't work for Windows
  #if (any(grepl(addon, pattern="No extension"))) #no extensions found
  #  stop("No GRASS extension found. Please check section Note in help of lump_grass_prep()")
  
  # addontest = try(execGRASS("r.stream.order", intern=TRUE), silent = TRUE)
  # if (class(addon)=="try-error")
  #   stop("Cannot run GRASS extension 'r.stream.order'. Please check section Note in help of lump_grass_prep()")
  # 
  # addontest = try(execGRASS("r.stream.distance", intern=TRUE), silent = TRUE)
  # if (class(addon)=="try-error")
  #   stop("Cannot run GRASS extension 'r.stream.distance'. Please check section Note in help of lump_grass_prep()")
  
  #try to install automatically: doesn't work on window
  #res = try(execGRASS("g.extension",extension="r.stream.distance", operation="add", intern=TRUE), silent = TRUE)
  
    
    
  if(!is.null(addon_path))
      if((addon_path!="") & substr(addon_path, nchar(addon_path), nchar(addon_path)) != "/")
      addon_path <- paste0(addon_path, "/")
    
    
# CLEAN UP AND RUNTIME OPTIONS #-----------------------------------------------
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
  
  options(error=cleanup) #in case of errors, clean up and reset to original warning and messaging state
  
  # remove output of previous function calls if overwrite=T (remove only relevant maps according to things2do)
  if (overwrite) {
    if("eha" %in% things2do)
      cmd_out <- execGRASS("g.remove", type="raster", pattern=paste("*_t,*_t1,*_t2,*_t3", eha, flowdir, flowacc, stream, sep=","), flags=c("f", "b"), intern=T)
    if("river" %in% things2do)
      cmd_out <- execGRASS("g.remove", type="raster", pattern=paste("*_t,*_t1,*_t2,*_t3", stream_horton, elevriv, distriv, sep=","), flags=c("f", "b"), intern=T)
    if("svc" %in% things2do)
      cmd_out <- execGRASS("g.remove", type="raster", pattern=paste("*_t,*_t1,*_t2,*_t3", svc, sep=","), flags=c("f", "b"), intern=T)
  } else {
    # remove temporary maps in any case
    cmd_out <- execGRASS("g.remove", type="raster", pattern="*_t,*_t1,*_t2,*_t3", flags=c("f", "b"), intern=T)
  }
  
  
  if(!silent) message("% OK")

  
  
### CALCULATIONS ###-----------------------------------------------------------
  
# eha #------------------------------------------------------------------------
  if ("eha" %in% things2do) {
      if(!silent) message("%")
      if(!silent) message("% Calculate and process EHAs...")
      
      # remove mask if there is any (and ignore error in case there is no mask)
      tt=tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r")), intern=TRUE), error=function(e){})
      
      # set mask and region
      cmd_out <- execGRASS("r.mask", raster=mask, intern = T)
      
      # calculate EHA etc.
      #cmd_out <- execGRASS("r.watershed", elevation=dem, threshold=eha_thres, half_basin="eha_t1", stream=stream,
      #          accumulation="flow_accum_t", drainage=flowdir, flags = c("s", "a"), intern=TRUE)
      cmd_out <- execGRASS("r.watershed", elevation=dem, threshold=eha_thres, half_basin="eha_t1", stream=stream,
                           accumulation=flowacc, drainage=flowdir, flags = ws_flags, intern=TRUE)

      if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")!=0) stop(cat(paste0("Error running r.watershed:", paste0(cmd_out, collapse="\n"))))
      
      ##border cells that receive inflow from outside get negative values, which propagate through entire basin. Check if this is a real problem or just an artefact!
      #cmd_out <- execGRASS("r.mapcalc", expression=paste0(flowacc, "= abs(flow_accum_t)"), intern=T)
      
      
      
      #remove fragments
      cmd_out <- execGRASS("r.reclass.area", input="eha_t1", mode="greater", value=sizefilter, output="eha_t2", intern = T)
      
      # grow EHA map to fill gaps resulted from remove of fragments
      cmd_out <- execGRASS("r.grow", input="eha_t2", output="eha_t3", radius=growrad, flags="overwrite", intern = T)
      
      # r.grow converts type CELL to type DCELL; convert back to CELL
      cmd_out <- execGRASS("r.mapcalc", expression=paste0(eha, " = int(eha_t3)"), flags = "overwrite", intern = T)
      
      # evaluate growing
      cmd_out <- execGRASS("r.stats", input=paste0(eha,",",mask), flags="quiet", separator="", intern=T)
      if(any(grepl(cmd_out, pattern = "^\\*[0-9]")))
        stop("There are still gaps in the EHA raster map after growing. Try to increase growrad and run again.")

      if(!silent) message("% OK")
      # if an error occurs delete all temporary output
    
  } # things2do: eha
     
  
# river #----------------------------------------------------------------------
  if ("river" %in% things2do) {
    
      if(!silent) message("%")
      if(!silent) message("% Calculate river network and morphological parameters...")
      
      # set mask
      cmd_out <-tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"), intern = T)), error=function(e){})
      cmd_out <- execGRASS("r.mask", raster=mask, intern = T)
    
      # calculate Horton stream order (works only for non-thinned stream segments!)
      cmd_out <- execGRASS("g.region", raster=flowdir, intern = T) # complains about defiation in resolution of flowdir although it is the same as for the region?!
      tryCatch(execGRASS(paste0(addon_path, "r.stream.order"), stream_rast=stream, direction=flowdir, horton=stream_horton, flags = "overwrite")
                   , error=function(e){ stop("Couldn't find r.stream.order. Check extensions and 'addon_path' (see help for details).")}
        )
      
      # calculate distance to river and relative elevation for each cell
      cmd_out <- execGRASS(paste0(addon_path, "r.stream.distance"), stream_rast=stream, direction=flowdir, elevation=dem,
                           method="downstream", distance="dist_riv_t", difference="elevriv_t", intern = T)
      
      # get resolution (mean between x and y resolution)
      RES <- execGRASS("r.info", map=dem, flags=c("g"), intern=TRUE)
      RES <- sum(as.numeric(gsub("[a-z]*=", "", grep("nsres|ewres", RES, value = T)))) / 2
      
      # convert units: metric to number of cells (expected by following scripts)
      cmd_out <- execGRASS("r.mapcalc", expression=paste0(distriv, "= dist_riv_t / ", RES), intern = T)
      
      # set gaps in elevriv to zero; these seem to be a unresolved bug (by Feb 2018): https://trac.osgeo.org/grass/ticket/2516#comment:7
      # in my case, under the grass 6.4 version fo r.stream.distance, there where values of -1 instead of NULL at the gaps
      cmd_out <- execGRASS("r.mapcalc", expression = paste0(elevriv, "= if(isnull(elevriv_t) && !isnull(", mask, "),0,elevriv_t)"), intern = T)
      # # define new mask (it doesn't work to specify output of r.stream.distance as mask; don't know why)
      # cmd_out <- execGRASS("r.mapcalc", expression = paste0(mask_corr, "= if(isnull(", elevriv, "),null(),1)"), intern = T)
      # # set new mask as area is slightly smaller afer r.stream.distance
      # cmd_out <- execGRASS("r.mask", raster=mask_corr, flags=c("r"), intern = T)
      
      if(!silent) message("% OK")
    # if an error occurs delete all temporary output
    
  } # things2do: river

  
# svc #------------------------------------------------------------------------
  if ("svc" %in% things2do) {
    
      if(!silent) message("%")
      if(!silent) message("% Calculate soil vegetation components...")
      
      # set mask
      cmd_out <-tryCatch(suppressWarnings(execGRASS("r.mask", flags=c("r"), intern = T)), error=function(e){})
      cmd_out <- execGRASS("r.mask", raster=mask, intern = T)
      
      #check existence of NULLs in input data
      cmd_out <- execGRASS("r.stats", input=paste0(soil,",MASK"), flags="quiet", separator="", intern=T)
      if(any(grepl(cmd_out, pattern = "^\\*[0-9]")))
        stop("Raster map '", soil,"' contains NULL values within 'mask' which is not allowed! For small patches, consider r.grow.")
      
      cmd_out <- execGRASS("r.stats", input=paste0(lcov,",MASK"), flags="quiet", separator="", intern=T)
      if(any(grepl(cmd_out, pattern = "^\\*[0-9]")))
        stop("Raster map '", lcov,"' contains NULL values within 'mask' which is not allowed! For small patches, consider r.grow.")
      
      if (!is.null(imperviousmask))
        cmd_out <- execGRASS("r.stats", input=paste0(imperviousmask,",MASK"), flags="quiet", separator="", intern=T)
      if(any(grepl(cmd_out, pattern = "^\\*[0-9]")))
        stop("Raster map '", imperviousmask,"' contains NULL values within 'mask' which is not allowed! For small patches, consider r.grow.")
      
      if (!is.null(watermask))
        cmd_out <- execGRASS("r.stats", input=paste0(watermask,",MASK"), flags="quiet", separator="", intern=T)
      if(any(grepl(cmd_out, pattern = "^\\*[0-9]")))
        stop("Raster map '", watermask,"' contains NULL values within 'mask' which is not allowed! For small patches, consider r.grow.")
      
      
      # create output directory
      dir.create(dir_out, recursive=T, showWarnings=F)
      
    

      # check output directory
      if (!overwrite & file.exists(paste(dir_out,svc_ofile,sep="/"))) 
        stop(paste0("In output directory '", dir_out, "' the file '", svc_ofile, "' already exists!"))
      
      # check if lcov or soil contains labels and create temporary map without labels if necessary (otherwise, the labels cause problems)
      lens <- sapply(unlist(execGRASS("r.category", map=soil, separator=",", intern=T)), function(x) length(unlist(strsplit(x,","))))
      if(any(lens>1)) {
        cmd_out <- execGRASS("r.mapcalc", expression=paste0(paste0(unlist(strsplit(soil, "@"))[1], "_t"), "= ", soil, "*1"), intern = T)
        soil <- paste0(unlist(strsplit(soil, "@"))[1], "_t")
      }
      lens <- sapply(unlist(execGRASS("r.category", map=lcov, separator=",", intern=T)), function(x) length(unlist(strsplit(x,","))))
      if(any(lens>1)) {
        cmd_out <- execGRASS("r.mapcalc", expression=paste0(paste0(unlist(strsplit(lcov, "@"))[1], "_t"), "= ", lcov, "*1"), intern = T)
        lcov <- paste0(unlist(strsplit(lcov, "@"))[1], "_t")
      }
      
      # create soil vegetation components from soil and landcover/vegetation data
      # NOTE: categories of soil, lcov, water and impervious only needed to fix r.cross bug
      cmd_out <- execGRASS("g.remove", type="raster", name=svc, flags=c("f", "b"), intern = T)
      if (!is.null(watermask) & !is.null(imperviousmask)) {
        cmd_out <- execGRASS("r.cross", input=paste(soil,lcov,watermask,imperviousmask,sep=","), output=svc, intern = T)
        cat_labs_soil <- execGRASS("r.stats", input=soil, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_lcov <- execGRASS("r.stats", input=lcov, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_wat <- execGRASS("r.stats", input=watermask, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_imp <- execGRASS("r.stats", input=imperviousmask, flags=c("n"), intern=T, ignore.stderr = T)
      } else if (!is.null(watermask) & is.null(imperviousmask)) {
        cmd_out <- execGRASS("r.cross", input=paste(soil,lcov,watermask,sep=","), output=svc, intern = T)
        cat_labs_soil <- execGRASS("r.stats", input=soil, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_lcov <- execGRASS("r.stats", input=lcov, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_wat <- execGRASS("r.stats", input=watermask, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_imp <- NULL
      } else if (is.null(watermask) & !is.null(imperviousmask)) {
        cmd_out <- execGRASS("r.cross", input=paste(soil,lcov,imperviousmask,sep=","), output=svc, intern = T) 
        cat_labs_soil <- execGRASS("r.stats", input=soil, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_lcov <- execGRASS("r.stats", input=lcov, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_imp <- execGRASS("r.stats", input=imperviousmask, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_wat <- NULL
      } else {
        cmd_out <- execGRASS("r.cross", input=paste(soil,lcov,sep=","), output=svc, intern = T)
        cat_labs_soil <- execGRASS("r.stats", input=soil, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_lcov <- execGRASS("r.stats", input=lcov, flags=c("n"), intern=T, ignore.stderr = T)
        cat_labs_wat <- NULL
        cat_labs_imp <- NULL
      }
      
      # check for and correct error in r.cross, see https://lists.osgeo.org/pipermail/grass-user/2018-February/077934.html
      cmd_out <- execGRASS("r.stats", input=svc, flags=c("n"), intern=T, ignore.stderr = T)
      if(any(as.numeric(cmd_out) == 0)) {
        # save category labels
        cat_labs <- execGRASS("r.category", map=svc, separator=":", intern=T)
        cat_labs <- strsplit(cat_labs[-1], ":")
        cat_labs <- lapply(cat_labs, function(x) c(as.numeric(x[1]) +1, x[2]))
        # add +1 to categories (destroys labels)
        cmd_out <- execGRASS("r.mapcalc", expression=paste0("svc_t = ", svc, "+1"), flags=c("overwrite"), intern=T)
        cmd_out <- execGRASS("g.remove", type="raster", name=svc, flags=c("f", "b"), intern = T)
        cmd_out <- execGRASS("g.rename", raster=paste0("svc_t,",svc), intern = T)
        
        # get missing category label
        cat_lab_miss <- c(1, paste(c("category"), c(cat_labs_soil[1], cat_labs_lcov[1], cat_labs_wat[1], cat_labs_imp[1]), sep=" ", collapse = "; "))
        # merge to stored labels
        cat_labs_mod <- sapply(c(list(cat_lab_miss), cat_labs), paste, collapse=":")
        # write to grass raster
        write.table(cat_labs_mod, paste(dir_out, "svc_recl_t.txt", sep="/"), sep="\t", quote=F, row.names = F, col.names = F)
        cmd_out <- execGRASS("r.category", map=svc, separator=":", rules=paste(dir_out, "svc_recl_t.txt", sep="/"), intern = T)
        file.remove(paste(dir_out, "svc_recl_t.txt", sep="/"))
      }
      
      # categories of SVCs
      svc_cats <- execGRASS("r.category", map=svc, separator=",", intern=T)
      
      if(any(grepl("no data", svc_cats))) {
        stop("Raster maps 'soil', 'lcov', 'watermask', and/or 'impervious' mask contain NULL values within 'mask' which is not allowed!")
      }
  
      # transformations ...
      svc_cats_grp <- grep("^0", svc_cats, invert=T, value=T) #remove zero entries
      svc_cats_sub <- gsub(",|;", "", svc_cats_grp)
      svc_cats_spl <- strsplit(svc_cats_sub, "category|Category")
  
      if (!is.null(watermask) & !is.null(imperviousmask)) {
        svc_cats_mat_t <- matrix(as.integer(unlist(svc_cats_spl)),ncol=5, byrow=T)
        colnames(svc_cats_mat_t) <- c("pid", "soil_id", "veg_id", "water", "impervious") # same order as input of "r.cross"!
        svc_cats_mat <- svc_cats_mat_t[,-5]
        colnames(svc_cats_mat)[4] <- "special_area"
        rows_water <- which(svc_cats_mat_t[,"water"] == 1)
        rows_impervious <- which(svc_cats_mat_t[,"impervious"] == 1)
        svc_cats_mat[rows_water,"special_area"] <- 1
        svc_cats_mat[rows_impervious,"special_area"] <- 2
      } else if (!is.null(watermask) & is.null(imperviousmask)) {
        svc_cats_mat <- matrix(as.integer(unlist(svc_cats_spl)),ncol=4, byrow=T)
        colnames(svc_cats_mat) <- c("pid", "soil_id", "veg_id", "special_area") # same order as input of "r.cross"!
      } else if (is.null(watermask) & !is.null(imperviousmask)) {
        svc_cats_mat <- matrix(as.integer(unlist(svc_cats_spl)),ncol=4, byrow=T)
        colnames(svc_cats_mat) <- c("pid", "soil_id", "veg_id", "special_area") # same order as input of "r.cross"!
        svc_cats_mat[which(svc_cats_mat[,"special_area"] == 1),"special_area"] <- 2
      } else {
        svc_cats_mat <- matrix(as.integer(unlist(svc_cats_spl)),ncol=3, byrow=T)
        colnames(svc_cats_mat) <- c("pid", "soil_id", "veg_id") # same order as input of "r.cross"!
        svc_cats_mat <- cbind(svc_cats_mat, rep(0,nrow(svc_cats_mat)))
        colnames(svc_cats_mat)[4] <- "special_area"
      }
  
      # header of svc output file
      svc_out <- matrix(NA, ncol=13, nrow=nrow(svc_cats_mat))
      svc_out_head <- c("pid", "description", "soil_id", "veg_id", "musle_k", "musle_c1","musle_c2","musle_c3","musle_c4","musle_p","coarse_frac","manning_n","special_area")
      colnames(svc_out) <- svc_out_head
      
      # merge data with output mat
      svc_out[,colnames(svc_cats_mat)] <- svc_cats_mat
  
      # write output
      write.table(svc_out, paste(dir_out, svc_ofile, sep="/"), quote=F, sep="\t", row.names=F)
  
      if(!silent) message("% OK")
      # if an error occurs delete all temporary output
    
  } # things2do: svc
  
  
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

