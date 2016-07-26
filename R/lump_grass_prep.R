# LUMP/lump_grass_prep.R
# Copyright (C) 2014,2015,2016 Tobias Pilz
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
#' @param mask Mask in GRASS location defining the catchment area. E.g. subbasin raster map.
#' @param dem Digital elevation model in GRASS location used for delineation of
#'      environmental hillslope areas. Should be larger than the expected catchment,
#'      otherwise artefacts close to boundaries may occur.
#' @param lcov Landcover / vegetation raster map in GRASS location.
#' @param soil Soil raster map in GRASS location.
#' @param watermask Raster in GRASS location masking water surfaces (value '1') from
#'      other areas (value '0'). Map is used for \code{svc} creation such that a
#'      \code{svc} is completely covered with water ('special_area' in \code{svc_ofile}
#'      equal to 1) or contains no water surface. Default: \code{NULL}.
#' @param imperviousmask The same as for \code{watermask} but for impervious (e.g. urban
#'      and/or rocky) areas. 'special_area' flag in \code{svc_ofile} equal to 2.
#' @param eha Output: Name of Environmental Hillslope Areas (EHA) raster map
#'      exported into GRASS location.
#' @param flowdir Output: Name of flow direction raster map exported into GRASS
#'      location.
#' @param flowacc Output: Name of flow accumulation raster map exported into GRASS
#'      location.
#' @param stream Output: Name of stream segments raster map exported into GRASS
#'      location. If you want to convert it into a vector map apply GRASS function
#'      \emph{r.thin} beforehand!
#' @param stream_horton Output: Name of stream segments raster map in Horton stream
#'      order exported into GRASS location.
#' @param elevriv Output: Name of relative elevation raster map exported into GRASS
#'      location.
#' @param distriv Output: Name of distance to river raster map exported into GRASS
#'      location.
#' @param mask_corr Output: Name of corrected mask (catchment area is slightly
#'      smaller afer applying \emph{r.stream.distance}).
#' @param svc Output: Name of Soil Vegetation Components raster map exported into
#'      GRASS location; cross product of categories of \code{soil} and \code{lcov}.
#' @param dir_out Character string specifying output directory (will be created;
#'      nothing will be overwritten).
#' @param svc_ofile Output: Name of file containing properties of \code{svc}s. For
#'      'special_area' flag values of 1 for water areas, 2 for impervious areas and
#'      0 in case it is an ordinary SVC are defined.
#' @param eha_thres Integer specifying threshold for delineation of \emph{EHA};
#'      parameter for GRASS function \emph{r.watershed}. This is a crucial parameter
#'      affecting the size of delineated hillslopes and the degree of detail of the
#'      landscape discretisation!
#' @param sizefilter Integer specifying minimum size of EHAs (in map units)
#'      not to be removed, smaller EHAs (artefacts) are removed; parameter for
#'      GRASS function \emph{r.reclass.area}.
#' @param growrad Integer specifying growing radius (in raster cells) to remove
#'      artefacts in EHA data; parameter for GRASS function \emph{r.grow}.
#' @param keep_temp \code{logical}. Set to \code{TRUE} if temporary files shall be kept
#'      in the GRASS location, e.g. for debugging or further analyses. Default: \code{FALSE}.
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings
#'      of internally used GRASS functions)? Default: \code{FALSE}.
#' @param addon_path Charactering string giving the path to your locally installed
#'      GRASS add-ons. Must only be given if necessary, see \code{Note}.
#' 
#' @return Function returns nothing. Output raster files as specified in arguments
#'      (see above) are written into GRASS location.
#'
#' @note Prepare GRASS location and necessary raster files in advance and start
#'      GRASS session in R using \code{\link[spgrass6]{initGRASS}}.
#' 
#'      Make sure that the GRASS functions \emph{r.stream.distance} and \emph{r.stream.order}
#'      are available to your GRASS installation. If not, consider \emph{g.extension} to
#'      install add-ons. If you installed add-ons locally it might occur that from within R
#'      the path to add-ons is not recognised. In such a case locate the local installation
#'      path (in a GRASS terminal use, e.g., \code{which r.stream.distance}) and specify the
#'      absolute path to add-ons via argument \code{addon_path}. For more information, see also
#'      \url{http://grasswiki.osgeo.org/wiki/AddOns/GRASS_6}.
#'      
#'      See GRASS documentation for further information on GRASS functions and
#'      parameters.
#'        
#' @references Source code based on \code{SHELL} and \code{MATLAB} scripts of Till Francke.
#' 
#'      Theory of LUMP:\cr
#'      Francke, T.; Guentner, A.; Mamede, G.; Mueller, E. N. and Bronstert, A (2008):
#'      Automated catena-based discretization of landscapes for the derivation of
#'      hydrological modelling units. \emph{International Journal of Geographical
#'      Information Science, Informa UK Limited}, 22(2), 111-132, DOI: 10.1080/13658810701300873
#' 
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}
#' 
#' @export
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
  addon_path=NULL
) {
  
  ### PREPROCESSING ###
  
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
  
  
  # CHECKS #
  if(is.null(mask))
    stop("The name of a raster within the mapset of your initialised GRASS session to be used as catchment MASK in GRASS has to be given!")
  if(is.null(dem))
    stop("The name of a DEM within the mapset of your initialised GRASS session has to be given!")
  if(is.null(lcov))
    stop("The name of a landcover / vegetation raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(soil))
    stop("The name of a soil raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(eha))
    stop("A name for the calculated EHA raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(flowdir))
    stop("A name for the calculated flow direction raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(flowacc))
    stop("A name for the calculated flow accumulation raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(stream))
    stop("A name for the calculated stream segments raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(stream_horton))
    stop("A name for the calculated Horton stream order raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(elevriv))
    stop("A name for the calculated relative elevation raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(distriv))
    stop("A name for the calculated distance to river raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(mask_corr))
    stop("A name for the corrected raster MASK within the mapset of your initialised GRASS session has to be given!")
  if(is.null(svc))
    stop("A name for the calculated soil-vegetation-components raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(dir_out))
    stop("An output directory has to be specified!")
  if(is.null(svc_ofile))
    stop("A name for the file containing the calculated SVC parameters has to be given!")
  if(!is.numeric(eha_thres))
    stop("You have to specify eha_thres as a number!")
  if(!is.numeric(sizefilter))
    stop("You have to specify sizefilter as a number!")
  if(!is.numeric(growrad))
    stop("You have to specify growrad as a number!")
  
  # add slash to end of addon_path if necessary
  if(!is.null(addon_path))
    if(substr(addon_path, nchar(addon_path), nchar(addon_path)) != "/")
      addon_path <- paste0(addon_path, "/")
  
  
  
  
  
  ### CALCULATIONS ###
  tryCatch({
    message("\nSTART hydrological preprocessing for LUMP using GRASS...\n")
    
    # create output directory
    dir.create(dir_out, recursive=T)
    
    # check output directory
    if (!overwrite & file.exists(paste(dir_out,svc_ofile,sep="/"))) 
      stop(paste0("In output directory '", dir_out, "' the file '", svc_ofile, "' already exists!"))
    
    
    # remove mask if any
    execGRASS("r.mask", flags=c("r"))
    
    # remove output of previous function calls if overwrite=T
    if (overwrite) {
      execGRASS("g.mremove", rast=paste("*_t,*_t1,*_t2", eha, flowdir, flowacc, stream, stream_horton, elevriv, distriv, mask_corr, svc, sep=","), flags=c("f"))
    } else {
      # remove temporary maps in any case
      execGRASS("g.mremove", rast="*_t,*_t1,*_t2", flags=c("f"))
    }
    
    
    
    # EHA #
    message("\nCalculate and process EHAs...\n")
    
    # check of lcov or soil contains labels and create temporary map without labels if necessary
    lens <- sapply(unlist(execGRASS("r.category", map=soil, fs=",", intern=T)), function(x) length(unlist(strsplit(x,","))))
    if(any(lens>1)) {
      execGRASS("r.mapcalculator", amap=soil, outfile=paste0(unlist(strsplit(soil, "@"))[1], "_t"), formula="A*1")
      soil <- paste0(unlist(strsplit(soil, "@"))[1], "_t")
    }
    lens <- sapply(unlist(execGRASS("r.category", map=lcov, fs=",", intern=T)), function(x) length(unlist(strsplit(x,","))))
    if(any(lens>1)) {
      execGRASS("r.mapcalculator", amap=lcov, outfile=paste0(unlist(strsplit(lcov, "@"))[1], "_t"), formula="A*1")
      lcov <- paste0(unlist(strsplit(lcov, "@"))[1], "_t")
    }
    
    # calculate EHA etc.
    execGRASS("r.watershed", elevation=dem, threshold=eha_thres, half.basin="eha_t1", stream=stream,
              accumulation="flow_accum_t", drainage=flowdir)

    #border cells that receive inflow from outside get negative values, which propagate through entire basin. Check is this is a real problem or just an artefact!
    execGRASS("r.mapcalculator", amap="flow_accum_t", outfile=flowacc, formula="abs(flow_accum_t)")
    
    # set mask and region
    execGRASS("r.mask", input=mask, flags=c("o"))  
    
    #remove fragments
    execGRASS("r.reclass.area", input="eha_t1", greater=sizefilter, output="eha_t2")
    
    # grow subbasin map to fill gaps resulted from remove of fragments
    grow_eval <- NULL
    execGRASS("r.grow", input="eha_t2", output=eha, radius=growrad)
    
    # evaluate growing
    execGRASS("r.mapcalculator", amap=mask, bmap=eha, outfile="grow_eval_t", formula="A * isnull(B)", flags=c("overwrite"))
    
    grow_eval2 <- execGRASS("r.stats", input="grow_eval_t", flags=c("n"), intern=TRUE)
    if (grepl(pattern="[0-9]+.*[\b]+",x=tail(grow_eval2, n=1)))
      grow_eval2 = grow_eval2[-length(grow_eval2)] #last line contains progress indicator, remove
    
    grow_eval2 <- as.numeric(grow_eval2)
    
    grow_eval <- c(grow_eval, grow_eval2)
    
    if (any(grow_eval==1)) {
      stop("There are still gaps in the subbasin and/or EHA raster maps after growing. Try to increase growrad and run again.")
    }

        
    
    
    # RIVER calculations #
    message("\nCalculate river network and morphological parameters...\n")
    
    # calculate Horton stream order (works only for non-thinned stream segments!)
    execGRASS("g.region", rast=flowdir) # complains about defiation in resolution of flowdir although it is the same as for the region?!
    execGRASS(paste0(addon_path, "r.stream.order"), stream=stream, dir=flowdir, horton=stream_horton)
    
    # calculate distance to river and relative elevation for each cell
    execGRASS(paste0(addon_path, "r.stream.distance"), stream=stream, dir=flowdir, dem=dem, method="downstream", 
              distance="dist_riv_t", elevation=elevriv)
    
    # get resolution (mean between x and y resolution)
    RES <- execGRASS("r.info", map=dem, flags=c("s"), intern=TRUE)
    RES <- sum(as.numeric(gsub("[a-z]*=", "", RES))) / 2
    
    # convert units: metric to number of cells (expected by following scripts)
    execGRASS("r.mapcalculator", amap="dist_riv_t", outfile=distriv, formula=paste("A/",RES,sep=""))
    
    # define new mask (it doesn't work to specify output of r.stream.distance as mask; don't know why)
    execGRASS("r.mapcalculator", amap=elevriv, outfile=mask_corr, formula="if(isnull(A),null(),1)")
    # set new mask as area is slightly smaller afer r.stream.distance
    execGRASS("r.mask", input=mask_corr, flags=c("o"))
    

    
    
    # SOIL VEGETATION COMPONENTS #
    message("\nCalculate soil vegetation components...\n")
    
    # create soil vegetation components from soil and landcover/vegetation data
    if (!is.null(watermask) & !is.null(imperviousmask)) {
      execGRASS("r.cross", input=paste(soil,lcov,watermask,imperviousmask,sep=","), output=svc)
    } else if (!is.null(watermask) & is.null(imperviousmask)) {
      execGRASS("r.cross", input=paste(soil,lcov,watermask,sep=","), output=svc)
    } else if (is.null(watermask) & !is.null(imperviousmask)) {
      execGRASS("r.cross", input=paste(soil,lcov,imperviousmask,sep=","), output=svc) 
    } else {
      execGRASS("r.cross", input=paste(soil,lcov,sep=","), output=svc)
    }
    
    # categories of SVCs
    svc_cats <- execGRASS("r.category", map=svc, fs=",", intern=T)

    # transformations ...
    svc_cats_grp <- grep("^0", svc_cats, invert=T, value=T)
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




    # remove temp files
    if(keep_temp == FALSE)
      execGRASS("g.mremove", rast="*_t,*_t1,*_t2", flags=c("f"))
    
    message("\nDONE!\n")
    
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    
    
    
    
    
    # if an error occurs delete all temporary output
  }, error = function(e) {
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    execGRASS("r.mask", flags=c("r"))
    
    if(keep_temp == FALSE)
      execGRASS("g.mremove", rast=paste("*_t,*_t1,*_t2", eha, flowdir, flowacc, stream, stream_horton, elevriv, distriv, mask_corr, svc, sep=","), flags=c("f"))
    
    stop(paste(e))  
  })
  
} # EOF
