#' Pre-processing for Landscape Unit deviation using GRASS GIS
#' 
#' Takes raster data from a GRASS location and calculates further raster data
#' using GRASS functions needed for Landscape Unit deviation.
#' 
#' @param mask Mask in GRASS location defining the catchment area. E.g. subbasin raster map.
#' @param dem Digital elevation model in GRASS location used for delineation of
#'      environmental hillslope areas. Should be larger than the expected catchment,
#'      otherwise artefacts close to boundaries may occur.
#' @param lcov Landcover / vegetation raster map in GRASS location. Must NOT contain
#'      labels!
#' @param soil Soil raster map in GRASS location. Must NOT contain labels!
#' @param watermask Raster in GRASS location masking water surfaces (value '1') from
#'      other areas (value NULL). Water areas are cropped from \code{svc}
#'      and \code{svc_watermask} is created. If set to \code{NULL} (default) this
#'      step is omitted.
#' @param urbanmask The same as for \code{watermask} but for urban areas.
#' @param r_stream_distance Character string with path to installation of GRASS
#'      function \emph{r.stream.distance} if needed (see \code{Note}).
#' @param r_stream_order Character string with path to installation of GRASS
#'      function \emph{r.stream.order} if needed (see \code{Note}).
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
#' @param svc_masked Output: The same as \code{svc} but with water surface and/or
#'      urban areas cutted out. Will only be created if a \code{watermask} and/or
#'      \code{urbanmask} are/is given. Default: \code{NULL}.
#' @param dir_out Character string specifying output directory (will be created;
#'      nothing will be overwritten).
#' @param svc_ofile Output: Name of file giving properties of \code{svc}s.
#' @param svc_ofile_masked Output: The same as \code{svc_ofile} but for \code{svc_masked}
#'      if a \code{watermask} and/or \code{urbanmask} are/is given. Default: \code{NULL}.
#' @param eha_thres Integer specifying threshold for delineation of \emph{EHA};
#'      parameter for GRASS function \emph{r.watershed}.
#' @param sizefilter Integer specifying minimum size of subbasins (in map units)
#'      not to be removed, smaller basins (artefacts) are removed; parameter for
#'      GRASS function \emph{r.reclass.area}.
#' @param growrad Integer specifying growing radius (in raster cells) to remove
#'      artefacts in raster data; parameter for GRASS function \emph{r.grow}.
#' 
#' @return Function returns nothing. Output raster files as specified in arguments
#'      (see above) are written into GRASS location.
#'
#' @note Prepare GRASS location and necessary raster files in advance and start
#'      GRASS session in R using \code{\link[spgrass6]{initGRASS}}.
#' 
#'      Make sure that the GRASS functions \emph{r.stream.distance} and \emph{r.stream.order}
#'      are available in your GRASS installation or add them manually and specify
#'      the installation path (see \url{http://grasswiki.osgeo.org/wiki/AddOns/GRASS_6#r.stream.distance}).
#'      
#'      See GRASS documentation for further information on GRASS functions and
#'      parameters.
#'      
#'      TODO:\cr
#'        - check arguments
#'        - make R detect GRASS addons automatically (some environment variable issue?!)
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
  mask,
  dem,
  lcov,
  soil,
  watermask=NULL,
  urbanmask=NULL,
  
  # GRASS function #
  r_stream_distance="r.stream.distance",
  r_stream_order="r.stream.order",
  
  # OUTPUT #
  eha,
  flowdir,
  flowacc,
  stream,
  stream_horton,
  elevriv,
  distriv,
  mask_corr,
  svc,
  svc_masked=NULL,
  dir_out,
  svc_ofile,
  svc_ofile_masked=NULL,
  
  # PARAMETERS #
  eha_thres,
  sizefilter,
  growrad
  
) {
  
  ### CALCULATIONS ###
  tryCatch({
    message("START hydrological preprocessing for LUMP using GRASS.")
    message("")
    
    # create output directory
    dir.create(dir_out, recursive=T)
    
    # check output directory
    if (file.exists(paste(dir_out,svc_ofile,sep="/"))) 
      stop(paste0("In output directory '", dir_out, "' the file '", svc_ofile, " already exists!"))
    
    
    # remove mask if any
    execGRASS("r.mask", flags=c("r"))
    
    
    
    # EHA #
    # calculate EHA etc.
    execGRASS("r.watershed", elevation=dem, threshold=eha_thres, half.basin="eha_t1", stream=stream,
              accumulation="flow_accum_t", drainage=flowdir)

    #border cells that receive inflow from outside get negative values, which propagate through entire basin. Check is this is a real problem or just an artefact!
    execGRASS("r.mapcalculator", amap="flow_accum_t", outfile=flowacc, formula="abs(flow_accum_t)")
    
    # set mask and region
    execGRASS("g.region", rast=mask)
    execGRASS("g.region", zoom=mask)
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

    # calculate Horton stream order (works only for non-thinned stream segments!)
    execGRASS("g.region", rast=flowdir) # complains about defiation in resolution of flowdir although it is the same as for the region?!
    execGRASS(r_stream_order, stream=stream, dir=flowdir, horton=stream_horton)
    
        
    
    # RIVER calculations #
    # update: commented out as only calculated river network should be used for further calculations
    #         this ensures compatibility of outputs
    #if river is not specified calculate river network
#     if (is.null(river)) {
#       execGRASS("r.mapcalculator", amap=flowacc, outfile="river_rast", formula=paste("if(A>", river_thres, ",1,0)", sep=""))
#       river <- "river_rast"
#       message(paste("River network computed as ", river, ". Control the output and adjust river_thres if necessary.", sep=""))
#       message("")
#     } 
    
    # calculate distance to river and relative elevation for each cell
    execGRASS(r_stream_distance, stream=stream, dir=flowdir, dem=dem, method="downstream", 
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
    
    warning(paste("Use output raster '", mask_corr, "' based on '", elevriv, "' as new mask as the catchment usually is slightly smaller after running r.stream.distance.", sep=""))
    message("")
    
    
    # SOIL VEGETATION COMPONENTS #
    # create soil vegetation components from soil and landcover/vegetation data
    execGRASS("r.cross", input=paste(soil,lcov,sep=","), output=svc)
    
    # categories of SVCs
    svc_cats <- execGRASS("r.category", map=svc, fs=",", intern=T)

    # transformations ...
    svc_cats_grp <- grep("^0", svc_cats, invert=T, value=T)
    svc_cats_sub <- gsub(",|;", "", svc_cats_grp)
    svc_cats_spl <- strsplit(svc_cats_sub, "category")
    svc_cats_mat <- matrix(as.integer(unlist(svc_cats_spl)),ncol=3, byrow=T)
    colnames(svc_cats_mat) <- c("pid", "soil_id", "veg_id") # same order as input of "r.cross"!
    
    # header of svc output file
    svc_out <- matrix(NA, ncol=12, nrow=nrow(svc_cats_mat))
    svc_out_head <- c("pid", "descr", "soil_id", "veg_id", "musle_k", "musle_c1","musle_c2","musle_c3","musle_c4","musle_p","coarse_frac","manning_n")
    colnames(svc_out) <- svc_out_head
    
    # merge data with output mat
    svc_out[,c("pid", "soil_id", "veg_id")] <- svc_cats_mat
    
    # write output
    write.table(svc_out, paste(dir_out, svc_ofile, sep="/"), quote=F, sep=",", row.names=F)
    
    
    
    # mask water areas and export file with properties if watermask is given
    if(!is.null(watermask) | !is.null(urbanmask)) {
      
      # remove water and/or urban areas from landcover map
      execGRASS("g.copy", rast=paste0(lcov,",lcov_mask_t"))
      if(!is.null(watermask)) {
        execGRASS("r.mapcalculator", amap=watermask, bmap="lcov_mask_t", outfile="lcov_mask_t",
                  formula="if(isnull(A),B,null())", flags=c("overwrite"))
      }
      if(!is.null(urbanmask)) {
        execGRASS("r.mapcalculator", amap=urbanmask, bmap="lcov_mask_t", outfile="lcov_mask_t",
                  formula="if(isnull(A),B,null())", flags=c("overwrite"))
      }
      execGRASS("r.mapcalculator", bmap=soil, amap="lcov_mask_t", outfile="soil_mask_t",
                formula="if(isnull(A),null(),B)")
      
      # create soil vegetation components from soil and landcover/vegetation data
      execGRASS("r.cross", input="soil_mask_t,lcov_mask_t", output=svc_masked)
      
      # categories of SVCs
      svc_cats <- execGRASS("r.category", map=svc_masked, fs=",", intern=T)
      
      # transformations ...
      svc_cats_grp <- grep("^0", svc_cats, invert=T, value=T)
      svc_cats_sub <- gsub(",|;", "", svc_cats_grp)
      svc_cats_spl <- strsplit(svc_cats_sub, "category")
      svc_cats_mat <- matrix(as.integer(unlist(svc_cats_spl)),ncol=3, byrow=T)
      colnames(svc_cats_mat) <- c("pid", "soil_id", "veg_id") # same order as input of "r.cross"!
      
      # header of svc output file
      svc_out <- matrix(NA, ncol=12, nrow=nrow(svc_cats_mat))
      svc_out_head <- c("pid", "descr", "soil_id", "veg_id", "musle_k", "musle_c1","musle_c2","musle_c3","musle_c4","musle_p","coarse_frac","manning_n")
      colnames(svc_out) <- svc_out_head
      
      # merge data with output mat
      svc_out[,c("pid", "soil_id", "veg_id")] <- svc_cats_mat
      
      # write output
      write.table(svc_out, paste(dir_out, svc_ofile_masked, sep="/"), quote=F, sep=",", row.names=F)
    }



    # remove temp files
    execGRASS("g.mremove", rast="*_t,*_t1,*_t2", flags=c("f"))
    
    message("DONE!")
    
    
    # if an error occurs delete all temporary output
  }, error = function(e) {
    execGRASS("g.mremove", rast="*_t,*_t1,*_t2", flags=c("f"))
    stop(paste(e))  
  })
  
} # EOF
