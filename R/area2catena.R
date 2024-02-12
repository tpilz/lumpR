# lumpR/area2catena.R
# Copyright (C) 2014-2018 Tobias Pilz, Till Francke
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


#' Calculates mean catena from spatial data
#' 
#' Takes raster data from a GRASS location and calculates catena properties.
#' 
#' @param mask Raster file to be used as MASK in the GRASS location defining the
#'      area of interest. E.g. \code{basin_out} of \code{\link[lumpR]{calc_subbas}}.
#' @param flowacc Name of flow accumulation raster map in GRASS location. Can
#'      be created with \code{\link[lumpR]{lump_grass_prep}}.
#' @param eha Name of elementary hillslope area raster map in GRASS location.
#'      Can be created with \code{\link[lumpR]{lump_grass_prep}}.
#' @param distriv Name of distance to river raster map in GRASS location. Can
#'      be created with \code{\link[lumpR]{lump_grass_prep}}.
#' @param elevriv Name of relative elevation raster map in GRASS location. Can
#'      be created with \code{\link[lumpR]{lump_grass_prep}}.
#' @param supp_quant Character vector containing names of quantitative
#'      supplemental raster maps in GRASS location; leave empty if you have none.
#' @param supp_qual Character vector containing names of qualitative
#'      supplemental raster maps in GRASS location; leave empty if you have none.
#' @param dir_out Character string specifying output directory (will be created;
#'      nothing will be overwritten).
#' @param catena_out Output: Name of output file containing mean catena information
#'      as input for \code{\link[lumpR]{prof_class}}. To save computation time and
#'      memory in cases of large catchments and/or high resolution, specify a file
#'      name with ending \code{*.RData}. In such a case, a compressed RData file will be
#'      written instead of a text file. 
#' @param catena_head_out Output: Name of output header file containing meta-information
#'      as input for \code{\link[lumpR]{prof_class}}; manual adjustment necessary.
#' @param ridge_thresh Integer specifying threshold of flow accumulation, below
#'      which a cell is considered a start of a flowpath (usually 1 for D8
#'      flowaccumulation grids, (DEFAULT), 1.5 for MFD-grids).
#' @param min_cell_in_slope Integer specifying minimum number of cells a hillslope
#'      area must have, all smaller ones are skipped. Default: 30.
#' @param min_catena_length Integer specifying minimum number of sampling points
#'      (cells) a catena should have. If there are less, the catena is not saved.
#'      Default: 3.
#' @param max_riv_dist Integer specifying maximum distance to river [in cells]:
#'      if the closest cell of an EHA is farther than \code{max_riv_dist}, the EHA
#'      is skipped, otherwise all distances within the EHA are redurced by the
#'      distance of the closest cell to river. Default: 10.
#' @param plot_catena logical; produce plots (scatter, mean catena, etc.) for
#'      each area / class (written into sub-directory \emph{plots_area2catena}).
#' @param grass_files logical; produce GRASS reclassification files for qualitative
#'      raster data. If an attribute name starting with 'svc' is found, the respective reclass file is produced anyway.
#' @param ncores Integer specifying number of cores that should be used for computation - allows faster parallel computations (see Details).
#' @param eha_subset NULL or integer vector with subset of EHA ids that shall
#'      be processed (for debugging and testing).
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing summary
#'      after function execution)? Default: \code{FALSE}.
#'      
#' @return Function returns nothing. Output files are written into output directory
#'      as specified in arguments.
#'  
#' @note Prepare GRASS location and necessary raster files in advance (e.g. using
#'      \code{\link[lumpR]{lump_grass_prep}}) and start GRASS session in R using 
#'      \code{\link[rgrass]{initGRASS}}.
#'      
#'      \bold{IMPORTANT:} Herein, when specifying the GRASS input maps, please do
#'      explicitly refer to the mapset if it is different from the mapset given in
#'      initGRASS() (even PERMANENT!), as otherwise internally used read_RAST() command
#'      resulted in errors under Windows.
#'      
#'      In case of \bold{long computation times or memory issues}, try \code{plot_catena = FALSE}
#'      and specify an RData file as \code{catena_out}. Furthermore, make sure your
#'      raster maps are not overly large (i.e. containing lots of NULL values around
#'      the region of interest) and are of type \code{CELL} (i.e. integer) where it makes
#'      sense (check with \code{r.info} in GRASS).
#'      
#'      Parallel option (\code{ncores}>1) may speed-up computations. It requires the packages 
#'      \code{doMC} and \code{doParallel}. Set \code{ncores} to a value less than the number of cores
#'      your computer has; larger values will decrease performance.
#'      GUIs such as RStudio may not produce some runtime messages (within parallel
#'      foreach loop).
#'      
#'      File \code{catena_out} contains information about the representative catena
#'      for each hillslope (EHA). For meaning of columns see file \code{catena_head_out}.
#'      It usually contains the catena ID, the catena's profile point IDs, relative
#'      vertical elevation above hillside toe, and supplemental information averaged
#'      over all raster cells associated with a specific catena profile point. For
#'      qualitative data the latter means the areal fraction of a specific attribute
#'      class (sum over all classes of an attribute should be equal to one for a profile
#'      point), for quantitative data it is a numerical value. Averages over raster
#'      cells are weighted by relative flow path densities of the raster cells. For
#'      more details on the algorithm see the reference below.
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
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}, Till Francke \email{francke@@uni-potsdam.de}
#'
area2catena <- function(
  
  ### INPUT ###
  # GRASS raster #
  mask=NULL,
  flowacc=NULL,
  eha=NULL,
  distriv=NULL,
  elevriv=NULL,
  supp_quant=NULL,
  supp_qual=NULL,
  
  # OUTPUT #
  dir_out="./",
  catena_out=NULL,
  catena_head_out=NULL,
  
  # PARAMETERS #
  ridge_thresh=1,
  min_cell_in_slope=30,
  min_catena_length=3,
  max_riv_dist=10,
  plot_catena=F,
  grass_files=F,
  ncores=1,
  eha_subset=NULL,
  overwrite=F,
  silent=F
) {
  
### PREPROCESSING ###----------------------------------------------------------
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START area2catena()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")
  
# checks #---------------------------------------------------------------------
  #tryCatch(gmeta(), error = function(e) stop("Cannot execute GRASS commands. Maybe you forgot to run initGRASS()?"))
  test_grass()
  
  # check output directory
  if (!overwrite & ( file.exists(paste(dir_out,catena_out,sep="/")) | file.exists(paste(dir_out,catena_head_out,sep="/")) ) ) 
    stop(paste0("In output directory '", dir_out, "' the files '", catena_out, "' and/or '", catena_head_out, "' already exist!"))
  
  if (plot_catena) {
    if(length(dir(paste(dir_out, "plots_area2catena", sep="/"))) != 0)
    {
      if (overwrite)
        file.remove(dir(paste(dir_out, "plots_area2catena", sep="/"), full.names = T))
      else
        stop(paste0("Output directory for plots '", dir_out, "/plots_area2catena/' is not empty!"))
    }
    
    dir.create(paste(dir_out, "plots_area2catena", sep="/"), recursive=T, showWarnings = F)
  }
  
  # create output directory
  dir.create(dir_out, recursive=T, showWarnings = F)
  
  # argument checks
  if(is.null(mask))
    stop("The name of a raster used as mask within the GRASS region has to be given to make sure calculations are done in the expected area!")
  if(is.null(flowacc))
    stop("The name of a flow accumulation raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(eha))
    stop("The name of a elementary hillslope area raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(distriv))
    stop("The name of a distance to river raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(elevriv))
    stop("The name of a relative elevation raster map within the mapset of your initialised GRASS session has to be given!")
  if(is.null(catena_out))
    stop("A name for the file containing mean catena information has to be given!")
  if(is.null(catena_head_out))
    stop("A name for the meta-information file has to be given!")
  
  check_raster(mask,"mask")
  check_raster(flowacc,"flowacc")
  check_raster(eha,"eha")
  check_raster(distriv,"distriv")
  check_raster(elevriv,"elevriv")
  
  #check existence of supplementary information maps
   if (length(supp_qual)==0) supp_qual=NULL else
     for (i in supp_qual) 
        check_raster(i, paste0("supp_qual[",i,"]"))
    
  if (length(supp_quant)==0) supp_quant=NULL else
    for (i in supp_quant) 
        check_raster(i,paste0("supp_quant[",i,"]"))

  # suppress annoying GRASS outputs
  tmp_file <- file(tempfile(), open="wt")
  sink(tmp_file, type="output")
  
  # suppress warnings in silent mode
  if(silent){
    tmp_file2 <- file(tempfile(), open="wt")
    sink(tmp_file2, type="message")
    oldw <- getOption("warn")
    options(warn = -1)
  }
  
  if(!silent) message("% OK")
  
  
  options(error=cleanup) #in case of errors, clean up and reset to original warning and messaging state
  
   
  
### CALCULATIONS ###-----------------------------------------------------------
  
    
# load files from grass #------------------------------------------------------
    if(!silent) message("%")
    if(!silent) message("% Load data from GRASS...")

    # set mask to make sure calculations are done exactly within expected area
    if (mask != "MASK") 
      cmd_out <- execGRASS("g.copy", raster=paste0(mask,",MASK"), flags = "overwrite", intern = T)
        
    # load flow accumulation
    flowaccum <- read_RAST(flowacc)
    flowaccum_rast <- raster(flowaccum)
    
    # load relative elevation
    relelev <- read_RAST(elevriv)
    relelev_rast <- raster(relelev)
    
    # load distance to river
    dist2river <- read_RAST(distriv)
    dist2river_rast <- raster(dist2river)
    
    # load EHAs
    eha_in <- read_RAST(eha)
    eha_rast <- raster(eha_in)
    
    
    # load qualitative supplemental data
    qual_rast <- NULL # initialise object containing all qualitative raster layers
    supp_data_classnames <- NULL # initialise object containing different classnames per attribute
    n_supp_data_qual_classes <- NULL # initialise object containing number of classes per attribute
    if (!is.null(supp_qual)) 
      supp_qual=supp_qual[supp_qual!=""]
    
    if (!is.null(supp_qual)) 
    {  
      for (i in supp_qual) {
        tmp <- read_raster(i)
        #qual_rast <- raster::stack(tmp, qual_rast) #this doesn't seem to work any longer
        qual_rast = c(tmp, qual_rast) #stack raster layers
        supp_data_classnames[[i]] <- terra::unique(tmp)[,1]
        n_supp_data_qual_classes <- c(n_supp_data_qual_classes, length(supp_data_classnames[[i]]))
      }
      
      # convert (at) symbol to point (in case input comes from another GRASS mapset; read_RAST6() converts it to point implicitly which causes errors during later processing)
      supp_qual <- gsub("[-+*/@.?!]", ".", supp_qual)
      
      names(n_supp_data_qual_classes) <- supp_qual
      names(supp_data_classnames) <- supp_qual
      names(qual_rast)            = supp_qual
    }  
    
    # load quantitative supplemental data
    quant_rast <- NULL # initialise object containing all quantitative raster layers
    for (i in rev(supp_quant)) {
      tmp <- read_raster(i)
      #quant_rast <- raster::stack(quant_rast, tmp) #this doesn't seem to work any longer
      quant_rast = c(quant_rast, tmp) #stack raster layers
    }
    
    # convert (at) symbol to point (in case input comes from another GRASS mapset; read_RAST6() converts it to point implicitly which causes errors during later processing)
    supp_quant <- gsub("[-+*/@.?!]", ".", supp_quant)
    
    if(exists("tmp"))
      rm(list=c("tmp"))
    
    
    # compare Rasters for extent, no. of rows and cols, CRS, resolution and origin
    if (!is.null(qual_rast) & !is.null(quant_rast)) {
      comp_val <- compareRaster_i(list(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                                qual_rast, quant_rast), res=T, orig=T)
    } else if(is.null(qual_rast) & !is.null(quant_rast)) {
      comp_val <- compareRaster_i(list(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                                quant_rast), res=T, orig=T)
    } else if(!is.null(qual_rast) & is.null(quant_rast)) {
      comp_val <- compareRaster_i(list(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                                qual_rast), res=T, orig=T)
    } else if(is.null(qual_rast) & is.null(quant_rast)) {
      comp_val <- compareRaster_i(list(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast), 
                                res=T, orig=T)
    }
    
    
    if (comp_val) {
      if(!silent) message("% -> All input data loaded and checked for extent, CRS and resolution successfully.")
      if(plot_catena & !silent) {
        message(paste0("% -> Plots will be produced and written to '", dir_out, "/plots_area2catena/'."))
      }
    } else {
      stop("ERROR: Loaded input rasters are not of the same extent, no. of rows/cols, CRS, resolution and/or origin!")
    }
    
    if(!silent) message("% OK.")
    
    
    
    
    
# process ehas #---------------------------------------------------------------
    # get resolution of spatial data
    xres <- terra::xres(eha_rast)
    
    # identify EHAs
    eha_ids <- terra::unique(eha_rast)
    if (!is.null(eha_subset)) eha_ids <- eha_subset
    
    # LOOP over EHAs
    if(!silent) message("%")
    if(!silent) message(paste("% Processing ", length(eha_ids), " EHAs. This might take a while depending on catchment size and number of cpu cores.", sep=""))
    if (!is.null(eha_subset) & !silent) message("% -> Note that this is just a subset as specified in the argument 'eha_subset'.")
    
    id <- NULL # to remove "R CMD CHECK ..." Note of "no visible binding for global variable 'id'"
  
    
    ##initialize parallelism
    if (ncores>1)
    {  
      if(suppressPackageStartupMessages(require(doMC)))
        # register cores
        registerDoMC(cores=ncores) else
          if (suppressPackageStartupMessages(require(doParallel)))
          {
            cl <- makePSOCKcluster(ncores) #make cluster, so we can explicitly close it later
            registerDoParallel(cl)
          } else
          {
            warning("No package for parallel backend (doMC, doParallel) found, reverting to single-core mode")
            ncores=1
          }
    }
    
    if (ncores==1) registerDoSEQ() # specify that %dopar% should run sequentially
    
    #parallel call using dopar (if no parallel backend is registered, this falls back to serial execution) 
    # NOTE: in case of problems within the foreach-loop set foreach argument .errorhandling="pass" and execute str(logdata) after the loop to show error messages
    logdata <- foreach (id = eha_ids, .combine=rbind, .errorhandling='remove', .options.multicore=list(silent=FALSE),
                        .inorder=FALSE, .export="eha_calc") %dopar% {
      eha_calc(id, eha_rast, flowaccum_rast, dist2river_rast, relelev_rast, supp_quant, supp_qual,
               n_supp_data_qual_classes, quant_rast, qual_rast, supp_data_classnames,
               min_cell_in_slope, max_riv_dist, plot_catena, ridge_thresh, min_catena_length,
               xres,dir_out)
    }
    
    if(!silent) message("% OK.")
# check output #---------------------------------------------------------------
    if(!silent) message("%")
    if(!silent) message("% Check and write output...")
    
    if (exists("cl")) #close cluster, if existing
      stopCluster(cl)
    
    # check if anything was produced (if NULL an unexpected error might have occured)
    if(is.null(logdata) || (nrow(logdata)==0))
      stop("Error: No valid EHAs remaining after processing. Something's fishy, check the warnings, eha_subset and coverage of the layers.")
    
    # check for severe errors
    if(any(logdata$error == 666))
      stop("Error: A problem occurred when averaging qualitative supplemental information. Check your data and contact the package author(s) if the problem remains.")
    
    if(any(logdata$error == 999))
      stop("Error: An unexpected error occurred in the EHA processing loop. Check your data and contact the package author(s) if the problem remains.")
    
    
    
    logdata = logdata[order(logdata[,1],logdata[,2]),] #sort by ID and distance (ensures consistent ordering even with .inorder=FALSE)
    
    # sort out erroneous values and store for diagnostics
    warn_ehas  <- unique(logdata[logdata$error == 5 | logdata$error == 6 | logdata$error == 7, c(1,ncol(logdata))])
     
    erroneous <- (logdata$error > 0 & logdata$error < 5)
    error_ehas <- logdata[erroneous, c(1,ncol(logdata))]
    
    
    logdata <- logdata[(logdata$error %in% c(0, warn_ehas$error)), -ncol(logdata)] #keep only valid rows and those with warnings
    
    # check for NAs
    if(any(is.na(logdata))) {
      warning(paste("NA values in the output which may crash function 'prof_class'! 
           This might be caused by NAs in the input rasters. Check file", catena_out))
    }
    
    
    
    
# write output #---------------------------------------------------------------
    #out_pre <- mapply(logdata[,c(3:length(logdata))], FUN=function(x) formatC(x, format="f", digits=3))
    #out_fmt <- cbind(logdata[,c(1,2)], out_pre)
    #format output to reasonable number of digits
    logdata <- round(logdata,3)
  
    if(grepl(".RData$", catena_out)) {
      save(logdata, file=paste(dir_out,catena_out, sep="/"))
    } else {
      write.table(logdata, paste(dir_out,catena_out, sep="/"), col.names=F, row.names=F, quote=F, sep="\t")
    }
    
    # write header file
    write("#This file works as a header to the output of area2catena. Don't add additional headerlines.",
          file=paste(dir_out,catena_head_out, sep="/"), append=F)
    write('#1. line after header: description/field names of the data columns contained in file catena_out',
          file=paste(dir_out,catena_head_out, sep="/"), append=T)
    write('#2. line after header: specifies, how many columns of data belong to the respective data-field given in line 1',
          file=paste(dir_out,catena_head_out, sep="/"), append=T)
    write('#3. line after header: number of classes/ weighting factors for classification process',
          file=paste(dir_out,catena_head_out, sep="/"), append=T)
    write('#4. line after header: factors used for weighting in partition process (column: 1: number of TC to create; 2.: partition method (not yet used); 3.: not used; 4-nn: weighting of supplemental data in TC-partitioning)',
          file=paste(dir_out,catena_head_out, sep="/"), append=T)
    # write attribute names
    att_names <- c("id", "p_no", "elevation", supp_quant, supp_qual, "slope_width")
    write(att_names,
          file=paste(dir_out,catena_head_out, sep="/"), ncolumns=length(att_names), append=T, sep="\t")
    #write number of columns occupied by each attribute
    col_no <- c(rep(1,3), rep(1,length(supp_quant)), n_supp_data_qual_classes, 1)
    write(col_no,
          file=paste(dir_out,catena_head_out, sep="/"), ncolumns=length(col_no), append=T, sep="\t")
    #write dummy weighting factors
    w_no <- rep(0, 3+length(supp_quant)+length(n_supp_data_qual_classes)+1)
    write(c(w_no, "[adjust weighting factors here and remove this comment]"),
          file=paste(dir_out,catena_head_out, sep="/"), ncolumns=length(w_no)+1, append=T, sep="\t")
    write(c(w_no, "[adjust weighting factors here and remove this comment]"),
          file=paste(dir_out,catena_head_out, sep="/"), ncolumns=length(w_no)+1, append=T, sep="\t")
    
    #generate qualitative-data reclassification file
    #Generate output files for reclassification (input class-IDs vs. internally used IDs)
    #(area2catena creates continuous class numbering; restoring the orginal classes will require these files)

    for (i in 1:length(supp_qual)) {
      if (grass_files | grepl("^svc", supp_qual[i])) {
        filename=paste0(dir_out, "/reclass_", supp_qual[i], ".txt")
        write(c("new_id", "original_id"),
              file=filename, ncolumns=2, append=F, sep="\t")
        for (n in 1:n_supp_data_qual_classes[i]) {
          write(c(n, supp_data_classnames[[i]][n]),
                file=filename, ncolumns=2, append=T, sep="\t")
        }
      }
    }    

    # FINAL REPORT #
    if(!silent) message("% OK.")
    if(!silent) message("%")
    if(!silent) message("% Final report:")
    if(!silent) message('% -> All outputs produced.')
    if(!silent) message(paste("% -> ", length(eha_ids)-length(which(logdata$error == 2))-length(which(logdata$error == 1)), ' slopes treated', sep=""))
    if(!silent) message(paste("% -> ", sum(error_ehas$error == 1), ' slopes skipped that have less than ', min_cell_in_slope, ' cells', sep=""))
    if(!silent) message(paste("% -> ", sum(error_ehas$error == 2), ' slopes skipped that are not adjacent to river', sep=""))
    if(!silent) message(paste("% -> ", sum(error_ehas$error == 4), ' slopes skipped that have a mean length shorter than ', min_catena_length, sep=""))
    if(!silent) message(paste("% -> ", sum(error_ehas$error == 3), ' slopes skipped that have only cells with dist2river=0.', sep=""))
    if(!silent) message(paste("% -> ", sum(warn_ehas$error  == 5), ' warnings due to slopes with no flow_accum less than ', ridge_thresh, sep=""))
    if(!silent) message(paste("% -> ", sum(warn_ehas$error  == 6), ' warnings due to slopes with NAs in topographics grids', sep=""))
    if(!silent) message(paste("% -> ", sum(warn_ehas$error  == 7), ' warnings due to slopes with NAs in auxiliary grids', sep=""))
    if(!silent) message('%')
    if(!silent) message("% DONE!")
    if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    
    
    
  
  # if an error occurs delete all temporary output
  
  
} # EOF




### INTERNAL FUNCTION: processing EHA ###--------------------------------------

# this function contains the algorithm for the derivation of a representative catena for each EHA
# as described in Francke et al. (2008)
eha_calc <- function(curr_id, eha_rast, flowaccum_rast, dist2river_rast, relelev_rast, supp_quant, supp_qual,
                     n_supp_data_qual_classes, quant_rast, qual_rast, supp_data_classnames,
                     min_cell_in_slope, max_riv_dist, plot_catena, ridge_thresh, min_catena_length,
                     xres,dir_out, supp_quant_maxflow=NULL) {
  
  errcode <- 0
  
res = try( #catch unexpected errors
  {  

    # EHA: CHECKS and PREPARATIONS #-----------------------------------------------
  # determine cell indices of curr_id
  curr_cells <- which(values(eha_rast) == curr_id)
  
  # determine number of cells in eha and skip processing if less than min_cell_in_slope
  # ERROR CODE 1
  if (length(curr_cells) < min_cell_in_slope) {
    message(paste('% -> WARNING: EHA ', curr_id, ' skipped because of low number of cells (', length(curr_cells), ')', sep=""))
    errcode <- 1
    return(data.frame(output=t(c(curr_id, rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4 - 1))), error=errcode))
  }
  
  # extract values out of raster objects into ordinary vectors to save time (internal calls to raster objects take time)
  flowaccum_vals  <- flowaccum_rast [curr_cells] #?Till: in parallel mode, this requires all the large rasters to be available to each thread. I wonder is this consumes too much replicates and overhead. Passing just the area of the current curr_cells may save ressources
  dist2river_vals <- dist2river_rast[curr_cells]
  relelev_vals    <- relelev_rast   [curr_cells]
  if(!is.null(quant_rast)) quant_vals <- raster::extract(quant_rast, curr_cells)
  if(!is.null(qual_rast))  qual_vals  <- raster::extract(qual_rast,  curr_cells)
  
  #detect NA values
  
  na_rasters= NULL
  if (any(is.na(flowaccum_vals)))  na_rasters = c(na_rasters, "'flowacc'")
  if (any(is.na(dist2river_vals))) na_rasters = c(na_rasters, "'distriv'")
  if (any(is.na(relelev_vals)))    na_rasters = c(na_rasters, "'elevriv'")

  if (!is.null(na_rasters)) {  # cells found with NAs in the mandatory grids
    na_vals = is.na(flowaccum_vals) | is.na(dist2river_vals) | is.na(relelev_vals) #detect NA values 
    message(paste('% -> WARNING: EHA ', curr_id, " has NA cells in raster(s) ", paste(na_rasters, collapse=","),". May be OK for EHAs at divide. Cells ignored.", sep=""))
    curr_cells <- curr_cells[!na_vals]
    flowaccum_vals  <- flowaccum_vals [!na_vals]
    dist2river_vals <- dist2river_vals[!na_vals]
    relelev_vals    <- relelev_vals   [!na_vals]
    errcode <- 6
  }
  
  na_vals <- NULL
  if(!is.null(quant_rast))
    #na_vals <-            apply(!is.finite(quant_vals), MARGIN=2, sum)
    #na_vals <- sum(!is.finite(as.matrix(quant_vals)))
    na_vals <- c(na_vals, apply(!is.finite(as.matrix(quant_vals)), MARGIN=2, sum))
  if(!is.null(qual_rast))
    #na_vals <- c(na_vals, apply(!is.finite(qual_vals), MARGIN=2, sum))
    #na_vals <-  c(na_vals, sum(!is.finite(as.matrix(qual_rast2))))
    na_vals <- c(na_vals, apply(!is.finite(as.matrix(qual_vals)), MARGIN=2, sum))
    
  if (any(na_vals>0)) {  # cells found with NAs in auxiliary grids
    message(paste('% -> WARNING: EHA ', curr_id, ': NAs or zeros in the grid(s) ', paste(names(na_vals[na_vals]), collapse=', ') ,' (max. ', max(na_vals), ' values).', sep=""))
    errcode <- 7
  }
  
  # determine closest distance to river and skip processing if more than max_riv_dist
  # ERROR CODE 2
  if(min(dist2river_vals) > max_riv_dist) {
    message(paste("% -> WARNING: EHA ", curr_id, ' skipped, not adjacent to channel / farther than ', max_riv_dist, ' cells (',min(dist2river_vals),')', sep=""))
    return(data.frame(output=t(c(curr_id, rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4 - 1))), error=2))
  } else {
    dist2river_vals <- dist2river_vals - min(dist2river_vals)
  }
  
  # test whether only river cells available (all dist2river = 0) and skip eha_clumped if so
  # ERROR CODE 3
  if(max(dist2river_vals) == 0) {
    message(paste("% -> WARNING: EHA ", curr_id, ' skipped because for all cells dist2river=0.', sep=""))
    return(data.frame(output=t(c(curr_id, rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4 - 1))), error=3))
  }
  
  
# EHA: compute MEAN CATENA LENGTH #--------------------------------------------
  # find all cells that are the beginning of a flowpath
  curr_entries <- which(flowaccum_vals<=ridge_thresh)
  if (!any(curr_entries)) {  # no cells found, strange, but try to fix this problem
    message(paste('% -> WARNING: EHA ', curr_id, ' has no cells with flowpath start (below ', ridge_thresh, ')', sep=""))
    # reduce threshold to minimum flow accumulation found in current slope
    curr_entries <- which(flowaccum_vals <= min(flowaccum_vals))
    errcode <- 5
  }

  # compute mean of flowpaths as average catena length (calclength-method, Chochrane & Flanagan, 2003) #
  mean_length <- sum(dist2river_vals[curr_entries]^2)/sum(dist2river_vals[curr_entries])
 
  # skip very short catenas
  # ERROR CODE 4
  if (mean_length < min_catena_length) {
    message(paste("% -> WARNING: EHA ", curr_id, ' skipped because of low length (', mean_length, ')', sep=""))
    return(data.frame(output=t(c(curr_id, rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4 - 1))), error=4))
  }
  
  # PLOT #
  if (plot_catena) {
    png(filename=paste0(dir_out, "/plots_area2catena/plots_area2catena_", curr_id, ".png"))
    # scatterplot
    plot(dist2river_vals*xres, relelev_vals, 
         pch=20, ylab="rel. elevation [m]", xlab="flowpath length [m]", cex=.5,
         main=paste("id: ", curr_id, ", ", length(curr_cells), " cells", sep=""))
    
    # plot mean length
    abline(v=mean_length*xres, lty=5)
  }
  
  
# EHA: calculate properties (loop over reference points) #---------------------
  # resolution, number of data points describing the mean catena
  res <- ceiling(mean_length)
  
  # minimum number of cells necessary for averaging one point of the mean catena
  min_no_cells <- 1
  
  # initialisations
  out_x <- NULL # initialise vector of point numbers for current EHA
  out_y <- NULL # initialise vector of average rel. elevation for current point in EHA
  supp_attrib_mean <- matrix(NA, nrow=length(supp_quant)+sum(n_supp_data_qual_classes), ncol=res+1) # init. matrix of mean supplemental information
  density <- array(NA, res+1) # initialise vector for density values
  entry_missing <- 0 # flag indicating that the value for the previous point in the mean catena could not be computed
  out_combined <- NULL # combined output of one catena 
  
  # loop over data points of mean catena
  for (j in 0:res) {
    # point number in catena output
    out_x[j+1] <- j
    
    # initial search range used for averaging the current point of the mean catena
    search_range <- 0.5
    
    # seek all entries within the "spacing" range that will be averaged to one point in the output catena
    while (1) {
      curr_entries <- which(dist2river_vals>=(j-search_range) & dist2river_vals<(j+search_range))

      # check, if enough cells have been found to be used for averaging this point of the catena
      if (j==0 || length(curr_entries)>=min_no_cells || search_range==100) {     
        break
      }
      # otherwise, increase search radius
      search_range <- search_range+0.5
    }
    
    # calculate averages
    if (any(curr_entries)) {
      # compute density of mean catena
      density[j+1] <- length(curr_entries) / length(curr_cells)
      
      # square root of flowaccumulation (~flow path density)
      flowaccum_sqrt <- sqrt(flowaccum_vals[curr_entries])
      sum_flowaccum_sqrt <- sum(flowaccum_sqrt) #safe some calculation time
      
      # averaging of elevations of all points
      # mean weighted with square root of flowaccumulation (~flow path density)
      out_y[j+1] <- sum(relelev_vals[curr_entries]*flowaccum_sqrt)/sum(flowaccum_sqrt)
      
      # compute average quantitative supplemental data
      if (!is.null(quant_rast)) {
        for (k in 1:length(supp_quant)) {
          supp_attrib_mean[k,j+1] <- weighted.mean(x=quant_vals[curr_entries,k], w=flowaccum_sqrt/sum_flowaccum_sqrt, na.rm=TRUE)  #weighted mean, weighted with sqrt(flow_accum) and NAs removed
        }
      }
      
      # for summing up the number of classes needed by the successive qualitative attributes
      col_counter <- 0
      # to ensure that the qualitative attributes are inserted after the quantitative ones
      quant_columns <- length(supp_quant)
      
      # compute average qualitative supplemental data
      if (!is.null(qual_rast)) {
        for (k in supp_qual) { # loop over attributes
          # select attribute values
          curr_att_val <- qual_vals[curr_entries,k]
          # do averaging for each class of each attribute separately
          for (kk in 1:n_supp_data_qual_classes[k]) { # loop over attribute's classes
            supp_attrib_mean[quant_columns+kk+col_counter,j+1] <- sum((curr_att_val==supp_data_classnames[[k]][kk])*flowaccum_sqrt)/sum(flowaccum_sqrt) 
          }

          # check averages (should sum up to one for each attribute), SEVERE ERROR CODE 666
          if(sum(supp_attrib_mean[(quant_columns+col_counter+1):(quant_columns+col_counter+n_supp_data_qual_classes[k]),j+1]) < 0.999) {
            message(paste("% -> WARNING: For EHA ", curr_id, " areal fractions of qualitative supplemental attribute '", k, "' does not sum to one for profile point ", j+1, sep=""))
            if (plot_catena) dev.off()
            return(data.frame(output=t(c(curr_id, rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4 - 1))), error=666))
          }
          
          # update counter
          col_counter <- col_counter+n_supp_data_qual_classes[k]
        } 
        
      }
      
      if (entry_missing) {
        # use the values of the current point to assign the previous point, which had problems in the calculation 
        supp_attrib_mean[,j] <- supp_attrib_mean[,j+1]
        entry_missing <- 0
      }
      
      # if no curr_entries
    } else {
      message(paste('% -> WARNING: problems calculating mean catena for ', curr_id, sep=""))
      
      # flag indicating that the value for the previous point in the mean catena could not be computed
      if (j!=0) entry_missing <- 1
    } # end if curr_entries
    
    
# OUTPUT #---------------------------------------------------------------------
    # collect logdata, '.combine' arg in 'foreach' command
    out_combined <- rbind(out_combined, 
                          c(c(curr_id, out_x[j+1]+1), 
                            out_y[j+1],
                            supp_attrib_mean[,j+1],
                            density[j+1]))
    
  } # end of reference points loop
  # PLOT mean catena + cumulated density
  if (plot_catena) {
    lines((out_x)*xres,out_y,lwd=2,col="red")
    
    #close graphic device
    dev.off()
  }
  
  
  # output aggregation by foreach loop via .combine method
  return(data.frame(output=out_combined, error=errcode))
  
}, silent=TRUE) #end of try

if (class(res)=="try-error")  #unexpected error
{  
  print(paste0("Unexpected error: ", attr(res, "condition")))
  return(data.frame(output=t(c(curr_id, rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4 - 1))), error=999))
}  
} # EOF
