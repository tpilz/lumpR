
#' Calculates mean catena from spatial data
#' 
#' Takes raster data from a GRASS location and calculates catena properties.
#' 
#' @param flowacc Name of flow accumulation raster map in GRASS location. Can
#'      be created with \code{\link[LUMP]{lump_grass_prep}}.
#' @param eha Name of elementary hillslope area raster map in GRASS location.
#'      Can be created with \code{\link[LUMP]{lump_grass_prep}}.
#' @param distriv Name of distance to river raster map in GRASS location. Can
#'      be created with \code{\link[LUMP]{lump_grass_prep}}.
#' @param elevriv Name of relative elevation raster map in GRASS location. Can
#'      be created with \code{\link[LUMP]{lump_grass_prep}}.
#' @param supp_quant Character vector containing names of quantitative
#'      supplemental raster maps in GRASS location; leave empty if you have none.
#' @param supp_qual Character vector containing names of qualitative
#'      supplemental raster maps in GRASS location; leave empty if you have none.
#' @param dir_out Character string specifying output directory (will be created;
#'      nothing will be overwritten).
#' @param catena_out Output: Name of output file containing mean catena information
#'      as input for \code{\link[LUMP]{prof_class}}.
#' @param catena_head_out Output: Name of output header file containing meta-information
#'      as input for \code{\link[LUMP]{prof_class}}; manual adjustment necessary.
#' @param ridge_thresh Integer specifying threshold of flow accumulation, below
#'      which a cell is considered a start of a flowpath (usually 1 for D8
#'      flowaccumulation grids).
#' @param min_cell_in_slope Integer specifying minimum number of cells a hillslope
#'      area must have, all smaller ones are skipped.
#' @param min_catena_length Integer specifying minimum number of sampling points
#'      (cells) a catena should have. If there are less, the catena is not saved.
#' @param max_riv_dist Integer specifying maximum distance to river [in cells]:
#'      if the closest cell of an EHA is farther than \code{max_riv_dist}, the EHA
#'      is skipped, otherwise all distances within the EHA are redurced by the
#'      distance of the closest cell to river.
#' @param plot_catena logical; produce plots (scatter, mean catena, etc.) for
#'      each area / class (written into sub-directory \emph{plots_area2catena}).
#' @param grass_files logical; produce GRASS reclassification files for qualitative
#'      raster data.
#' @param ncores Ineger specifying number of cores that should be used for computation.
#' @param eha_subset NULL or integer vector with subset of EHA ids that shall
#'      be processed (for debugging and testing).
#'      
#' @return Function returns nothing. Output files are written into output directory
#'      as specified in arguments.
#'  
#' @note Prepare GRASS location and necessary raster files in advance (e.g. using
#'      \code{\link[LUMP]{lump_grass_prep}}) and start GRASS session in R using 
#'      \code{\link[spgrass6]{initGRASS}}.
#'      
#'      GUIs such as RStudio may not produce some runtime messages (within parallel
#'      foreach loop).
#'      
#'      TODO:\cr
#'        - check arguments (tests for spatial data already included)\cr
#'        - in case of problems within the foreach-loop set foreach
#'          argument \emph{.errorhandling="pass"} and execute \code{str(logdata)} after
#'          the loop to show error messages
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
area2catena <- function(
  
  ### INPUT ###
  # GRASS raster #
  flowacc,
  eha,
  distriv,
  elevriv,
  supp_quant=NULL,
  supp_qual=NULL,
  
  # OUTPUT #
  dir_out="./",
  catena_out,
  catena_head_out,
  
  # PARAMETERS #
  ridge_thresh=1,
  min_cell_in_slope,
  min_catena_length,
  max_riv_dist,
  plot_catena=F,
  grass_files=F,
  ncores=1,
  eha_subset=NULL
  
) {
  ### CALCULATIONS ###
  
  message("START 'area2catena'.")
  message("")
  
  if (ncores>1)
    if(require(doMC))
    # register cores
      registerDoMC(cores=ncores) else
      {warning("Package doMC not found, reverting to single-core mode"); ncores=1}
  else
  ncores=1	
  
  # LOAD FILES FROM GRASS #
  # create output directory
  dir.create(dir_out, recursive=T)
  
  # check output directory
  if (file.exists(paste(dir_out,catena_out,sep="/")) | file.exists(paste(dir_out,catena_head_out,sep="/"))) 
    stop(paste0("In output directory '", dir_out, "' the files '", catena_out, "' and/or '", catena_head_out, "' already exist!"))
  
  if (plot_catena) {
    if(length(dir(paste(dir_out, "plots_area2catena", sep="/"))) != 0)
      stop("Output directory for plots '", dir_out, "/plots_area2catena/' is not empty!")
    
    dir.create(paste(dir_out, "plots_area2catena", sep="/"), recursive=T)
  }
  
  # load flow accumulation
  flowaccum <- readRAST6(flowacc)
  flowaccum_rast <- raster(flowaccum)
  
  # load relative elevation
  relelev <- readRAST6(elevriv)
  relelev_rast <- raster(relelev)
  
  # load distance to river
  dist2river <- readRAST6(distriv)
  dist2river_rast <- raster(dist2river)
  
  # load EHAs
  eha_in <- readRAST6(eha)
  eha_rast <- raster(eha_in)
  
  # load qualitative supplemental data
  qual_rast <- NULL # initialise object containing all qualitative raster layers
  supp_data_classnames <- NULL # initialise object containing different classnames per attribute
  n_supp_data_qual_classes <- NULL # initialise object containing number of classes per attribute
  for (i in supp_qual) {
    tmp <- readRAST6(i)
    tmp2 <- raster(tmp)
    qual_rast <- stack(tmp2, qual_rast)
    supp_data_classnames[[i]] <- raster::unique(tmp2)
    n_supp_data_qual_classes <- c(n_supp_data_qual_classes, length(raster::unique(tmp2)))
  }
  
  # convert (at) symbol to point (in case input comes from another GRASS mapset; readRAST6() convert it to point implicitly which causes errors during later processing)
  supp_qual <- gsub("@", ".", supp_qual)
  
  names(n_supp_data_qual_classes) <- supp_qual
  
  
  
  # load quantitative supplemental data
  quant_rast <- NULL # initialise object containing all quantitative raster layers
  for (i in supp_quant) {
    tmp <- readRAST6(i)
    tmp2 <- raster(tmp)
    quant_rast <- stack(tmp2, quant_rast)
  }
  
  # convert (at) symbol to point (in case input comes from another GRASS mapset; readRAST6() convert it to point implicitly which causes errors during later processing)
  supp_quant <- gsub("@", ".", supp_quant)
  
  rm(list=c("tmp","tmp2"))
  
  
  # compare Rasters for extent, no. of rows and cols, CRS, resolution and origin
  if (!is.null(qual_rast) & !is.null(quant_rast)) {
    comp_val <- compareRaster(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                              qual_rast, quant_rast, res=T, orig=T)
  } else if(is.null(qual_rast) & !is.null(quant_rast)) {
    comp_val <- compareRaster(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                              quant_rast, res=T, orig=T)
  } else if(!is.null(qual_rast) & is.null(quant_rast)) {
    comp_val <- compareRaster(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                              qual_rast, res=T, orig=T)
  } else if(is.null(qual_rast) & is.null(quant_rast)) {
    comp_val <- compareRaster(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
                              res=T, orig=T)
  }
  
  
  if (comp_val) {
    message("All input data loaded and checked for extent, CRS and resolution successfully.")
    message("")
    if(plot_catena) {
      message(paste0("Plots will be produced and written to '", dir_out, "/plots_area2catena/'."))
      message("")
    }
  } else {
    stop("ERROR: Loaded input rasters are not of the same extent, no. of rows/cols, CRS, resolution and/or origin!")
  }
  
  
  
  # PROCESS EHAs #
  # get resolution of spatial data
  xres <- xres(eha_rast)
  yres <- yres(eha_rast)
  
  
  # identify EHAs
  eha_ids <- raster::unique(eha_rast)
  if (!is.null(eha_subset)) eha_ids <- eha_subset
  
  # LOOP over EHAs
  message(paste("Processing ", length(eha_ids), " EHAs. This might take a while depending on catchment size and number of cpu cores.", sep=""))
  if (!is.null(eha_subset)) message("Note that this is just a subset as specified in the input 'eha_subset'.")
  message("")
  
  id <- NULL # to remove "R CMD CHECK ..." Note of "no visible binding for global variable 'id'"
  #   for (curr_id in eha_ids) {
  if(ncores==1)
  {  
  #serial call using simple foreach
  logdata <- foreach (id = 1:length(eha_ids), .combine=rbind, .errorhandling='remove') %do% {
    eha_calc(id, eha_ids, eha_rast, flowaccum_rast, dist2river_rast, relelev_rast, supp_quant, supp_qual,
             n_supp_data_qual_classes, quant_rast, qual_rast, supp_data_classnames,
             min_cell_in_slope, max_riv_dist, plot_catena, ridge_thresh, min_catena_length,
             xres,dir_out)
  }
  }
    else
  #parallel call using dopar  
  logdata <- foreach (id = 1:length(eha_ids), .combine=rbind, .errorhandling='remove', .options.multicore=list(silent=FALSE)) %dopar% {
    eha_calc(id, eha_ids, eha_rast, flowaccum_rast, dist2river_rast, relelev_rast, supp_quant, supp_qual,
             n_supp_data_qual_classes, quant_rast, qual_rast, supp_data_classnames,
             min_cell_in_slope, max_riv_dist, plot_catena, ridge_thresh, min_catena_length,
             xres,dir_out)
  }
  
  message("")
  message("Looping over EHAs completed.")
  message("")
  
  
  # check if anything was produced (if NULL an unexpected error might have occured)
  if(is.null(logdata))
    stop("An unexpected error occured while processing EHAs. Please contact the author.")
  
  
  # sort out erroneous values
  out_dat <- logdata[which(logdata$error == 0 | logdata$error == 5), -length(logdata)]
  
  # check for NAs
  if(any(is.na(out_dat))) {
    save(out_dat, file=paste(dir_out,"rstats_error.Rdat",sep="/"))
    stop(paste("There are NA values in the output which have to be removed for function 'prof_class'! 
         This might be caused by overlapping NA and non-NA values in the input raster files.
         Look at saved R data file ", dir_out,"/rstats_error.Rdat which variable produced NA values.
         Columns are: ID, profile number, elevation, supplemental data (one column per variable and class) and slope width.",sep=""))
  }
  
  # write output
  #out_pre <- mapply(out_dat[,c(3:length(out_dat))], FUN=function(x) formatC(x, format="f", digits=3))
  #out_fmt <- cbind(out_dat[,c(1,2)], out_pre)
  write.table(out_dat, paste(dir_out,catena_out, sep="/"), col.names=F, row.names=F, quote=F, sep="\t")
  
  
  # WRITE OUTPUT #
  # write header file
  write("#This file works as a header to the output of area2catena. Don't add additional headerlines.",
        file=paste(dir_out,catena_head_out, sep="/"), append=F)
  write('#1. line after header: description/field names of the data columns contained in the file',
        file=paste(dir_out,catena_head_out, sep="/"), append=T)
  write('#2. line after header: specifies, how many columns of data belong to the respective data-field given in line 1',
        file=paste(dir_out,catena_head_out, sep="/"), append=T)
  write('#3. line after header: number of classes/ weighting factors in classification process (see LUMPuserman*.rtf for details)',
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
  if (grass_files) {
    for (i in supp_qual) {
      write(c("new_id", "original_id"),
            file=paste(dir_out, "/reclass_", i, ".txt", sep=""), ncolumns=2, append=F, sep="\t")
      for (n in 1:n_supp_data_qual_classes[i]) {
        write(c(n, supp_data_classnames[[i]][n]),
              file=paste(dir_out, "/reclass_", i, ".txt", sep=""), ncolumns=2, append=T, sep="\t")
      }
    }
  }
  
  
  # FINAL REPORT #
  message('')
  message('All outputs produced.')
  message('')
  message(paste(length(eha_ids)-length(which(logdata$error == 2))-length(which(logdata$error == 1)), ' slopes treated', sep=""))
  message(paste(length(which(logdata$error == 1)), ' slopes skipped that have less than ', min_cell_in_slope, ' cells', sep=""))
  message(paste(length(which(logdata$error == 2)), ' slopes skipped that are not adjacent to river', sep=""))
  message(paste(length(which(logdata$error == 4)), ' slopes skipped that have a mean length shorter than ', min_catena_length, sep=""))
  message(paste(length(which(logdata$error == 3)), ' slopes skipped that have only cells with dist2river=0.', sep=""))
  message(paste(length(which(logdata$error == 5)), ' warnings due to slopes with no flow_accum less than ', ridge_thresh, sep=""))
  message('')
  message("DONE!")
  
} # EOF




### Internal Function processing EHA ###


eha_calc <- function(id, eha_ids, eha_rast, flowaccum_rast, dist2river_rast, relelev_rast, supp_quant, supp_qual,
                     n_supp_data_qual_classes, quant_rast, qual_rast, supp_data_classnames,
                     min_cell_in_slope, max_riv_dist, plot_catena, ridge_thresh, min_catena_length,
                     xres,dir_out) {
  
  errcode <- 0
  curr_id <- eha_ids[id]
  
  # EHA: CHECKS and PREPARATIONS #    
  # determine cell indices of curr_id
  curr_cells <- which(eha_rast@data@values == curr_id)
  
  # determine number of cells in eha and skip processing if less than min_cell_in_slope
  # ERROR CODE 1
  if (length(curr_cells) < min_cell_in_slope) {
    message(paste('EHA ', curr_id, ' skipped because of low number of cells (', length(curr_cells), ')', sep=""))
    return(data.frame(output=t(rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4)), error=1))
    stop() # use stop instead of next in foreach loop and define '.errorhandling' #? is this necessary?
  }
  
  # extract values out of raster objects into ordinary vectors to save time (internal calls to raster objects take time)
  flowaccum_vals <- flowaccum_rast[curr_cells]
  dist2river_vals <- as.numeric(format(dist2river_rast[curr_cells],digits=3))
  relelev_vals <- relelev_rast[curr_cells]
  
  # determine closest distance to river and skip processing if more than max_riv_dist
  # ERROR CODE 2
  if(min(dist2river_vals) > max_riv_dist) {
    message(paste(curr_id, ' skipped, not adjacent to channel / farther than ', max_riv_dist, ' cells (',min(dist2river_vals),')', sep=""))
    return(data.frame(output=t(rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4)), error=2))
    stop() # use stop istead of next in foreach loop and define '.errorhandling'
  } else {
    dist2river_vals <- dist2river_vals - min(dist2river_vals)
  }
  
  # test whether only river cells available (all dist2river = 0) and skip eha_clumped if so
  # ERROR CODE 3
  if(max(dist2river_vals) == 0) {
    message(paste(curr_id, ' skipped because for all cells dist2river=0.', sep=""))
    return(data.frame(output=t(rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4)), error=3))
    stop() # use stop istead of next in foreach loop and define '.errorhandling'
  }
  
  
  # EHA: compute MEAN CATENA LENGTH #
  # find all cells that are the beginning of a flowpath
  curr_entries <- which(flowaccum_vals<=ridge_thresh)
  if (!any(curr_entries)) {  # no cells found, strange, but try to fix this problem
    warning(paste('EHA ', curr_id, ' has no cells with flowpath start (below ', ridge_thresh, ')', sep=""))
    # reduce threshold to minimum flow accumulation found in current slope
    curr_entries <- which(flowaccum_vals <= min(flowaccum_vals))
    errcode <- 5
  }
  
  # convert indices from "relative to curr_cells" to "relative to dist2river"
  curr_entries <- curr_cells[curr_entries]
  
  # compute mean catena length (calclength-method, Chochrane & Flanagan, 2003) #
  mean_length <- sum(dist2river_rast[curr_entries]^2)/sum(dist2river_rast[curr_entries])
  
  # skip very short catenas
  # ERROR CODE 4
  if (mean_length < min_catena_length) {
    message(paste(curr_id, ' skipped because of low length (', mean_length, ')', sep=""))
    return(data.frame(output=t(rep(NA, sum(n_supp_data_qual_classes) + length(supp_quant) + 4)), error=4))
    stop() # use stop istead of next in foreach loop and define '.errorhandling'
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
  
  
  # EHA: calculate properties (loop over reference points) #
  # resolution, number of data points describing the mean catena
  res <- ceiling(mean_length)
  
  # minimum number of cells necessary for averaging one point of the mean catena
  min_no_cells <- 1
  
  # initialisations
  out_x <- NULL # initialise vector of point numbers for current EHA
  out_y <- NULL # initialise vector of average rel. elevation for current point in EHA
  supp_attrib_mean <- matrix(NA, nrow=length(supp_quant)+sum(n_supp_data_qual_classes), ncol=res+1) # init. matrix of mean supplemental information
  density <- NULL # initialise vector for density values
  entry_missing <- 0 # flag indicating that the value for the previous point in the mean catena could not be computed
  out_combined <- NULL # combined output of one catena    
  
  for (j in 0:res) {
    # point number in catena output
    out_x[j+1] <- j
    
    # initial search range used for averaging the current point of the mean catena
    search_range <- 0.5
    
    # seek all entries within the "spacing" range that are averaged to one point in the output catena
    while (1) {
      curr_entries <- which(dist2river_vals>=(j-search_range) & dist2river_vals<(j+search_range))
      
      # convert indices from "relative to curr_cells" to "relative to dist2river"
      curr_entries <- curr_cells[curr_entries]
      
      # check, if enough cells have been found to be used for averageing this point of the catena
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
      flowaccum_sqrt <- sqrt(flowaccum_rast[curr_entries])
      
      # averaging of elevations of all points
      # mean weighted with square root of flowaccumulation (~flow path density)
      out_y[j+1] <- sum(relelev_rast[curr_entries]*flowaccum_sqrt)/sum(flowaccum_sqrt)
      
      # compute average quantitative supplemental data
      if (!is.null(quant_rast)) {
        for (k in 1:length(supp_quant)) {
          supp_attrib_mean[k,j+1] <- sum(quant_rast[curr_entries][,k]*flowaccum_sqrt)/sum(flowaccum_sqrt) 
        }
        
      }
      
      # for summing up the number of classes needed by the successive qualitative attributes
      col_counter <- 0
      # to ensure that the qualitative attributes are inserted after the quantitative ones
      quant_columns <- length(supp_quant)
      
      # compute average qualitative supplemental data
      if (!is.null(qual_rast)) {
        for (k in supp_qual) {
          # select attribute values
          curr_att_val <- qual_rast[curr_entries][,k]
          # do averaging for each class of each attribute separately
          for (kk in 1:n_supp_data_qual_classes[k]) {
            supp_attrib_mean[quant_columns+kk+col_counter,j+1] <- sum((curr_att_val==supp_data_classnames[[k]][kk])*flowaccum_sqrt)/sum(flowaccum_sqrt) 
          }
          col_counter <- col_counter+n_supp_data_qual_classes[k]
        } 
        
      }
      
      if (entry_missing) {
        # use the values of the current point to assign the previous point, which had problems in the calculation 
        supp_attrib_mean[k,j] <- supp_attrib_mean[k,j+1]
        entry_missing <- 0
      }
      
      # if no curr_entries
    } else {
      message(paste('problems calculating mean catena for ', curr_id, sep=""))
      
      # flag indicating that the value for the previous point in the mean catena could not be computed
      if (j!=0) entry_missing <- 1
    } # end if curr_entries
    
    
    # OUTPUT #
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
  
} # EOF
