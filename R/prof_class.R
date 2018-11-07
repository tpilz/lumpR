# lumpR/prof_class.R
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


#' Classification of mean catenas
#' 
#' Classifies mean catenas derived from \code{\link[lumpR]{area2catena}} into \emph{Landscape
#' Units} and \emph{Terrain Components}.
#' 
#' @param catena_file Name of file containing mean catena information derived from
#'      \code{\link[lumpR]{area2catena}}.
#' @param catena_head_file Name of file containing meta-information for classification
#'      derived from \code{\link[lumpR]{area2catena}} and adjusted manually (see Notes). 
#'      Superseded by \code{attribute_file}, may be deprecated in future versions.
#' @param attribute_file Path to attribute file (see Notes). Supersedes and overrides \code{catena_head_file}.
#' @param svc_column Field name in \code{catena_head_file} that holds the information
#'      of SVCs for generating \code{tccontainssvcoutfile}. Default: 'svc'.
#' @param dir_out Character string specifying output directory (will be created;
#'      nothing will be overwritten).
#' @param luoutfile Output: Name of file containing the average properties of
#'      \emph{Landscape Units}.
#' @param tcoutfile Output: Name of file containing the average properties of
#'      \emph{Terrain Components}.
#' @param lucontainstcoutfile Output: Name of file containing information wich
#'      LU contains which TCs.
#' @param tccontainssvcoutfile Output: Name of file containing information wich
#'      TC contains which SVCs.
#' @param terraincomponentsoutfile Output: Name of file containing general
#'      properties of TCs.
#' @param recl_lu Output: Name of GRASS reclassification file: EHA -> LU.
#' @param saved_clusters Output: Name of R file that can be used to store LU
#'      characteristics for later re-use; set to NULL to omit output (default).
#' @param classify_type Type of classification:\cr
#'      ' ': (default) unsupervised classification, no \code{saved_clusters} will be produced\cr
#'      'save': do unsupervised classification and save cluster centers to \code{saved_clusters}
#'              for future supervised classification\cr
#'      'load': load cluster centers from existing file and classify according
#'              to these clusters (e.g. supervised classification). CURRENTLY NOT SUPPORTED!
#' @param seed Integer specifying seed for random processes in cluster analysis.
#' @param resolution Integer specifying resolution of profiles/spacing of samples.
#'      Typically the resolution of your GRASS location used for \code{\link[lumpR]{area2catena}}.
#' @param max_com_length Integer defining the maximum common length of profiles,
#'      i.e. the maximum number of support points representing each catena during the
#'      classification procedure. Too large values consume more memory and computational
#'      effort.
#' @param com_length Integer giving common length of profiles, i.e. the number of
#'      support points representing each catena during the classification procedure.
#'      Too large values consume more memory and computational effort. Overwrites
#'      max_com_length.
#' @param make_plots logical; visualisation of classification results written into
#'      sub-directory \emph{plots_prof_class}. WARNING: Consumes a lot of processing
#'      time and memory. Default: \code{FALSE}.
#' @param eha_subset NULL or integer vector with subset of EHA ids that shall
#'      be processed (for debugging and testing).
#' @param eha_blacklist NULL or integer vector with subset of EHA ids that will
#'      be excluded (use this for manual exclusion of strange profiles).
#' @param overwrite \code{logical}. Shall output of previous calls of this function be
#'      deleted? If \code{FALSE} the function returns an error if output already exists.
#'      Default: \code{FALSE}.
#' @param silent \code{logical}. Shall the function be silent (also suppressing warnings)?
#'      Default: \code{FALSE}.
#' @param plot_silhouette \code{logical}. Shall a silhouette plot (illustrating the clustering
#'      process) be generated? Consumes much memory and processing time and should be disabled,
#'      if a memory error is thrown. Will be \code{FALSE} if \code{make_plots = FALSE}.
#'      Default: \code{TRUE}.
#'      
#' @return Function returns nothing. Output files are written into output directory
#'      as specified in arguments.
#' 
#' @note Function uses output of \code{\link[lumpR]{area2catena}}. However, no GRASS
#'      session needs to be started in this case.
#'      
#'      After applying \code{recl_lu}, the resulting landscape units raster map in your GRASS
#'      location might show gaps depending on the number of generated landscape units
#'      as each landscape unit refers to the representative EHA. The gaps can be filled
#'      with GRASS function \code{r.grow}.
#'      
#'      In case of \bold{long computation times or memory issues}, try \code{make_plots = FALSE}
#'      and specify an RData file as \code{catena_file} (already in \code{\link[lumpR]{area2catena}}).
#'      
#' @details This function first resamples the catenas derived from \code{\link[lumpR]{area2catena}}
#'      to a common length (\code{com_length} or the median number of support points
#'      of all catenas but not more than \code{max_com_length}). Second, k-means clustering
#'      is employed to group the catenas into representative \emph{Landscape Units}
#'      according to parameters given via \code{catena_head_file} taking into account
#'      hillslope length, shape, and supplemental properties. Third, each group is further
#'      partitioned into a given number of \emph{Terrain Components} in a way that the
#'      variance within each TC is minimized considering slope gradient and supplemental
#'      information.
#'      
#'      The classification and partitioning can be customized via the files passed to the function: \code{attribute_file} (recommended) or \code{catena_head_file} (deprecated).
#'      Structure of \code{attribute_file}:\cr
#'      Tab-limited text table with the following fields in the header (Fields with * are mandatory):\cr
#'      \describe{
#'      \item{attribute*}{[String] name of attribute to be used.}
#'      \item{type*}{[String] 'qualitative': categorical data (e.g. soil type); 'quantitative': numerical value (e.g. slope); 'ignored': not to be used in classification}
#'      \item{group}{[String]: combine attributes to groups that are considered together in the classification ('multifield-attributes'). Empty strings denote no grouping (default). The fields *weight_4tc*, *n_classes_4lu*, *is_spatial* and *type* must be homogenous within a group.}
#'      \item{group_weight}{[numeric] relative weight within the group (if any). Default: 1.}
#'      \item{is_spatial}{[logical] flag indicating if an attribute is distributed along the profile (e.g. soil type) or used as a single value for the entire profile (e.g. profile length)}
#'      \item{n_classes_4lu*}{[integer] Number of classes to create from this attribute or group.}
#'      \item{weight_4tc*}{[weight] weighting factor to be used for this attribute in partitioning. The value entered for attribute \code{id} is interpreted as the number of TCs to create.}
#'      \item{n_datacolumns}{[integer] Number of data columns used for this attribute. Should be 1 for qualitative attributes, or <number of categories> for quantitative attributes. For attributes automatically generated by \code{prof_class} (i.e. \code{x_extent, z_extent}), this value is 0.}
#'      }
#'      
#'      Each row contains one attribute to be used in the classification. For \code{prof_class}, at least the attributes
#'      \code{id}, \code{shape}, \code{x_extent} and \code{z_extent} must be present. The directory /example within the package directory contains
#'      an example file \code{attributes.txt.mod}.
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

prof_class <- function(
  
  ### INPUT ###
  catena_file=NULL,
  catena_head_file=NULL,
  attribute_file=NULL,
  svc_column='svc',
  
  ### OUTPUT ###
  dir_out="./",
  luoutfile="lu.dat",
  tcoutfile="tc.dat",
  lucontainstcoutfile="lucontainstc.dat",
  tccontainssvcoutfile="r_tc_contains_svc.dat",
  terraincomponentsoutfile="terraincomponents.dat",
  recl_lu="reclass_lu.txt",
  saved_clusters=NULL,
  
  ### PARAMETERS ###
  seed=1312,
  resolution=NULL,
  classify_type=' ',
  max_com_length=NULL,
  com_length=NULL,
  make_plots=F,
  eha_subset=NULL,
  eha_blacklist=NULL,
  overwrite=F,
  silent=F,
  plot_silhouette=T
) {
  
### PREPROCESSING ###----------------------------------------------------------
  
  if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  if(!silent) message("% START prof_class()")
  if(!silent) message("%")
  if(!silent) message("% Initialise function...")

# checks #---------------------------------------------------------------------
  
  # check output directory
  if (!overwrite & ( file.exists(paste(dir_out,luoutfile,sep="/")) | 
                     file.exists(paste(dir_out,tcoutfile,sep="/")) |
                     file.exists(paste(dir_out,lucontainstcoutfile,sep="/")) |
                     file.exists(paste(dir_out,tccontainssvcoutfile,sep="/")) |
                     file.exists(paste(dir_out,terraincomponentsoutfile,sep="/")) |
                     file.exists(paste(dir_out,recl_lu,sep="/")) ) ) 
    stop(paste0("In output directory '", dir_out, "' some or all output file(s) already exist!"))
  
  if (make_plots) {
    if(length(dir(paste(dir_out, "plots_prof_class", sep="/"))) != 0)
    {
      if(overwrite)
        file.remove(dir(paste(dir_out, "plots_prof_class", sep="/"), full.names = T))
      else
        stop(paste0("Output directory for plots '", dir_out, "/plots_prof_class/' is not empty!"))
    }  
    dir.create(paste(dir_out, "plots_prof_class", sep="/"), recursive=T,  showWarnings = F)
  }
  
  # argument checks
  if(is.null(catena_file) || !file.exists(catena_file))
    stop("'catena_file' has not been specified or does not exist!")
  if(is.null(catena_head_file) || !file.exists(catena_head_file))
    if (is.null(attribute_file))
      stop("'catena_head_file' has not been specified or does not exist!")
  if(!is.numeric(resolution))
    stop("Resolution of the raster used to produce 'catena_file' and 'catena_head_out' needs to be given (as numeric)!")
  if(is.null(max_com_length) & is.null(com_length))
    stop("Either parameter 'max_com_length' or 'com_length' has to be given!")
  if(!grepl("^[[:blank:]]{1}|save",classify_type))
    stop("'classify_type' must be ' ' or 'save' ('load' is currently not supported).")
  if(any(classify_type == "save") & is.null(saved_clusters))
    stop("Parameter 'saved_clusters' must be given when 'classify_type'='save'!")
  
  
  if (!is.null(attribute_file)) #attribute description file specified?
  {
    if(!file.exists(attribute_file))
      stop(past0("<attribute_file> ",attribute_file, "not found."))
    
    if (!is.null(catena_head_file) )
      warning("<attribute_file> specified, ignoring <catena_head_file>.")
    
    attribute_table=read.table(attribute_file, sep="\t", header=TRUE, stringsAsFactors = FALSE)
    attribute_table = check_attr_table(attribute_table) #check validity of /correct attribute table
    attribute_table$group_weight [is.na(attribute_table$group_weight) ]=1 #set unset weights to 1
    attribute_table$n_classes_4lu[is.na(attribute_table$n_classes_4lu)]=1 #set unset number of classes to 1
    attribute_table$weight_4tc   [is.na(attribute_table$weight_4tc)   ]=0 #set unset weighting in TCs to 0
  } else attribute_table=NULL
  
  
  
  # supress warnings in silent mode
  if(silent){
    tmp_file2 <- file(tempfile(), open="wt")
    sink(tmp_file2, type="message")
    oldw <- getOption("warn")
    options(warn = -1)
  }
  
  if(!silent) message("% OK")
  

  
### CALCULATIONS ###-----------------------------------------------------------
#  tryCatch({
    
    if(!silent) message("%")
    if(!silent) message("% Load and prepare data...")
    
# import and prepare data #----------------------------------------------------
    
    # output dir
    dir.create(dir_out, recursive=T, showWarnings=F)
    
    # horizontal resolution of profiles/spacing of samples
    if (!(is.numeric(resolution) && is.finite(resolution) && (resolution>0)))
      stop("Argument 'resolution must be a positive number.")
    dx <- resolution
    
    # separator in outfiles
    tab <- "\t"
    
    
    # LOAD INPUT #
    # load stats header
    if (is.null(attribute_table))
    { #use rstats_head.txt
      headerdat <- as.matrix(read.table(catena_head_file, header=T))
      
      attribute_table=data.frame(attribute=colnames(headerdat), stringsAsFactors = FALSE)
      
      # specification of number of columns used by each attribute
      attribute_table$n_datacolumns <- headerdat[1,]
      # relative weight of each attribute (supplemental data) to be used in classification
      attribute_table$n_classes_4lu <-  as.numeric(headerdat[2,])
      attribute_table$group_weight=1
      attribute_table$group_weight[3]  = attribute_table$n_classes_4lu[3] #special case: the weighting factor for x/z
      attribute_table$n_classes_4lu[3] = attribute_table$n_classes_4lu[2] #this is the same group ("extent")
      # relative weight of each attribute (supplemental data) to be used in partition  (terrain component decomposition)
      attribute_table$weight_4tc <- as.numeric(headerdat[3,])
      # store the names of the attributes
      attribute_table$attribute <- colnames(headerdat)
      rm(headerdat)

      # number of TCs to be created in each LU
      ntc <- attribute_table$weight_4tc[1]
      
      #convergence toward single implementation
        #which attributes contain spatial information, which contain only  a single value for the entire profile?
        is_spatial = rep(TRUE, length(attribute_table$attribute))
        is_spatial[2:3]= FALSE  #only x and y extent are not spatial attributes
      
      attribute_table=data.frame(attribute=attribute_table$attribute, group="", group_weight=attribute_table$group_weight, is_spatial=as.integer(is_spatial), n_classes_4lu=attribute_table$n_classes_4lu,
                                 weight_4tc=attribute_table$weight_4tc, n_datacolumns=attribute_table$n_datacolumns, stringsAsFactors = FALSE)
      attribute_table$group        [2:3]="extent" #x and y extent are treated within one group
      attribute_table$n_datacolumns[2:3]=0        #they are not contained in the input file, but will be computed later
      attribute_table$attribute =  gsub(x = attribute_table$attribute, pattern = "^id$",   replacement = "shape")
      attribute_table$attribute =  gsub(x = attribute_table$attribute, pattern = "p_no",   replacement = "x_extent")
      attribute_table$attribute =  gsub(x = attribute_table$attribute, pattern = "elevation",   replacement = "z_extent")
      
      save(list = ls(), file="head.RData")
    }   else
    { #use attribute_table
      check_attr_table(attribute_table, manatory_attribs = c("id", "shape", "x_extent", "z_extent"))

      #FIXME: we cannot do this here, since this is the order that is in the rstats-file!! rather sort in the output
      #order by group
      #attribute_table = attribute_table[order(attribute_table$group, 1:nrow(attribute_table)), ] 
      
      #bring "shape", "x_extent", "z_extent" to top fixme: rather sort in the output
      new_order = match(c("shape", "x_extent", "z_extent"), attribute_table$attribute)
      new_order = c(new_order, which(! (attribute_table$attribute %in% c("shape", "x_extent", "z_extent"))))
      attribute_table = attribute_table[new_order, ] 
      
      nclasses        =attribute_table$n_classes_4lu[attribute_table$attribute=="shape"]
      ntc             =attribute_table$weight_4tc   [attribute_table$attribute=="id"]
      
      attribute_table=attribute_table[!attribute_table$attribute %in% c("id", "x_coord"), ] #remove rows (these are not treated)
      
      #which attributes contain spatial information, which contain only a single value for the entire profile?
      attribute_table$is_spatial = as.logical(attribute_table$is_spatial)
      
      # specification of number of columns used by each attribute
      attribute_table$n_datacolumns <- as.integer(attribute_table$n_datacolumns)
      
      # number of classes of each attribute (supplemental data) to be used in classification
      #legacy
        attribute_table$n_classes_4lu [1] = -abs(nclasses) 

      save(list = ls(), file="attr.RData")
    }
    
    ungrouped_atttribs = is.na(attribute_table$group) | (attribute_table$group=="") #these are the attributes to be treated separately
    attribute_table$group[ungrouped_atttribs] = attribute_table$attrib[ungrouped_atttribs] #set group names to the attribute names 8later, we will iterate over these)
        
    if (ntc < 1) {
      message(paste('% -> WARNING: number of TCs will be set to 2 instead of ', ntc, ' as specified in catena_head_file', sep=""))
      ntc <- 2
    }

    # determine type of classification from catena_head_file
    if (attribute_table$n_classes_4lu[1] <= 0) {
      cf_mode <- 'successive' # classification performed to specified number of classes for each attribute (option 2)
      attribute_table$n_classes_4lu <- abs(attribute_table$n_classes_4lu)
    } else {
      cf_mode <- 'singlerun' # classification performed in single run for all classes using the specified weighting factors (option 1)
      warning("cf_mode='singlerun' is experimental. Please consider providing argument 'attribute_table' and grouping all attributes.")
    }
 
    
    # if supervised classification using saved clusters is to be used
    #   com_length <- -1
    
    # load standard catena data
    if(grepl(".RData$", catena_file)) {
      load(catena_file)
      stats <- logdata[,1,drop=F]
    } else {
      stats <- scan(catena_file, nlines = 1, what=numeric(), sep = "\t", quiet = TRUE) #read first line only
      stats <- read.table(file = catena_file, colClasses = c("numeric", rep("NULL", length(stats)-1)), sep = "\t") #read first column only
    }
    
    profpoints <- table(stats[,1])  #count number of points of each catena
    p_id_unique = unique(stats[,1]) #get unique IDs  
    rm(stats)
    

    if (!is.null(eha_subset)) 
    {
      if(!silent) message("% -> WARNING: Using only a subset as specified in the argument 'eha_subset'.")
      p_id_unique = intersect(p_id_unique, eha_subset)
      if (length(p_id_unique)==0)
        stop("Specified 'eha_subset' not found.")
    } 
    
    if (!is.null(eha_blacklist)) 
    {
      if(!silent) message("% -> WARNING: Excluding a subset as specified in the argument 'eha_blacklist'.")
      p_id_unique = setdiff(p_id_unique, eha_blacklist)
      if (length(p_id_unique)==0)
        stop("No profiles remaining after exclusion. Check 'eha_blacklist'")
    } 
    
    n_profs = length(p_id_unique)
    
    
    
    # use the median of sampling points as the desired common length of profiles
    if (classify_type != 'load') {  
      if (is.null(com_length)) #set com_length, if not specified from outside
      { 
        # common number of points for re-sampled hillslopes
        com_length <- max(ntc, round(median(profpoints))) #use at least as many sampling points as requested TCs
        com_length <- min(com_length, max_com_length)     
      }  
    }     # otherwise, the resolution from the saved clusters is used
    
    #browser()
    suppl_cols =!(attribute_table$attribute %in% c("shape", "x_extent", "z_extent"))
    n_suppl_attributes = sum(suppl_cols) #number of supplemental attributes
    n_supp_data_columns <- sum(attribute_table$n_datacolumns[suppl_cols]) #number of respective columns
    
    # allocate new matrix for storing resampled profiles (hillslopes)
    # for each profile (rows) com_length elevation points, the profile length, the
    # profile height, and supplemental data is stored in one long vector
    profs_resampled_stored <- matrix(NA, nrow=n_profs, ncol=com_length+2+com_length*n_supp_data_columns)
    
    # PLOT original profile
    if (make_plots) {
      pdf(paste(dir_out, "plots_prof_class/plots_prof_class.pdf", sep="/"))
      plot(1,1,type="n", xlim=c(0,max(profpoints)*resolution), ylim=c(0,500),
           main="Original catenas", xlab="horizontal length [m]", ylab="elevation [m]")
    }
  #read and resample profiles (done at the same time to avoid duplicates in memory)
    # TODO: This mess needs to be improved!
     if(!grepl(".RData$", catena_file)) testcon <- file(catena_file,open="r")
     if(!silent) #for printing progress indicator
       pb <- txtProgressBar(min = 0, max = length(p_id_unique), style = 3)

     i=0 #counter for valid profiles read
     total_read = 0 #counter for total profiles read
     while (i < length(p_id_unique))
     {
       i=i+1
       if (!silent) #next progress message
         setTxtProgressBar(pb, i)
       
       cur_p_id = p_id_unique[i] #id of profile to be loaded
       p_pos = which(names(profpoints)==cur_p_id) #position of current profile in list
       
       skiplines = sum(profpoints[(total_read+1) : p_pos]) -  profpoints[p_pos] #compute number of lines to skip to reach the next selected profile
       if(grepl(".RData$", catena_file)) {
         tt <- as.matrix(logdata[which(logdata[,1] == cur_p_id),])
       } else {
        tt = readLines(con = testcon, n = skiplines)
        tt = scan(file=testcon, what=numeric(), nlines = profpoints[p_pos], quiet = TRUE) #read single profile
        tt = matrix(tt, nrow = c(profpoints[p_pos]), byrow = TRUE) #reshape matrix
       }
        
       total_read = p_pos  #we are now just at the desired profile
       
       if (any(tt[,1]!= cur_p_id)) stop(paste0("Format error in ",catena_file))
       if (make_plots) 
         lines(tt[,2]*resolution, tt[,3])
       
       
       if (profpoints[p_pos] < 2) { #catena too short, ignore
         if(!silent) message(paste('% -> WARNING: profile ', paste(cur_p_id, collapse=", "), ' contains only one point. Skipped.', sep=""))
         profs_resampled_stored[i,] = NA
       } 
       
       
         
       p_resampled <- apply(tt[,-(1:2)], MARGIN = 2, FUN=function(y) approx(x = tt[,2]-1, y, xout = 0:(com_length-1)/(com_length-1)*(profpoints[p_pos]-1))$y)

       # set foot of profile to zero elevation
       p_resampled[,1] = p_resampled[,1] - p_resampled[1,1]
       
       # amplitude of profile will be scaled to 1
       d <- max(p_resampled[,1]) 
       if(d==0) {
         if(!silent) message(paste('% -> WARNING: Profile ', cur_p_id, ' has no elevation gain (runs flat)', sep=""))
         p_resampled[,1] <- 0
       } else {
         # normalize profile in elevation (ie. normally, top end at elevation = 1)
         p_resampled[,1] <- p_resampled[,1] / d
       }
       
       prof_length = (profpoints[p_pos]-1) * resolution #compute original length of catena
       prof_height = max(tt[,3]) - tt[1,3] #compute original elevation gain of catena
       
       # append the dimension components, unweighted
       profs_resampled_stored[i,1:(com_length+2)] <- c(p_resampled[,1], prof_length, prof_height)
       attribute_table$n_datacolumns[attribute_table$attribute=="x_extent"] = 1  #record that there are these new attributes now
       attribute_table$n_datacolumns[attribute_table$attribute=="z_extent"] = 1
       
       
       
       # treat supp_data if present (resample, weigh and add to profile vector to be included in cluster analysis)
       if (n_suppl_attributes>0) 
       {
         #check supplemental data for this profile
         p_supp <- tt[-(1:3),]
         all_na <- apply(p_supp, 2, function(x) all(is.na(x)))
         if (any(all_na))
         {  
           if (make_plots)
             dev.off() #close PDF output
           #browser()
           na_attributes = names(attribute_table$n_datacolumns[-(1:3)])[unique(sapply (X = which(all_na), function(x) min(which(cumsum(attribute_table$n_datacolumns[-(1:3)]) >= x))))] #names of attributes with all NAs
           stop(paste0("Error: EHA ", cur_p_id," has only NAs for attribute(s) ", paste0(na_attributes, collapse=", "),". Most likely a result of insufficient map coverage. Fix coverage, remove this attribute or replace NAs manually."))
         }
         profs_resampled_stored[i,(com_length+3):(com_length+2+com_length*n_supp_data_columns)] <- as.vector(p_resampled[,-1])
       }     
     }
     
     if(!silent) # close progress bar
       close(pb)
     
     if(grepl(".RData$", catena_file)) {
       rm(logdata)
     } else {
       close(testcon)
     }
     # remove not needed objects to save memory
     rm(list = c("p_supp", "p_resampled", "tt"))
     gc(verbose = F);gc(verbose = F)
     
     
     
    # PREPARE attribute loop and key-generation #

    # START OF CLASS KEY GENERATION #

    # successive weighting vs. single run: set number of times the classification loop has to be run
    if (cf_mode == 'successive') {
      iw_max <- length(unique(attribute_table$group)) # successive weighting for each single attribute
    } else {
      iw_max <- 1  # cf_mode ='singlerun'
    }
    

    # PLOT modified (resampled) profiles
    if (make_plots) {
      plot(1,1,type="n", xlim=c(0,com_length-1), ylim=c(0,1),
           main="catenas, resampled, reduced & normalized", xlab="catena point number", ylab="relative elevation")
      for (i in 1:nrow(profs_resampled_stored)) {
        lines(0:(com_length-1), profs_resampled_stored[i,1:com_length])
      }
    }
    
  
    if(!silent) message(paste0("% OK, ",length(p_id_unique)," profiles loaded."))
    
    
    #generate column indices for all attributes
    column_indices = array(FALSE, c(length(attribute_table$attribute), ncol(profs_resampled_stored)))
    offset=0
    for (attr_i in 1:length(attribute_table$attribute)) 
    {
      start_col = offset + 1    #index of start column of this attribute
      #browser()
      end_col   = offset + attribute_table$n_datacolumns[attr_i] * ifelse (attribute_table$is_spatial[attr_i], com_length, 1) #index of end column of this attribute
      column_indices[attr_i, start_col:end_col] = TRUE
      offset = offset + (end_col-start_col)+1 #increase offset
    }  
    
    
# classification of ehas (clustering) #----------------------------------------
    if(!silent) message("%")
    if(!silent) message("% Clustering of EHAs...")
    
    cidx_save <- list(NULL) # initialise list to store classification results; i.e. a vector assigning each EHA to a cluster class for each attribute
    hc <- 0
    for (attr_group in unique(attribute_table$group))
      {
      # ensure reproducible random numbers for debugging / repeatitions
      set.seed(seed)
            current_attribs = which (attr_group == attribute_table$group)
      
            iw = current_attribs[1] #find index of first attribute of current group

#            if (attr_group=="aspect")
#            browser()
      
      # SUCCESSIVE weighting for each single attribute
      if (cf_mode == 'successive') {
        attr_weights_class <- 0*attribute_table$group_weight  #modify weights based on original vector of weights
          
        attr_weights_class[current_attribs] = attribute_table$group_weight[current_attribs]
        
        # set specified number of classes to classify to
        nclasses <- attribute_table$n_classes_4lu[iw] 
        
        # in case of erroneous input zero or only 1 class, don't do classification, just append a dummy classification (into one class)
        if (nclasses==0 || nclasses==1) {
          
          cidx_save[[iw]] <- rep(1, n_profs) #put all profiles into class 1
          
          if(!silent) message(paste("% -> skipped '", attr_group, "' (", iw-(iw>2), "/", iw_max, ") because number of classes=1", sep=""))
          next
        }
        
        if(!silent) message(paste("% -> successive clustering loop, treating attribute/group '", attribute_table$group[iw], "' (", iw-(iw>2), "/", iw_max, ")", sep="")) 
        hc <- hc+1
      } # end cases of cf_mode 'successive'/'singlerun'
      
      
      
    # assemble weighted (excerpt) of the resampled profiles 
      attributes2consider = (attr_weights_class !=0) 
      
      n_data_columns_needed =  sum(column_indices[attributes2consider,])
      
      #ii: profs_resampled this is a copy/except of profs_resampled_stored, which is then weighted? Do we need this duplication, or
      #we could weight and "unweigh" (if necessary later) the same instance to save memory?
      profs_resampled <- matrix(0, nrow=n_profs, ncol=n_data_columns_needed)
      
      offset=0
      for (attr_i in which(attributes2consider)) #loop through all attributes that need to be considered in this iteration
      {
        src_cols  = column_indices[attr_i,]
        start_col = offset + 1    #index of start column of this attribute
        end_col   = offset + sum(src_cols)
        
        # Weigh the current attribute
        profs_resampled[,start_col:end_col] = profs_resampled_stored[, src_cols] * attr_weights_class[attr_i] 
        
        # divide by number of fields (end_col-start_col+1) to
        # prevent multi-field attributes to get more relative weight
        #FIXME: why is this all small positives for cos_azimuth
        profs_resampled[,start_col:end_col] = profs_resampled[,start_col:end_col] / (end_col-start_col+1)
        
        offset = end_col #increase offset
      }
      
      
      # CLUSTER-ANALYSIS
      cidx=array(NA,nrow(profs_resampled)) #cluster membership
      if (classify_type=='load') {
        # classification based on loaded classes, TODO
        #[cidx,sumd,dists]=cluster_supervised(profs_resampled_stored,nclasses,mean_prof);
        stop("not yet implemented")
      } else {
        # unsupervised classification
        dups = duplicated(profs_resampled)
        if (nrow(profs_resampled) - sum(dups) <= nclasses) #not enough distinct profiles for this attribute 
        {
          kmeans_out=NULL #disables later plots
          for(jj in 1:nrow(profs_resampled)) 
            cidx[jj]=which(apply(profs_resampled[!dups,, drop=FALSE], MARGIN=1, FUN=identical, profs_resampled[jj,]))
          cmeans2 <- profs_resampled[dups,, drop=FALSE] # matrix of cluster centers
          sumd <- 0   # within-cluster sum of squares, one per cluster  
        } else
        { #regular case  
          kmeans_out <- kmeans(profs_resampled, centers=nclasses, nstart=10)
          
          cidx <- kmeans_out$cluster    # cluster number for each point
          cmeans2 <- kmeans_out$centers # matrix of cluster centers
          sumd <- kmeans_out$withinss   # within-cluster sum of squares, one per cluster
        }  
      }

      # store classification result of this treated attribute
      cidx_save[[iw]] <- cidx
      
      
      if(!silent) message(paste('% -> profile clustering: fitting index_c = ', round(sqrt(sum(sumd^2)),2), sep=""))
      
      
      if (length(unique(cidx)) < nclasses) {
        if(!silent) message(paste("% -> WARNING: ", nclasses-length(unique(cidx)), ' empty clusters produced.', sep=""))
      } else if (make_plots) {
        # silhouette plot, doesn't work with empty clusters
        if (!is.null(kmeans_out) & plot_silhouette)
        {
          dists <- daisy(profs_resampled) # compute pairwise distances, TODO: see warnings
          plot(silhouette(kmeans_out$cluster, dists^2), main=attribute_table$attribute[iw]) # plot silhouette
        }  else
          dists <- matrix(-9999, nrow=n_profs, ncol=nclasses)  #dummy, no distance computed
      
      }
      
      
  
      
      
      # PLOT classified catenas
      # originals
      if (make_plots) {
        plot(1,1,type="n", xlim=c(0,max(profs_resampled_stored[,com_length+1])), ylim=c(0,max(profs_resampled_stored[,com_length+2])),
             main=paste("Original catenas\nclassified according ", attribute_table$attribute[iw], sep=""), xlab="horizontal length [m]", ylab="elevation [m]")
        for (i in 1:n_profs) {
          lines(   (0:(com_length-1) / (com_length-1)) * profs_resampled_stored[i,com_length+1], 
                profs_resampled_stored[i,1:com_length] * profs_resampled_stored[i,com_length+2], col=cidx[i])
        }
      }
      
      # modified
      if (make_plots) {
        plot(1,1,type="n", xlim=c(0,com_length-1), ylim=c(0,1), main=paste("catenas, resampled, reduced & normalized\nclassified according ", attribute_table$attribute[iw], sep=""), 
             xlab="catena point number", ylab="relative elevation")
        for (i in 1:nrow(profs_resampled_stored)) {
          lines(0:(com_length-1), profs_resampled_stored[i,1:com_length], col=cidx[i])
        }
      }
      
  
    } # end classification loop through all attributes
    
    
    
    # complete key generation (successive mode only)
    # successive weighting mode: all prior classifications will be merged into one by generating unique key 
    if (cf_mode == 'successive') {
      cidx_save[[iw_max+1]] <- 0 #inititalize overall (composite) classification result
      
      for (iz in 1:iw_max) {
        
        # eliminate empty iw=3 (horiz. & vertical extent treated together) by jumping to next entry
        if (iz==3) next
        
        # generate key from previous classifications
        cidx_save[[iw_max+1]] <- cidx_save[[iw_max+1]]*10 + cidx_save[[iz]] #attention: this will be faulty when more than 10 classes have been chosen for any attribute, fix this
      }
      
      nclasses <- length(unique(cidx_save[[iw_max+1]]))
      #attribute_table$n_classes_4lu[iw+1] <- nclasses
      
      # "pretend" this is the actual classification
      cidx <- cidx_save[[iw_max+1]]
      
      # quick and dirty computation of distance matrix to allow the rest of the script to be run without problems
      dists <- matrix(1, nrow=n_profs, ncol=nclasses) 
      unique_classes <- unique(cidx)
      for (ii in 1:nclasses) {
        # find all profiles belonging to current class
        class_i <- which(cidx==unique_classes[ii])
        # set their distance to the cluster centre to 0
        dists[class_i,ii] <- 0
      }       
    } # end key generation
    
    if(!silent) message("% OK.")
    
    
    
# calculation of mean catena for each cluster #--------------------------------
    if(!silent) message("%")
    if(!silent) message("% Calculate mean catena for each cluster...")
    
    unique_classes <- unique(cidx)
    
    if (length(unique_classes)!=nclasses) {
      if(!silent) message(paste('% -> WARNING: Number of generated classes (', length(unique_classes), ') is not not as expected (', nclasses, '). Too few EHAs, too many classes requested, too few differences in EHAs? Please check what happended.'))
      nclasses <- length(unique_classes)
    }
    
    mean_prof <- matrix(NA, nrow=nclasses, ncol=ncol(profs_resampled_stored)) # mean shape of every class
    class_repr <- matrix(NA, nrow=nclasses, ncol=2) # min. distance of class i to centroid and resp. ID
    lims_collected <- NULL   #for collecting the resulting TC limits for each LU
    
    for (i in 1:nclasses) {
      
      # find all profiles belonging to current class
      class_i <- which(cidx==unique_classes[i])
      
      # empty cluster
      if (!any(class_i)) {
        if(!silent) message(paste('-> WARNING: cluster ', i, ' is empty.', sep=""))
        next
      }
      
      # calculate mean catena (average attributes over all profiles belonging to the current class)
      mean_prof[i,] <- apply(profs_resampled_stored[class_i,, drop=F],2,mean)
      
      # find closest catena (=most representative) to each class centre
      dists_class_i <- dists[class_i,i]        # retrieve distances of catenas of class i to centroid of class i
      class_repr[i,2] <- min(dists_class_i)   # find minimum distance of class i
      j <- min(which(dists_class_i == min(dists_class_i)))
      class_repr[i,1] <- class_i[j]                 # store internal id of closest catena
      
      # TODO: what is this good for?!
      Y <- sort(dists_class_i)
      ix <- sort(dists_class_i, index.return=T)$ix
      if (cf_mode=='singlerun') {
        if(!silent) message(paste('% -> WARNING: three closest catenas to centre of class ', i, ' (ext id): ', p_id_unique[class_i[ix[1:min(3,length(ix))]]], sep=""))
      }
      
      
      # draw a separate figure with the cluster centre (mean toposequence) and the closest catena
      if (make_plots && FALSE) {
        xvec <- c(0:(com_length-1))/(com_length-1)*mean_prof[i,com_length+1]
        yvec <- mean_prof[i,1:com_length] * mean_prof[i,com_length+2]
        
        plot(xvec, yvec, type="l", main=paste('mean toposequence and closest catena\nclass ', i, sep=""),
             xlab='horizontal length', ylab='relative elevation gain')
        lines(profsx[[class_i[j]]], profs[[class_i[j]]]-min(profs[[class_i[j]]]), col="blue")
        legend("topleft", c("mean toposequence", "closest catena"), col=c("black", "blue"), lty=c(1,1))
      }
      
      
      
      # save reclass files
      write(file=paste(dir_out,recl_lu,sep="/"), append=ifelse(i==1,FALSE,TRUE), x=paste(p_id_unique[class_i], "=",i," ", unique_classes[i], sep=""))
      
    } # end calc mean catena of each class
    
    # in reclass files: all other (unclassified) profiles are assigned nodata
    write(file=paste(dir_out,recl_lu,sep="/"), append=T, x="* = NULL")
    

#order output    
#order by group
    # order1 = order(attribute_table$group, 1:nrow(attribute_table))
    # 
    # #bring "shape", "x_extent", "z_extent" to top
    # new_order = match(c("shape", "x_extent", "z_extent"), attribute_table$attribute[order1])
    # new_order = c(new_order, which(! (attribute_table$attribute %in% c("shape", "x_extent", "z_extent"))))
    # 
    # mean_prof
#    browser()
    
# prepare output files ----------------------------------------    
  
    #---------prepare file output luoutfile
    # prepare header line for output file
    dumstr <- paste('LU-ID', tab, 'closest_catena', tab, 'closest_dist', tab, 'x_length', tab, 'y_length', sep="")
    
    # write header fields for calculated TC-limits, minimisation of variance
    if (ntc > 1) {
      for (i in 1:(ntc-1)) {
        dumstr <- paste(dumstr, tab, 'lim_var', i, sep="")
      }
      
      # write header fields for calculated TC-limits, cluster analysis
      for (i in 1:(ntc-1)) {
        dumstr <- paste(dumstr, tab, 'lim_clu', i, sep="")
      }
    }
    
    # write header for all attributes
    #browser()
    output_cols = !(attribute_table$attribute %in% c("x_extent", "z_extent")) #the two extent fields have been treated before, output the rest

    #legacy cosmetics for downward compatibility of output files
    renamed =which(attribute_table$attribute=="shape") 
    if (length(renamed)>0) attribute_table$attribute[renamed]="elevation"
      
    for (i in which(output_cols)) {
      # write all fields of this attribute for this catena point
      for (k in 1:attribute_table$n_datacolumns[i]) {
        # write this attribute for all catena points FIXME: treat non-spatial attributes differently
        for (j in 1:ifelse(attribute_table$is_spatial[i], com_length, 1)) {
          dumstr <- paste(dumstr, tab, attribute_table$attribute[i], '_p', j, sep="")
          
          # if this is a multi-field attribute...
          if (attribute_table$n_datacolumns[i]>1) dumstr <- paste(dumstr, '_c', k, sep="")  # denote field numbering in header
        }
      }
    }
    
    #legacy: revert to original name
    if (length(renamed)>0) attribute_table$attribute[renamed]="shape"
    
     # write header string to output file
    write(file=paste(dir_out,luoutfile,sep="/"), append=F, x=dumstr)
    #---------end prepare file output
    
    
    
    #---------prepare file output tc.dat
    dumstr <- 'TC'
    # write header for all attributes
    #browser()
    for (i in 4:length(attribute_table$n_datacolumns)) {
      # write all fields of this attribute for this catena point
      for (k in 1:attribute_table$n_datacolumns[i]) {
        dumstr <- paste(dumstr, tab, attribute_table$attribute[i], sep="")
        
        # if this is a multi-field attribute...
        if (attribute_table$n_datacolumns[i]>1) dumstr <- paste(dumstr, '_c', k, sep="")  # denote field numbering in header
      }
    }
    
    # write header string to output file tc.dat
    write(file=paste(dir_out,tcoutfile,sep="/"), append=F, x=dumstr)
    #---------end prepare file output tc.dat
    

    #---------prepare file output r_tc_contains_svc
    svc_col_index <- 0
    # find index of column containing svcs
    for (j in 1:length(attribute_table$attribute)) {
      if (attribute_table$attribute[j]==svc_column) {
        svc_col_index <- j
        break
      }
    }
  
    if (svc_col_index) {
      #write header string to output file r_tc_contains_svc
      write(file=paste(dir_out,tccontainssvcoutfile,sep="/"), append=F, x='tc_id\tsvc_id\tfraction')
      
      svc_recl_file <- paste('reclass_', attribute_table$attribute[svc_col_index], '.txt', sep="")
      
      # check existence of file containing original SVC-IDs
      if (file.exists(svc_recl_file)) {
        svc_recl_dat <- read.table(svc_recl_file, header=T)
        mod_svc_ids <- svc_recl_dat$new_id
        org_svc_ids <- svc_recl_dat$original_id
      } else {
        warning(paste0(svc_recl_file, " not found. SVC-ids may have changed, please check."))
        # no reclass file found - don't change IDs
        #browser()
        mod_svc_ids <- 1:attribute_table$n_datacolumns[svc_col_index]
        org_svc_ids <- mod_svc_ids
      }
    }
    #---------end prepare file output r_tc_contains_svc
      

    
    
    if(!silent) message("% OK.")
    
    
    
# decomposition into TCs #-----------------------------------------------------
    if(!silent) message("%")
    if(!silent) message("% Decomposition into TCs...")
    
    lu_contains_tc <- NULL
    if (ntc==1) {
      if(!silent) message('% -> NOTE: only one TC per LU will be produced!')
    }
    
    # save original mean_prof (for later plotting only)
    mean_prof_orig_t <- mean_prof
    
    #create labels for LUs
    lu_labels=NULL #labels for LUs consisting of appended class memberships for each attribute  
    for (i in 1:nclasses) {
      class_i <- which(cidx==unique_classes[i])
      curr_lu_key <- unique(cidx[class_i])
      lu_labels=c(lu_labels, curr_lu_key) #ii
    }  
    # PARTITIONING OF MEAN PROFILE FOR EACH LU #
    for (i in 1:nclasses) {
      if(!silent) message(paste('% -> partitioning class ', i, ' of ', nclasses, sep=""))
      
      # find all profiles belonging to current class
      class_i <- which(cidx==unique_classes[i])
      
      # get the ID of the currently treated LU
      curr_lu_key <- unique(cidx[class_i])
      if(length(curr_lu_key) > 1)
        stop("Unexpected error during partitioning of the mean profile for each LU: more than one LU identified in partitioning loop. Contact the package developer!")
      
      # if supplemental data exists, calculate average
      if (n_suppl_attributes) {
        mean_supp_data_t <- apply(profs_resampled_stored[class_i,(com_length+3):ncol(profs_resampled_stored), drop=F],2, mean)
        mean_supp_data <- matrix(mean_supp_data_t, ncol=com_length, nrow=n_supp_data_columns, byrow=T)

      } else {
        mean_supp_data <- 0      # no supplemental data present
      }
      
      
      # compute x-dimension for current mean profile
      xvec <- c(0:(com_length-1))/(com_length-1)*mean_prof[i,(com_length+1)]

      
      # TERRAIN COMPONENTS
      # get exact horizontal resolution for exact plotting
      dx <- xvec[com_length]/(com_length-1)
      if (ntc==1) {
        # only one TC per LU has to be produced - not much work to be done
        lim_var=NULL
        lim_clu=NULL
      } else {
        
        # decompose profile into terrain components
        #    [lim_var,lim_clu] = tc_decomp(mean_prof(i,1:com_length), mean_supp_data, attribute_table$n_datacolumns, attribute_table$weight_4tc, xvec,monocrome,plot_approx_ts);
        #    tc_decomp(prof, supp_data, attribute_table$n_datacolumns, attribute_table$weight_4tc, xvec, monocrome, plot_approx_ts)
        
        # fill hollows/sinks
        for (ii in 2:com_length) {
          mean_prof[i,1:com_length][ii] <- max(mean_prof[i,1:com_length][ii-1], mean_prof[i,1:com_length][ii])
        }
        # compute local slopes of profile
        prof_slopes <- vector("numeric", length=com_length-1)
        # the first and last point are treated differently
        prof_slopes[1] <- (mean_prof[i,1:com_length][2]-mean_prof[i,1:com_length][1])/dx
        for (ii in 2:(com_length-1)) {
          prof_slopes[ii] <- 0.5/dx*((mean_prof[i,1:com_length][ii+1]-mean_prof[i,1:com_length][ii])+(mean_prof[i,1:com_length][ii]-mean_prof[i,1:com_length][ii-1]))
        }
        prof_slopes[com_length] <- (mean_prof[i,1:com_length][com_length]-mean_prof[i,1:com_length][com_length-1])/dx
        
        
        
        # if supplemental data is present
        if (n_suppl_attributes>0 && any(attribute_table$weight_4tc[4:length(attribute_table$weight_4tc)]>0)) 
        {
          supp_data_weighted <- NULL
          supp_data_weighted <- array(0, dim(mean_supp_data))
          # weigh supplemental information according to weighting factors given
          #browser()
          for (jj in 4:length(attribute_table$n_datacolumns)) {
            # if an attribute is to be weighted with 0, we can as well skip it
            if (attribute_table$weight_4tc[jj]==0) next
            attr_start_column <- sum(attribute_table$n_datacolumns[1:(jj-1)][-(1:3)])+1 
            attr_end_column <- attr_start_column+attribute_table$n_datacolumns[jj]-1
            
            ci = range(which(column_indices[jj,])) #get columns of current attribute
            attr_start_column_src = ci[1]
            attr_end_column_src   = ci[2]
            supp_data_weighted[attr_start_column:attr_end_column,] <- mean_prof[i,attr_start_column_src:attr_end_column_src]*attribute_table$weight_4tc[jj]/(attr_end_column-attr_start_column+1)
          }  
          # data that is given to partitioning algorithm
          #remove columns without variability to conserve space and computation time
          to_keep=rep(TRUE, nrow(supp_data_weighted))
          for (jj in 1:nrow(supp_data_weighted)) 
            to_keep[jj] =  any(supp_data_weighted[jj,1] != supp_data_weighted[jj,]) #check if this row contains different values
          supp_data_weighted = supp_data_weighted[to_keep,]  
          pdata <- rbind(prof_slopes, supp_data_weighted)
        } else {
          # only the slope data is given to partitioning algorithm
          supp_data_weighted <- 0
          pdata <- matrix(prof_slopes,nrow = 1)
        }
        

        # decomposition using min variance
        b_part <- best_partition_new(pdata, ntc)

        qual <- b_part[1] # partitioning quality
        best_limits <- b_part[-1]  # best limits of partitioning
        
        if(!silent) message(paste('% -> partition by min variance: ', paste(best_limits, collapse=" "), 
                      '; fitting index_v = ', qual, sep=""))
        
        
        #decomposition using cluster analysis - deactivated
        best_limits_c <- NULL
        best_limits_c[1:(ntc-1)] <- 0
        if (FALSE){ 
          clusterdata <- pdata # matrix containing data that is fed to cluster analysis
          dummy_dim_appended <- 0 # flag if dummy dimension has already been appended
          
          # modify classification data by adding "location" dimensions [1..lengths]
          # until the classification produces spatially continuous clusters
          for (j in 1:(length(supp_data_weighted)*10+2)) {
            
            # cluster analysis
            kmeans_out <- kmeans(clusterdata, centers=ntc, nstart=5)
            cidxtc <- kmeans_out$cluster    # cluster number for each point
            cmeanstc <- kmeans_out$centers # matrix of cluster centers
            fitc <- kmeans_out$withinss   # within-cluster sum of squares, one per cluster
            
            best_limits_c <- NULL
            continuous_clusters <- 1
            
            # check if clusters are continuous or distributed
            for (k in 1:ntc){
              xvec <- which(cidxtc==k) # find limits
              
              if (!any(xvec)) next
              
              # check if found cluster is connected / undistributed
              if (sqrt(sum((xvec - c(xvec[1]:(xvec[1]+length(xvec)-1)))^2)) != 0) {
                continuous_clusters <- 0
                break
              } else if (xvec[1]!=1) {
                # the beginning of the profile is not an actual cluster limit, so don't store this
                best_limits_c <- c(best_limits_c, xvec[1])
              } 
            }

            # continuous clustering achieved?
            if (continuous_clusters) {
              break
            } else {
              if (!dummy_dim_appended) {
                # append another dummy- / location dimension (scaled to match the order of magnitude of the original data) and do another loop
                clusterdata <- c(clusterdata, c(1:length(prof_slopes))/length(prof_slopes)*mean(clusterdata ))
                dummy_dim_appended <- 1
              } else {
                # double the values of the existing dummy dimension to force the creation of continuous clusters
                clusterdata[length(clusterdata)] <- clusterdata[length(clusterdata)]*2
              }
            }
            
          } # end modify classification data
          
          
          # continuous clustering achieved?
          if (!continuous_clusters) {
            if(!silent) message('% -> WARNING: partitioning using cluster analysis failed.')  
            best_limits_c[1:(ntc-1)] <- 0
          } else {
            # sort limits to ascending order
            best_limits_c <- sort(best_limits_c)
            if(!silent) message(paste('% -> partition by clustering  : ', paste(best_limits_c, collapse=" "), '; fitting index_c = ', sqrt(sum((fitc)^2)), sep=""))
            
            # only for drawing - from beginning till end of profile
            best_limits_t <- c(1, best_limits_c, length(mean_prof[i,1:com_length]))
            xvec <- seq(0,(com_length-1)*dx, by=dx)
            
            # plot
            if (make_plots) {   
              lines(xvec[best_limits_t], mean_prof[i,1:com_length][best_limits_t], col="green")
              
              # plot limits using vertical lines
              for (m in 1:length(best_limits_c)) {
                lines(c((best_limits_c[m]-1)*dx, (best_limits_c[m]-1)*dx), c(mean_prof[i,1:com_length][best_limits_c[m]], max(mean_prof[i,1:com_length])*1.1), lty=2, col="green")
              }
            }
          }
        }
        
        
        # save values
        lim_var <- best_limits
        lim_clu <- best_limits_c
        
      } # end else ntc==1
      
      lims_collected=rbind(lims_collected, c(lim_var, lim_clu)) #collect determined TC limits
      
      if (make_plots) {
        # plot orig
        plot(xvec, mean_prof_orig_t[i,1:com_length], type="l", xlab='horizontal length', ylab='relative elevation gain',
             main=paste("partitioning class ", i, sep=""))
      # plot filled
        points(xvec, mean_prof[i,1:com_length], pch=1)
      
      # plot parameterized slope  
        if(ntc==1) {
          lines(c(min(xvec), max(xvec)), mean_prof[i,c(1,com_length)], col="red")
        } else {
          # only for drawing - from beginning till end
          best_limits_t <- c(1, best_limits, length(mean_prof[i,1:com_length]))
          
          lines(xvec[best_limits_t], mean_prof[i,1:com_length][best_limits_t], col="red")
          
          # plot limits using vertical lines
          for (ii in 1:length(best_limits)) {       
            lines(c((best_limits[ii]-1)*dx, (best_limits[ii]-1)*dx), c(0, mean_prof[i,1:com_length][best_limits[ii]]), lty=2, col="red")
          }
        }
    
        # plot legend
        #           legend("topleft", c("orig. toposequence", "filled profile", "approx. (min var)", "TC boundary (var)",
        #                               "approx. (cluster anal.)", "TC boundary (cluster anal.)"), lty=c(1,NA,1,2,1,2),
        #                  pch=c(NA,1,NA,NA,NA,NA), col=c("black", "black", "red","red","green","green"))
        # cluster analysis currently not supported and does not need to appear in plots
        legend("topleft", c("orig. toposequence", "filled profile", "approx. (min var)", "TC boundary (var)"), 
               lty=c(1,NA,1,2), pch=c(NA,1,NA,NA), col=c("black", "black", "red","red"))
      }
      
#### write output    #-----------------------------------
   
      
      #----------file TC-output
      lim_var_t <- c(0.5, lim_var, com_length+0.5)
      
      # TC fractions
      frac_tc <- vector("numeric", length=ntc)
      for (j in 1:ntc) {
        frac_tc[j] <- (lim_var_t[j+1]-lim_var_t[j])/com_length
      }
      
      lim_var_t[1] <- 1
      lim_var_t[length(lim_var_t)] <- com_length
      
      # set start of profile to zero
      mean_prof[i,1:com_length] <- mean_prof[i,1:com_length]-mean_prof[i,1]
      
      # TC slopes
      ts <- 0
      slope_tc <- NULL
      for (j in 1:ntc) { 
        if (lim_var_t[j+1]==1) {
          # partition at first point reaches halfway to next sampling point
          ydist <- 0.5*mean_prof[i,2]* mean_prof[i,com_length+2]
        } else if (lim_var_t[j]==com_length) {
          # partition at last point start halfway from previous sampling point
          ydist <- 0.5*(mean_prof[i,com_length]-mean_prof[i,com_length-1])* mean_prof[i,com_length+2]
        } else {
          if (lim_var_t[j+1]==com_length && j==(ntc-1)) {
            # if the last TC is one sampling point only, the second-to-last ends halfway before last sampling point
            ydist <- (0.5*(mean_prof[i,com_length]+mean_prof[i,com_length-1])-mean_prof[i,lim_var_t[j]])* mean_prof[i,com_length+2]
          } else {
            # normal case
            ydist <- (mean_prof[i,lim_var_t[j+1]]-mean_prof[i,lim_var_t[j]])* mean_prof[i,com_length+2]
          }
        }
        
        xdist <- mean_prof[i,com_length+1] * frac_tc[j]
        slope_tc[j] <- 100*ydist/xdist
        ts <- ts+ydist
      }
      
      # write data fields for all TCs
      lu_id <- i
      for (j in 1:ntc) {
        dumstr <- ''
        treated_attribs <- 0
        tc_id <- (lu_id-1)*ntc+j  # generate a unique number for the TC
        
        from_point <- lim_var_t[j]
        till_point <- lim_var_t[j+1]
        
        # write data for all supplemental attributes
        for (ii in 4:length(attribute_table$n_datacolumns)) {
          # write all fields of this attribute for this catena point
          for (k in 1:attribute_table$n_datacolumns[ii]) {
            tmp_v <- round(mean(mean_supp_data[treated_attribs+k,from_point:till_point]),5)
            dumstr <- paste(dumstr, paste(tmp_v,collapse=tab),sep=tab)
            
            #---------output r_tc_contains_svc.dat
            # if this is the svc column...
            if (ii==svc_col_index) {
              # print out svc-fration, if greater than 0
              if (tmp_v) {
                # fraction of svc in current tc to output file r_tc_contains_svc
                write(file=paste(dir_out,tccontainssvcoutfile,sep="/"), append=T, x=paste(tc_id, org_svc_ids[k], tmp_v, sep=tab))
              }
            }
            #---------end output r_tc_contains_svc.dat
          }
          # increase counter for attributes already treated
          treated_attribs <- treated_attribs+k
        }
        # write constitution of this TC (tc.dat)
        write(file=paste(dir_out,tcoutfile,sep="/"), append=T, x=paste(tc_id, paste(dumstr,collapse=tab), sep=""))
      } # end loop over ntc
      
      
      tc_ids <- c(1:ntc)+(lu_id-1)*rep(1,ntc)*ntc
      
      lu_contains_tc <- rbind(lu_contains_tc, cbind(i*rep(1,ntc), tc_ids, round(frac_tc,5), 1:ntc, slope_tc))
      #----------end file TC-output
      
    } # end loop over nclasses for TC decomposition
    
  write.table(file=paste(dir_out,"lu_labels.dat",sep="/"), x=data.frame(no=1:nclasses, lu_labels=lu_labels), append=F, row.names=FALSE, quote=FALSE, sep=tab) #label file
    
    # close plot output device
  if (make_plots)
    dev.off()
    
    
  #----------file output lu.dat
  lu_out_dat <- cbind(1:nclasses, p_id_unique[class_repr[,1]], class_repr[,2], round(mean_prof[,(com_length+1):(com_length+2)],1),
                  # write LU-ID, closest catena and its distance, catena length and relative elevation
                  lims_collected[,],   # write limits of TC-decomposition
                  round(mean_prof[,-(com_length+1:2)],2)) # write elevation data and all supplemental data
  # write into file
  write.table(file=paste(dir_out,luoutfile,sep="/"), append=T, x=lu_out_dat, sep=tab, col.names = FALSE, row.names=FALSE, quote=FALSE)
  #----------end file output
  
     
    
    # save remaining classification results
    # cluster centers can be saved for future use (supervised classification, single run only)
    if (classify_type=='save'){
      save('mean_prof','com_length','attribute_table',file=paste(dir_out,saved_clusters,sep="/"));
      if(!silent) message(paste("% -> NOTE: saved cluster centers to ", dir_out, "/", saved_clusters, sep=""))
    }
    
    
    # write LU contains TC files
    colnames(lu_contains_tc) <- c("lu_id", "tc_id", "fraction", "position", "slope")
    lu_contains_out <- matrix(lu_contains_tc[,-5], ncol=4, dimnames=list(NULL, colnames(lu_contains_tc)[-5])) # ensure matrix if even if only one row exists
    write.table(lu_contains_out, file=paste(dir_out,lucontainstcoutfile,sep="/"), quote=F, col.names=T, row.names=F, sep=tab)
    
    tc_dat <- cbind(lu_contains_tc[,2], rep("NA",nrow(lu_contains_tc)), round(lu_contains_tc[,5],2), rep(NA,nrow(lu_contains_tc)), rep(NA,nrow(lu_contains_tc)), rep(NA,nrow(lu_contains_tc)))
    colnames(tc_dat) <- c("pid", "description", "slope", "frac_rocky", "beta_fac", "sdr")
    write.table(tc_dat, file=paste(dir_out,terraincomponentsoutfile,sep="/"), quote=F, col.names=T, row.names=F, sep=tab)
    
    
    
    # stop sinking
    closeAllConnections()
    
    # restore original warning mode
    if(silent)
      options(warn = oldw)
    
    
    if(!silent) message("% OK.")
    if(!silent) message('%')
    if(!silent) message("% DONE!")
    if(!silent) message("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    

    # if an error occurs...
  
    
    # },    error = function(e) {
    # 
    #  # stop sinking
    #  closeAllConnections()
    #   
    #  if (make_plots) dev.off() #close open devices
    #  # restore original warning mode
    #  if(silent)
    #    options(warn = oldw)
    # 
    #  stop(paste(e))
    # 
    #  })
    
} # EOF


### INTERNAL FUNCTIONS USED BY prof_class ###----------------------------------

# delivers partition quality: sum of weighted variances of subdivisions #------
get_part_quality <- function(data_mat, lim, cur_best=Inf) {
  
  # get number of points contained in this data (sub-)set
  n_points_in_data_mat <- ncol(data_mat) 
  
  qual <- 0
  lim <- c(1, lim, n_points_in_data_mat+1)
  
  # function get_part_quality() in original matlab script
  for (iii in 1:(length(lim)-1)) {
    part_start <- lim[iii] # start index of current partition
    part_end <- max(part_start,lim[iii+1]-1) # end index of current partition
    
    if (part_start==part_end) {
      next       # this CAN be done because the variance in a subsection that contains only one point is zero
      # it MUST be done because the var() function doesn't work as intended with one column matrices
      # if you use an alternative quality formula, remove the continue
    }
    
    # factor for weighting the variance of this subdivisions in the overall variance
    wf <- (part_end-part_start)
    # sum up weighted variances of subdivisions
    w_var = wf*sum(apply(data_mat[,part_start:part_end, drop=FALSE], MAR=1, FUN = var)) 
    qual = qual + w_var
    if (qual >= cur_best) break #the current best is already reached or exceed, don't look any further
  }
  
  return(qual)
} # EOF



# partitioning of a hillslope into Terrain Components #------------------------
best_partition <- function(data_mat, no_part, cur_best=Inf) {
  
  n_points_in_data_mat <- ncol(data_mat) 
  
  # partition the vector in 2 only subdivisions
  if (no_part==2) {
    # initial value for loop
    best_lim_qual <- cur_best
    best_limit = 1
    startat <- 1
    
    for (jj in startat:n_points_in_data_mat) {
      # get quality of this subdivision
      lim_qual <- get_part_quality(data_mat, jj, best_lim_qual)
      
      # this subdivision is better than the previous best
      if (lim_qual < best_lim_qual) {
        best_lim_qual <- lim_qual    # set new best subdivision
        best_limit <- jj
      }    
    } # end loop over data_mat
    
    # return the best limitation found and its quality
    return(c(best_lim_qual, best_limit))  
    
  } else {
    
    # initial value for loop
    best_lim_qual <- Inf #cur_best
    bl = 1 #default: nothing better found
    
    for (ii in 1:(n_points_in_data_mat-no_part-2)) {
      # get best partitioning inside the remaining part - the returned quality value is not used
      bp <- best_partition(data_mat[,ii:n_points_in_data_mat, drop=FALSE], no_part-1, best_lim_qual) 
      lim_qual <- bp[1]
      if (lim_qual > best_lim_qual) next #no use searching any further
      best_limits <- bp[2:(no_part-1)]
      
      # with a limit at ii, the rest is best partitioned as contained in best_limits
      this_i_best_limits <- c(ii, best_limits+ii-1)
      
      # get the overall quality of this partitioning
      lim_qual <- get_part_quality(data_mat, this_i_best_limits, best_lim_qual)

      # this subdivision is better than the previous best
      if (lim_qual < best_lim_qual) {
        # set new best subdivision
        best_lim_qual <- lim_qual
        bl <- this_i_best_limits
      }
      
    }
    # return the best limitation found and its quality
    return(c(best_lim_qual, bl))
    
  } # end if no_part==2
} # EOF

best_partition_new <- function(data_mat, no_parts, start=NULL)
{
  shift_break = function(breaks, break_no, n_points_in_data_mat)
  {
    no_parts=length(breaks)+1
    if (breaks[break_no] < n_points_in_data_mat - (no_parts-1-break_no) ) #shifting possible?
      breaks[break_no:(no_parts-1)] = breaks[break_no] + (1:(no_parts-break_no)) else
      breaks=NA  #end reached
    return(breaks)    
  }   
 
  best_lim_qual=Inf
  
  variances = array(NA, no_parts)
  n_points_in_data_mat <- ncol(data_mat)
  breaks = 1:(no_parts-1)
  
  active_break = no_parts-1 #start shifting here
  turnover=FALSE
  
  if (!is.null(start)) breaks=start
 

  while (TRUE)
  {
    #shift active break
    breaks_new = shift_break(breaks, break_no=active_break, n_points_in_data_mat = n_points_in_data_mat )
    if (is.na(breaks_new[1]))
    {
      if (active_break == 1)  {       
        break
        } else   #no previous break to shift, all done
        active_break=active_break-1 #shift previous break
        turnover=TRUE #remember to change to back next break after next iteration
        variances[active_break:(no_parts-1)]=NA #demark variances that need updating
        next
    } else
    breaks=breaks_new
    variances[active_break]=NA #from here, new calculation of variance is necessary
    if (turnover) active_break = no_parts-1  #back to shifting last break
    stepup_again=0
    
    #update highest missing variance until rest
    start_part=min(which(is.na(variances)))  #no need to update all values, some did not change
    qual=sum(variances[1:(start_part-1)], na.rm=TRUE)
    lim <- c(1, breaks, n_points_in_data_mat+1)
    for (iii in start_part:(length(lim)-1)) {
    
      part_start <- lim[iii] # start index of current partition
      part_end <- max(part_start,lim[iii+1]-1) # end index of current partition
      
      if (part_start==part_end) {
        variances[iii]=0
        next       # this CAN be done because the variance in a subsection that contains only one point is zero
        # it MUST be done because the var() function doesn't work as intended with one column matrices
      }
      
      # factor for weighting the variance of this subdivisions in the overall variance
      wf <- (part_end-part_start)
      # sum up weighted variances of subdivisions
      variances[iii] = wf*sum(apply(data_mat[,part_start:part_end, drop=FALSE], MAR=1, FUN = var))
      qual = qual + variances[iii]
      if (qual >= best_lim_qual) 
      {  
        active_break=min(active_break, iii)
        break #abort, if already worse than current best and modify this break
      }  
    }
    if (qual < best_lim_qual) { #new optimun found
      # set new best subdivision
      best_lim_qual <- qual
      bl <- breaks
    }
    
  }
  return(c(best_lim_qual, bl))

}

# 
# 
# 
# system.time({ıb2=best_partition(vec, 5)})
# 
# b2
# 
# get_part_quality(vec, b2)
# get_part_quality(vec, a[-1])
