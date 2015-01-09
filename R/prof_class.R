#' Classification of mean catenas
#' 
#' Classifies mean catenas derived from \code{\link[LUMP]{area2catena}} into \emph{Landscape
#' Units} and \emph{Terrain Components}.
#' 
#' @param catena_file Name of file containing mean catena information derived from
#'      \code{\link[LUMP]{area2catena}}.
#' @param catena_head_file Name of file containing meta-information for classification
#'      derived from \code{\link[LUMP]{area2catena}} and adjusted manually (see \code{Notes}).
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
#'      characteristics for later re-use; set to NULL to omit output.
#' @param classify_type Type of classification:\cr
#'      ' ': (default) unsupervised classification, no \code{saved_clusters} will be produced\cr
#'      'save': do unsupervised classification and save cluster centers to \code{saved_clusters}
#'              for future supervised classification\cr
#'      'load': load cluster centers from existing file and classify according
#'              to these clusters (e.g. supervised classification)
#' @param seed Integer specifying seed for random processes in cluster analysis.
#' @param resolution Integer specifying resolution of profiles/spacing of samples.
#'      Typically the resolution of your GRASS location used for \code{\link[LUMP]{area2catena}}.
#' @param max_com_length Integer specifying maximum common length (-> support points)
#'      of profiles (if there are more points it takes too much time).
#' @param make_plots logical; visualisation of classification results written into
#'      sub-directory \emph{plots_prof_class}.
#' @param eha_subset NULL or integer vector with subset of EHA ids that shall
#'      be processed (for debugging and testing).
#'      
#' @return Function returns nothing. Output files are written into output directory
#'      as specified in arguments.
#' 
#' @note Function uses output of \code{\link[LUMP]{area2catena}}. However, no GRASS
#'      session needs to be started in this case.
#'      
#'      TODO:\cr
#'        - \code{classify_type} 'load'\cr
#'        - what to do with cluster analysis in prof_class.R (turned off, variance analysis
#'          is used instead)?\cr
#'        - seed obsolete?!
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
prof_class <- function(
  
  ### INPUT ###
  catena_file,
  catena_head_file,
  
  ### OUTPUT ###
  dir_out="./",
  luoutfile,
  tcoutfile,
  lucontainstcoutfile,
  tccontainssvcoutfile,
  terraincomponentsoutfile,
  recl_lu,
  saved_clusters,
  
  ### PARAMETERS ###
  seed,
  resolution,
  classify_type=' ',
  max_com_length,
  make_plots=F,
  eha_subset=NULL
) {
  
  ### CALCULATIONS ###
  message("START 'prof_class'.")
  message("")
  
  
  # SETTINGS #
  # output dir
  dir.create(dir_out, recursive=T, showWarnings=F)
  
  if (make_plots) {
    if(length(dir(paste(dir_out, "plots_prof_class", sep="/"))) != 0)
      stop("Output directory for plots '", dir_out, "/plots_prof_class/' is not empty!")
    
    dir.create(paste(dir_out, "plots_prof_class", sep="/"), recursive=T)
  }
  
  # immer der gleiche Zufall - hilft bei debuggen und erzeugt konstante Ergebnisse
  set.seed(seed)
  
  # horizontal resolution of profiles/spacing of samples
  dx <- resolution
  
  # separator in outfiles
  tab <- "\t"
  
  
  # LOAD INPUT #
  # load stats header
  headerdat <- as.matrix(read.table(catena_head_file, header=T))
  # specification of number of columns used by each attribute
  datacolumns <- headerdat[1,]
  # relative weight of each attribute (supplemental data) to be used in classification
  attr_weights_class <-  headerdat[2,]
  # relative weight of each attribute (supplemental data) to be used in partition  (terrain component decomposition)
  attr_weights_partition <- headerdat[3,]
  if (attr_weights_partition[1] < 1) {
    message(paste('Warning: number of TCs will be set to 2 instead of ', attr_weights_partition[1], ' as specified in catena_head_file', sep=""))
    attr_weights_partition[1] <- 2
  }
  
  # store the names of the attributes
  attr_names <- colnames(headerdat)
  
  # determine type of classification from catena_head_file
  if (attr_weights_class[1] < 0) {
    cf_mode <- 'successive' # classification performed to specified number of classes for each attribute (option 2)
    attr_weights_class <- abs(attr_weights_class)
    
    # only for nice output in loop
    attr_names[1] <- 'shape'
    attr_names[2] <- 'xy-extent'
  } else {
    cf_mode <- 'singlerun' # classification performed in single run for all classes using the specified weighting factors (option 1)
  }
  
  # number of classes to divide the dataset into
  nclasses <- attr_weights_class[1]
  # number of TCs to be created in each LU
  ntc <- attr_weights_partition[1]
  
  # if supervised classification using saved clusters is to be used
  #   com_length <- -1
  
  # load standard catena data
  stats <- scan(catena_file, what=numeric())
  stats <- matrix(stats, ncol=sum(datacolumns), byrow=TRUE)
  s2 <- stats
  
  
  p_id <- stats[,1]
  # if eha_subset is specified: select resp. values in every necessary vector/matrix
  if (!is.null(eha_subset)) 
  {
    message('')
    warning(paste('Using only subset of input catenas.'))
    
    #start_prof <- max(start_prof, min(p_id))
    #end_prof <-   min(end_prof,   max(p_id))
    
    stats <- stats[eha_subset,]
    p_id <- stats[,1]
    
  }
  
  
  
  # load supplemental data if present
  if (length(datacolumns) > 3) {
    n_supp_data_columns <- sum(datacolumns[4:length(datacolumns)])
    supp_data <- stats[,4:(n_supp_data_columns+3)]
    n_suppl_attributes <- length(supp_data[1,])      # get number of supplemental attributes
  } else {
    n_supp_data_columns <- 0
    supp_data <- NULL
    n_suppl_attributes <- 0
  }
  
  # arrange input data
  px <- stats[,2]
  elev <- stats[,3]
  p_id_unique = unique(p_id)
  
  
  
  curr_id <- -1         #initial values for loop
  p_count <- 1          #count profiles
  
  profs <- NULL         # initialise list of profile elevation data
  profsx <- NULL        # initialise list of x-profile data
  
  vecx <- NULL          # initalise temp vector with current profile's x-values
  vecy <- NULL          # initalise temp vector with current profile's elev. values
  
  p_id_unique <- NULL   # initialise vector with unique profile ids
  
  
  
  # extract profiles from read data set
  for (i in 1:length(p_id)) {
    
    
    # new profile reached?
    if (p_id[i] != curr_id) { 
      
      # don't do this at first run of loop
      if (i > 1) {
        
        # only one point in profile
        if (plength <= 1) {
          message('')
          message(paste('profile ', curr_id, ' contains only one point. Skipped.', sep=""))
          
        } else {      # save collected profile data
          
          #flip downslope profiles
          if (vecy[1] > vecy[length(vecy)]) {
            vecy <- rev(vecy)
          }
          
          # store previously accumulated profile data
          profs[[p_count]] <- vecy
          # convert stored x-values in real metric data and store
          profsx[[p_count]] <- vecx*dx
          
          # increase profile counter
          p_count <- p_count+1
          
        } # end save collected profile data
        
      } # end if first loop
      
      
      # reset temp variables to collect new profile
      vecx <- 0            # temporary storage for x-values of profiles
      vecy <- NULL         # temporary storage for y-values of profiles
      prev_x <- 0.0        # for storing x-value of previous sampling point
      curr_id <- p_id[i]
      plength <- 1
      p_id_unique[p_count] <- curr_id  # store new profile id
      
      
    } else {        # another point in current profile
      
      
      # gridcell spacing
      if ((px[i]-px[i-1])==1) {        
        prev_x <- prev_x+1       # single gridcell spacing
      } else {
        prev_x <- prev_x+sqrt(2) # diagonal gridcell spacing
      }
      
      # store x value of current point in current profile
      vecx <- c(vecx, prev_x)
      plength <- plength+1
      
      
    } # end another point in current profile
    
    
    # retrieve y value (elevation) of current point in current profile
    vecy <- c(vecy, elev[i])
    
    
  } # end extract profiles from data set
  
  
  
  
  if (plength <=1) {
    message('')
    message(paste('profile ', i, ' contains only one point. Skipped.', sep=""))
  } else {
    profs[[p_count]] <- vecy       # store last accumulated profile data
    profsx[[p_count]] <- vecx*dx   # convert stored x-values in real metric data and store
  }
  
  
  # remove not needed objects (at least the larger ones to save memory)
  rm(stats,elev,px)
  
  
  # profile meta data
  profpoints <- NULL
  profheights <- NULL
  proflengths <- NULL
  for (i in 1:length(profs)) {
    # save length of profile (in sample points)
    profpoints[i] <- length(profs[[i]])  
    # save relative height of profile in [m]
    profheights[i] <- max(profs[[i]]) - profs[[i]][1]  
    # save absolute length of profile
    proflengths[i] <- max(profsx[[i]])
  }
  
  
  # PLOT original profile
  if (make_plots) {
    pdf(paste(dir_out, "plots_prof_class/plots_prof_class.pdf", sep="/"))
    plot(1,1,type="n", xlim=c(0,max(unlist(profsx))), ylim=c(0,max(unlist(profs))),
         main="Original catenas", xlab="horizontal length [m]", ylab="elevation [m]")
    for (i in 1:length(profs)) {
      lines(profsx[[i]], profs[[i]])
    }
  }
  
  
  
  # PREPARE attribute loop and key-generation #
  # use the median of sampling points as the desired common length of profiles
  if (classify_type != 'load') {  
    com_length <- round(median(profpoints))
    if (com_length > max_com_length) com_length <- max_com_length
  }     # otherwise, the resolution from the saved clusters is used
  
  # allocate new matrix for storing resampled profiles
  # for each profile com_length elevation points, the profile length, the
  # profile height, and supplemental data is stored in one long vector
  profs_resampled_stored <- matrix(NA, nrow=length(profs), ncol=com_length+2+com_length*n_supp_data_columns)
  
  
  
  # START OF CLASS KEY GENERATION #
  attr_weights_class_original <- attr_weights_class
  iw <- 1
  
  # successive weighting vs. single run
  if (cf_mode == 'successive') {
    iw_max <- length(attr_weights_class) # successive weighting for each single attribute
  } else {
    iw_max <- 1  # cf_mode ='singlerun'
  }
  
  
  
  # classification loop through all attributes
  cidx_save <- NULL # initialise list to store classification results
  while (iw <= iw_max) {
    
    
    # SUCCESSIVE weighting for each single attribute
    if (cf_mode == 'successive') {
      emptymask <- vector("numeric", length=length(attr_weights_class_original))
      
      # first run > shape is considered
      if (iw==1) {
        maskw <- emptymask
        maskw[1] <- 1
      } else if (iw==2) { # second run > x/y dimension is considered
        maskw <- emptymask
        maskw[2] <- 1  
        maskw[3] <- attr_weights_class_original[3]
      } else if (iw>3) { # next runs > weights of all other parameters are set to zero, consideration of single attribute
        maskw <- emptymask
        maskw[iw] <- 1 
      }
      
      # modify original weights:
      attr_weights_class <- maskw
      
      # set nclass to specification in header file
      nclasses <- attr_weights_class_original[iw]
      
      # in case of erroneous input zero or only 1 class don't do classification, just append the result of classifying into 1 class
      if (nclasses==0 || nclasses==1) {
        
        cidx_save[[iw]] <- 1
        
        message('')
        message(paste("skipped '", attr_names[iw], "' (", iw-(iw>2), "/", length(attr_names)-1, ") because number of classes=1", sep=""))
        
        if (iw==2) {
          iw <- 4
        } else {
          iw <- iw+1 # alter index for attribute-loop
        }
        
        next
      }
      
      message('')
      message(paste("successive clustering loop, treating attribute '", attr_names[iw], "' (", iw-(iw>2), "/", length(attr_names)-1, ")", sep="")) 
    } # end cases of cf_mode 'successive'/'singlerun'
    
    
    
    
    # first run, do RESAMPLING and store resampled profiles
    if (iw==1) {
      
      # do resampling for all profiles
      for (i in 1:length(profs)) {
        
        # current relative location of sampling points on the scale [0:com_lengths-1]
        cur_x <- profsx[[i]]/proflengths[i]*(com_length-1)
        
        # resample to common length
        p_new <- approx(cur_x,profs[[i]],0:(com_length-1))$y
        # set foot of profile to zero elevation
        p_new <- p_new-p_new[1]
        
        # amplitude of profile will be scaled to 1
        d <- max(p_new[1:com_length]) - min(p_new[1:com_length])
        if(d==0) {
          message('')
          warning(paste('Warning: Profile ', p_id_unique[i], ' has no elevation gain (runs flat)', sep=""))
          p_new[1:com_length] <- 0
        } else {
          # normalize profile in elevation (ie. normally, top end at elevation = 1)
          p_new <- p_new / d
        }
        
        # append the dimension components, unweighted
        profs_resampled_stored[i,1:(com_length+2)] <- c(p_new, proflengths[i], profheights[i])
        
        # treat supp_data if present (resample, weigh and add to profile vector to be included in cluster analysis)
        if (n_suppl_attributes) {
          #retrieve all supplemental data for this profile
          p_supp <- supp_data[which(p_id==p_id_unique[i]),]
          
          # resample supplemental data to common resolution and store the results in a temporary vector
          p_supp_resampled_temp <- apply(p_supp,2,function(x) approx(cur_x, x, 0:(com_length-1))$y)
          
          profs_resampled_stored[i,(com_length+3):(com_length+2+com_length*n_supp_data_columns)] <- as.vector(p_supp_resampled_temp)
        }
        
      } # end resamling profiles
      
      # save memory
      rm(supp_data)
      
    } # all profile and supplemental data resampled
    
    
    
    
    # auxiliary vector for computing required array size for pre-allocation
    t_help <- rep(1,length(datacolumns))*com_length
    t_help[2:3] <- 1 # dimension attributes need only one value
    profs_resampled <- matrix(0, nrow=length(profs), ncol=sum(t_help*datacolumns*(attr_weights_class!=0)))
    
    
    
    
    # do WEIGHTING for all profiles according to current weighting scheme
    for (i in 1:length(profs)) {
      
      dest_column <- 1      # destination coulumn where an attribute is placed
      
      # ii proflengths, profheights eliminieren
      # weigh shape and the dimension components, weighted with specified weights attr_weights_class(2), attr_weights_class(3) multiplied by com_length to make the weighting independent of any com_length that was computed
      # put into data matrix only if the weighting factors are not zero
      if (attr_weights_class[1]) {
        profs_resampled[i,dest_column:(dest_column+com_length-1)] <- profs_resampled_stored[i,1:com_length]*attr_weights_class[1]
        dest_column <- dest_column+com_length
      }
      if (attr_weights_class[2]) {
        profs_resampled[i,dest_column] <- proflengths[i]*com_length*attr_weights_class[2]
        dest_column <- dest_column+1
      }
      if (attr_weights_class[3]) {
        profs_resampled[i,dest_column] <- profheights[i]*com_length*attr_weights_class[3]
        dest_column <- dest_column+1
      }
      
      
      #treat supp_data if present (resample, weigh and add to profile vector to be included in cluster analysis)
      if (n_suppl_attributes) {
        attr_start_column <- 1+com_length+2   #initial value for first loop
        
        # append all the supplemental components, weighted with specified weights
        for (j in 4:length(datacolumns)) {
          
          # skip attributes with no data columns
          if(datacolumns[j]==0) next
          
          new_columns <- datacolumns[j]*com_length-1
          
          # skip attributes weighted with 0
          if(attr_weights_class[j]==0) {
            attr_start_column <- attr_start_column+new_columns+1
            next              
          }
          
          attr_end_column <- attr_start_column+new_columns
          
          # Weigh the current supplemental attribute, divide by
          # number of fields (attr_end_column-attr_start_column+1) to
          # prevent multi-field attributes to get more relative weight
          profs_resampled[i,dest_column:(dest_column+new_columns)] <- profs_resampled_stored[i,attr_start_column:attr_end_column] * attr_weights_class[j] / (attr_end_column-attr_start_column+1)
          
          dest_column <- dest_column+new_columns
          
          # the next attribute starts one column further in prof_resampled_stored
          attr_start_column <- attr_end_column+1
          
        } # end weigh and append suppl data
        
        
      } else { 
        
        profs_resampled <- profs_resampled[i,1:com_length]
        
      } # end treat suppl data   
      
    } # end weighting
    
    
    
    
    # PLOT modified (resampled) profiles
    if (make_plots && (cf_mode=='singlerun') || iw==1) {
      profs_resampled_plot <- profs_resampled
      plot(1,1,type="n", xlim=c(0,com_length-1), ylim=c(0,1),
           main="catenas, resampled, reduced & normalized", xlab="catena point number", ylab="relative elevation")
      for (i in 1:nrow(profs_resampled_plot)) {
        lines(0:(com_length-1), profs_resampled_plot[i,])
      }
    }
    
    
    
    
    # CLUSTER-ANALYSIS
    cidx=array(NA,nrow(profs_resampled)) #cluster membership
    if (classify_type=='load') {
      # classification based on loaded classes, TODO
      #[cidx,sumd,dists]=cluster_supervised(profs_resampled_stored,nclasses,cluster_centers_weighted);
    } else {
      # unsupervised classification
      unique_profs=unique(x=profs_resampled)
      if (nrow(unique_profs) <= nclasses) #not enough distinct profiles for this attribute 
      {
        kmeans_out=NULL #disables later plots
        for(jj in 1:nrow(profs_resampled)) 
          cidx[jj]=which(apply(unique_profs, MARGIN=1, FUN=identical, profs_resampled[jj,]))
        cmeans2 <- unique_profs # matrix of cluster centers
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
    
    
    message('')
    message(paste('profile clustering: fitting index_c = ', round(sqrt(sum(sumd^2)),2), sep=""))
    
    
    if (length(unique(cidx)) < nclasses) {
      message('')
      message(paste(nclasses-length(unique(cidx)), ' empty clusters produced.', sep=""))
    } else if (make_plots) {
      # silhouette plot, doesn't work with empty clusters
      if (!is.null(kmeans_out))
      {
        dists <- daisy(profs_resampled) # compute pairwise distances, TODO: see warnings
        plot(silhouette(kmeans_out$cluster, dists^2), main=attr_names[iw]) # plot silhouette
      }  
    }
    
    
    
    
    
    # PLOT classified catenas
    # originals
    if (make_plots) {
      plot(1,1,type="n", xlim=c(0,max(unlist(profsx))), ylim=c(0,max(unlist(profs))),
           main=paste("Original catenas\nclassified according ", attr_names[iw], sep=""), xlab="horizontal length [m]", ylab="elevation [m]")
      for (i in 1:length(profs)) {
        lines(profsx[[i]], profs[[i]], col=cidx[i])
      }
    }
    
    # modified
    if (make_plots) {
      plot(1,1,type="n", xlim=c(0,com_length-1), ylim=c(0,1), main=paste("catenas, resampled, reduced & normalized\nclassified according ", attr_names[iw], sep=""), 
           xlab="catena point number", ylab="relative elevation")
      for (i in 1:nrow(profs_resampled)) {
        lines(0:(com_length-1), profs_resampled_plot[i,], col=cidx[i])
      }
    }
    
    
    # alter index for attribute-loop
    if (iw==2) {
      iw <- 4           # because x and y dimension are treated together
    } else {
      iw <- iw+1 
    }
    
    
  } # end classification loop through all attributes
  
  
  
  
  # complete key generation (successive mode only)
  # successive weighting mode: all prior classifications will be merged into one by generating unique key 
  if (cf_mode == 'successive') {
    cidx_save[[iw_max+1]] <- 0
    
    for (iz in 1:iw_max) {
      
      # eliminate empty iw=3 (horiz. & vertical extent treated together) by jumping to next entry
      if (iz==3) next
      
      # generate key from previous classifications
      cidx_save[[iw_max+1]] <- cidx_save[[iw_max+1]]*10 + cidx_save[[iz]]
    }
    
    nclasses <- length(unique(cidx_save[[iw_max+1]]))
    attr_weights_class_original[iw+1] <- nclasses
    
    # "pretend" this is the actual classification
    cidx <- cidx_save[[iw_max+1]]
    
    # quick and dirty computation of distance matrix to allow the rest of the script to be run without problems
    dists <- matrix(1, nrow=length(profs), ncol=nclasses) 
    unique_classes <- unique(cidx)
    for (ii in 1:nclasses) {
      # find all profiles belonging to current class
      class_i <- which(cidx==unique_classes[ii])
      # set their distance to the cluster centre to 0
      dists[class_i,ii] <- 0
    }       
  } # end key generation
  
  #-----------end of class key generation
  
  
  
  
  # calculate MEAN CATENA of each class
  unique_classes <- unique(cidx)
  
  if (length(unique_classes)!=nclasses) {
    warning(paste('Number of generated classes (', length(unique_classes), ') is not not as expected (', nclasses, '). Please check what happended.'))
    nclasses <- length(unique_classes)
  }
  
  mean_intc <- rep(0,nclasses) # mean intercept, TODO: always zero?!
  mean_prof <- matrix(NA, nrow=nclasses, ncol=ncol(profs_resampled_stored)) # mean shape of every class
  class_repr <- matrix(NA, nrow=nclasses, ncol=2) # min. distance of class i to centroid and resp. ID
  for (i in 1:nclasses) {
    
    # find all profiles belonging to current class
    class_i <- which(cidx==unique_classes[i])
    
    # empty cluster
    if (!any(class_i)) {
      message('')
      message(paste('cluster ', i, ' is empty.', sep=""))
      next
    }
    
    
    # calculate mean shape, dimension
    mean_prof[i,] <- apply(matrix(profs_resampled_stored[class_i,], ncol=ncol(profs_resampled_stored)),2,mean)
    
    
    # calculate mean intercept (based on unreduced profiles), TODO: isn't this always zero by definition?
    for (j in 1:length(class_i)) {
      mean_intc[i] <- mean_intc[i] + profs[[class_i[j]]][1]
    }
    mean_intc[i] <- mean_intc[i]/length(class_i)
    
    
    # find closest catena (=most representative) to each class centre
    dists_class_i <- dists[class_i,i]        # retrieve distances of catenas of class i to centroid of class i
    class_repr[i,2] <- min(dists_class_i)   # find minimum distance of class i
    j <- min(which(dists_class_i == min(dists_class_i)))
    class_repr[i,1] <- class_i[j]                 # store internal id of closest catena
    
    # TODO: what is this good for?!
    Y <- sort(dists_class_i)
    ix <- sort(dists_class_i, index.return=T)$ix
    if (cf_mode=='singlerun') {
      message('')
      message(paste('three closest catenas to centre of class ', i, ' (ext id): ', p_id_unique[class_i[ix[1:min(3,length(ix))]]], sep=""))
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
    write(file=paste(dir_out,recl_lu,sep="/"), append=ifelse(i==1,FALSE,TRUE), x=paste(p_id_unique[class_i], "=", unique_classes[i], sep=""))
    
    # TODO: sql class file -> is that needed?
    
  } # end clalc mean catena of each class
  
  # in reclass files: all other (unclassified) profiles are assigned nodata
  write(file=paste(dir_out,recl_lu,sep="/"), append=T, x="* = NULL")
  
  
  
  
  #---------prepare file output luoutfile
  # prepare header line for output file
  dumstr <- paste('LU-ID', tab, 'closest_catena', tab, 'closest_dist', tab, 'x_length', tab, 'y_length', sep="")
  
  # write header fields for calculated TC-limits, minimisation of variance
  for (i in 1:(ntc-1)) {
    dumstr <- paste(dumstr, tab, 'lim_var', i, sep="")
  }
  
  # write header fields for calculated TC-limits, cluster analysis
  for (i in 1:(ntc-1)) {
    dumstr <- paste(dumstr, tab, 'lim_clu', i, sep="")
  }
  
  # write header for all attributes
  for (i in 3:length(datacolumns)) {
    # write all fields of this attribute for this catena point
    for (k in 1:datacolumns[i]) {
      # write this attribute for all catena points
      for (j in 1:com_length) {
        dumstr <- paste(dumstr, tab, attr_names[i], '_p', j, sep="")
        
        # if this is a multi-field attribute...
        if (datacolumns[i]>1) dumstr <- paste(dumstr, '_c', k, sep="")  # denote field numbering in header
      }
    }
  }
  
  # write header string to output file
  write(file=paste(dir_out,luoutfile,sep="/"), append=F, x=dumstr)
  #---------end prepare file output
  
  
  
  #---------prepare file output tc.dat
  dumstr <- 'TC'
  # write header for all attributes
  for (i in 4:length(datacolumns)) {
    # write all fields of this attribute for this catena point
    for (k in 1:datacolumns[i]) {
      dumstr <- paste(dumstr, tab, attr_names[i], sep="")
      
      # if this is a multi-field attribute...
      if (datacolumns[i]>1) dumstr <- paste(dumstr, '_c', k, sep="")  # denote field numbering in header
    }
  }
  
  # write header string to output file tc.dat
  write(file=paste(dir_out,tcoutfile,sep="/"), append=F, x=dumstr)
  #---------end prepare file output tc.dat
  
  
  
  #---------prepare file output tc_contains_svc.dat
  svc_col_index <- 0
  # find index of column containing svcs
  for (j in 1:length(attr_names)) {
    if (attr_names[j]=='svc' | attr_names[j]=='svc_new' | attr_names[j]=='svc_1957') {
      svc_col_index <- j
      break
    }
  }
  
  if (svc_col_index) {
    #write header string to output file tc_contains_svc.dat
    write(file=paste(dir_out,tccontainssvcoutfile,sep="/"), append=F, x='tc_id\tsvc_id\tfraction')
    
    svc_recl_file <- paste('reclass_', attr_names[svc_col_index], '.txt', sep="")
    
    # check existence of file containing original SVC-IDs
    if (file.exists(svc_recl_file)) {
      svc_recl_dat <- read.table(svc_recl_file, header=T)
      mod_svc_ids <- svc_recl_dat$new_id
      org_svc_ids <- svc_recl_dat$original_id
    } else {
      # no reclass file found - don't change IDs
      mod_svc_ids <- 1:datacolumns[svc_col_index]
      org_svc_ids <- mod_svc_ids
    }
  }
  #---------end prepare file output tc_contains_svc.dat
  
  
  
  # decomposition into TCs
  cluster_centers <- matrix(NA, nrow=nclasses, ncol=ncol(profs_resampled_stored)) # initialise matrix for cluster centers for each class
  lu_contains_tc <- NULL
  # decompose / partition the mean profile of each class
  for (i in 1:nclasses) {
    message('')
    message(paste('partitioning class ', i, ' of ', nclasses, sep=""))
    
    # find all profiles belonging to current class
    class_i <- which(cidx==unique_classes[i])
    
    # empty cluster
    if (!any(class_i)) {
      cluster_centers[i,] <- NaN
      next
    }
    
    # get the ID of the currently treated LU
    curr_lu_key <- cidx[class_i[1]]
    
    
    # if supplemental data exists, calculate average
    if (n_suppl_attributes) {
      mean_supp_data_t <- apply(matrix(profs_resampled_stored[class_i,(com_length+3):ncol(profs_resampled_stored)], nrow=length(class_i)),2, mean)
      mean_supp_data <- matrix(mean_supp_data_t, ncol=com_length, nrow=n_supp_data_columns, byrow=T)
    } else {
      mean_supp_data <- 0      # no supplemental data present
    }
    
    
    # compute x-dimension for current mean profile
    xvec <- c(0:(com_length-1))/(com_length-1)*mean_prof[i,(com_length+1)]
    
    
    # cluster centers can be saved for future use (supervised classification)
    if (classify_type=='save') {
      tmp_v=NULL
      
      # store data for all supplemental attributes
      for (ii in 4:length(datacolumns)) {
        # for retrieval of this attribute
        attr_start_column <- 1+sum(datacolumns[4:(ii-1)])
        attr_end_column <- attr_start_column+datacolumns[ii]-1
        
        # for all points in profile
        for (jj in 1:com_length) {
          tmp_v <- c(tmp_v, mean_supp_data[attr_start_column:attr_end_column,jj])
        }
      }  
      
      cluster_centers[i,] <- c(mean_prof[i,1:(com_length+2)], tmp_v) 
    } # end if classify_type==save
    
    
    
    
    
    # TERRAIN COMPONENTS
    # get exact horizontal resolution for exact plotting
    dx <- xvec[com_length]/(com_length-1)
    if (attr_weights_partition[1]==1) {
      # only one TC per LU has to be produced - not much work to be done
      lim_var=NULL
      lim_clu=NULL
    } else {
      
      # decompose profile into terrain components
      #    [lim_var,lim_clu] = tc_decomp(mean_prof(i,1:com_length), mean_supp_data, datacolumns, attr_weights_partition, xvec,monocrome,plot_approx_ts);
      #    tc_decomp(prof, supp_data, datacolumns, attr_weights_partition, xvec, monocrome, plot_approx_ts)
      
      
      # plot orig
      if (make_plots) {
        plot(xvec, mean_prof[i,1:com_length], type="l", xlab='horizontal length', ylab='relative elevation gain',
             main=paste("partitioning class ", i, sep=""))
      }
      
      # fill hollows/sinks
      for (ii in 2:com_length) {
        mean_prof[i,1:com_length][ii] <- max(mean_prof[i,1:com_length][ii-1], mean_prof[i,1:com_length][ii])
      }
      
      # plot filled
      if (make_plots) {
        points(xvec, mean_prof[i,1:com_length], pch=1)
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
      # TODO: to be tested
      if (exists("supp_data") && sum(attr_weights_partition[4:length(attr_weights_partition)])) {
        supp_data_weighted <- NULL
        # weigh supplemental information according to weighting factors given
        for (jj in 4:length(datacolumns)) {
          # if an attribute is not be weighted with 0, we can as well skip it
          if (attr_weights_partition[jj]==0) next
          
          attr_start_column <- sum(datacolumns[4:(jj-1)])+1
          attr_end_column <- attr_start_column+datacolumns[jj]-1
          
          supp_data_weighted <- c(supp_data_weighted, supp_data[attr_start_column:attr_end_column,]*attr_weights_partition[jj]/(attr_end_column-attr_start_column+1))
        }  
        # data that is given to partitioning algorithm
        pdata <- c(prof_slopes, supp_data_weighted)
      } else {
        # only the slope data that is given to partitioning algorithm
        supp_data_weighted <- 0
        pdata <- prof_slopes
      }
      
      
      
      # decomposition using standard deviation
      # quality and  best limits of partitioning
      b_part <- best_partition(pdata, attr_weights_partition[1])
      qual <- b_part[[1]]
      best_limits <- b_part[[2]]
      
      message('')
      message(paste('partition by min variance: ', paste(best_limits, collapse=" "), 
                    '; fitting index_v = ', qual, sep=""))
      
      
      
      # plot parameterized slope
      # only for drawing - from beginning till end
      best_limits_t <- c(1, best_limits, length(mean_prof[i,1:com_length]))
      
      if (make_plots) {   
        lines(xvec[best_limits_t], mean_prof[i,1:com_length][best_limits_t], col="red")
        
        # plot limits using vertical lines
        for (ii in 1:length(best_limits)) {       
          lines(c((best_limits[ii]-1)*dx, (best_limits[ii]-1)*dx), c(0, mean_prof[i,1:com_length][best_limits[ii]]), lty=2, col="red")
        }
      }
      
      
      
      
      #decomposition using cluster analysis - deactivated
      best_limits_c <- NULL
      best_limits_c[1:(attr_weights_partition[1]-1)] <- 0
      if (FALSE){ 
        clusterdata <- pdata # matrix containing data that is fed to cluster analysis
        dummy_dim_appended <- 0 # flag if dummy dimension has already been appended
        
        # modify classification data by adding "location" dimensions [1..lengths]
        # until the classification produces spatially continuous clusters
        for (j in 1:(length(supp_data_weighted)*10+2)) {
          
          # cluster analysis
          kmeans_out <- kmeans(clusterdata, centers=attr_weights_partition[1], nstart=5)
          cidxtc <- kmeans_out$cluster    # cluster number for each point
          cmeanstc <- kmeans_out$centers # matrix of cluster centers
          fitc <- kmeans_out$withinss   # within-cluster sum of squares, one per cluster
          
          best_limits_c <- NULL
          continuous_clusters <- 1
          
          # check if clusters are continuous or distributed
          for (k in 1:attr_weights_partition[1]){
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
          message('')
          message('partitioning using cluster analysis failed.')  
          best_limits_c[1:(attr_weights_partition[1]-1)] <- 0
        } else {
          # sort limits to ascending order
          best_limits_c <- sort(best_limits_c)
          message('')
          message(paste('partition by clustering  : ', paste(best_limits_c, collapse=" "), '; fitting index_c = ', sqrt(sum((fitc)^2)), sep=""))
          
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
      
      
      # plot legend
      if (make_plots) { 
        legend("topleft", c("orig. toposequence", "filled profile", "approx. (min var)", "TC boundary (var)",
                            "approx. (cluster anal.)", "TC boundary (cluster anal.)"), lty=c(1,NA,1,2,1,2),
               pch=c(NA,1,NA,NA,NA,NA), col=c("black", "black", "red","red","green","green"))
      }
      
      
      # save values
      lim_var <- best_limits
      lim_clu <- best_limits_c
      
    } # end else attr_weights_partition[1]==1
    
    
    
    #----------file output lu.dat
    lu_out_dat <- NULL
    # write LU-ID, closest catena and its distance
    lu_out_dat <- c(lu_out_dat, curr_lu_key, p_id_unique[class_repr[i,1]], class_repr[i,2], round(mean_prof[i,(com_length+1):(com_length+2)],1))
    # write limits of TC-decomposition
    lu_out_dat <- c(lu_out_dat, lim_var, lim_clu)
    # write elevation data
    lu_out_dat <- c(lu_out_dat, round(mean_prof[i,1:com_length],2))
    # write for all catena points
    for (j in 1:nrow(mean_supp_data)) {
      # write data string to output file
      lu_out_dat <- c(lu_out_dat, round(mean_supp_data[j,],2))
    }
    
    # write into file
    write(file=paste(dir_out,luoutfile,sep="/"), append=T, ncolumns=length(lu_out_dat), x=paste(lu_out_dat, sep=tab))
    #----------end file output
    
    
    
    
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
      for (ii in 4:length(datacolumns)) {
        # write all fields of this attribute for this catena point
        for (k in 1:datacolumns[ii]) {
          tmp_v <- round(mean(mean_supp_data[treated_attribs+k,from_point:till_point]),5)
          dumstr <- paste(dumstr, paste(tmp_v,collapse=tab),sep=tab)
          
          #---------output tc_contains_svc.dat
          # if this is the svc column...
          if (ii==svc_col_index) {
            # print out svc-fration, if greater than 0
            if (tmp_v) {
              # fraction of svc in current tc to output file tc_contains_svc.dat
              write(file=paste(dir_out,tccontainssvcoutfile,sep="/"), append=T, x=paste(tc_id, org_svc_ids[k], tmp_v, sep=tab))
            }
          }
          #---------end output tc_contains_svc.dat
        }
        # increase counter for attributes already treated
        treated_attribs <- treated_attribs+k
      }
      # write constitution of this TC (tc.dat)
      write(file=paste(dir_out,tcoutfile,sep="/"), append=T, x=paste(tc_id, paste(dumstr,collapse=tab), sep=tab))
    } # end loop over ntc
    
    
    tc_ids <- c(1:ntc)+(lu_id-1)*rep(1,ntc)*ntc
    
    lu_contains_tc <- rbind(lu_contains_tc, cbind(curr_lu_key*rep(1,ntc), tc_ids, round(frac_tc,5), 1:ntc, slope_tc))
    #----------end file TC-output
    
  } # end loop over nclasses for TC decomposition
  
  
  
  # close plot output device
  dev.off()
  
  
  
  
  # save remaining classification results
  # cluster centers can be saved for future use (supervised classification, single run only)
  if (classify_type=='save'){
    save('cluster_centers','com_length','datacolumns','attr_names',file=paste(dir_out,saved_clusters,sep="/"));
    message('')
    message(paste('saved cluster centers to ', dir_out, "/", saved_clusters, sep=""))
  }
  
  
  # write LU contains TC files
  colnames(lu_contains_tc) <- c("lu_id", "tc_id", "fraction", "position", "slope")
  write.table(lu_contains_tc[,-5], file=paste(dir_out,lucontainstcoutfile,sep="/"), quote=F, col.names=T, row.names=F, sep=tab)
  
  tc_dat <- cbind(lu_contains_tc[,2], rep("[na]",nrow(lu_contains_tc)), round(lu_contains_tc[,5],2), rep(0,nrow(lu_contains_tc)))
  colnames(tc_dat) <- c("pid", "descr", "slope", "frac_rocky")
  write.table(tc_dat, file=paste(dir_out,terraincomponentsoutfile,sep="/"), quote=F, col.names=T, row.names=F, sep=tab)
  
  
  
  message('')
  message('DONE!')
  message('')
  
} # EOF


### INTERNAL FUNCTIONS USED BY prof_class ###

# delivers partition quality: sum of weighted variances of subdivisions
get_part_quality <- function(data_mat, lim) {
  
  # get number of points contained in this data (sub-)set
  n_points_in_data_mat <- length(data_mat)
  
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
    qual <- qual + wf*var(data_mat[part_start:part_end]) 
  }
  
  return(qual)
} # EOF




best_partition <- function(data_mat, no_part) {
  
  n_points_in_data_mat<- length(data_mat)
  
  # partition the vector in 2 only subdivisions
  if (no_part==2) {
    # initial value for loop
    best_lim_qual <- Inf
    startat <- 1
    
    for (jj in startat:n_points_in_data_mat) {
      # get quality of this subdivision
      lim_qual <- get_part_quality(data_mat,jj)
      
      # this subdivision is better than the previous best
      if (lim_qual < best_lim_qual) {
        best_lim_qual <- lim_qual    # set new best subdivision
        best_limit <- jj
      }    
    } # end loop over data_mat
    
    # return the best limitation found and its quality
    return(list(best_lim_qual, best_limit))  
    
  } else {
    
    # initial value for loop
    best_lim_qual <- Inf
    
    for (ii in 1:(n_points_in_data_mat-no_part-2)) {
      # get best partitioning inside the remaining part - the returned quality value is not used
      bp <- best_partition(data_mat[ii:n_points_in_data_mat], no_part-1)
      lim_qual <- bp[[1]]
      best_limits <- bp[[2]]
      
      # with a limit at ii, the rest is best partitioned as contained in best_limits
      this_i_best_limits <- c(ii, best_limits+ii-1)
      
      # get the overall quality of this partitioning
      lim_qual <- get_part_quality(data_mat, this_i_best_limits)
      
      # this subdivision is better than the previous best
      if (lim_qual < best_lim_qual) {
        # set new best subdivision
        best_lim_qual <- lim_qual
        bl <- this_i_best_limits
      }
      
    }
    # return the best limitation found and its quality
    return(list(best_lim_qual, bl))
    
  } # end if no_part==2
} # EOF
