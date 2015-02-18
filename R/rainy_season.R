#' Determine start and end date of the rainy season
#' 
#' Function calculates the yearly start and end dates of the rainy season from 
#' a precipitation time series based on a statistical approach as described by
#' Gerstengarbe & Werner (1999).
#' 
#' @param prec_ts Daily precipitation time series object of class \code{\link[zoo]{zoo}}.
#'      Data should be a numerical vector or a matrix with one named column per 
#'      station a time series is available for.
#'      
#' @param dry_season Day of year in the expected centre of dry season. This is the
#'      starting point of the stastical procedure to look for the next rainy season
#'      in the data set. Should be set approximately (+- 1 month) to the centre of
#'      the dry season.
#' @param nodata No-data value in the time series. Default: \code{NA}.
#' 
#' @return \code{data.frame} of rainy season start and end days in input format for
#'      the hydrological model WASA.
#'      
#'      Columns are: station ID (column names of input \code{zoo} object), year, start
#'      day of rainy season (negative values refer to the previous year), day of year
#'      the climax of the vegetation season is reached, end day of rainy season,
#'      last day of transition period from rainy to dry season (values greater than
#'      365/366 refer to the next year).
#'    
#' @note The first rainy season that can be identified by the function is the one
#'      that starts after the first dry season in the given time series. This means,
#'      for instance, that if the time series starts on Jan 1st 1978 and \code{dry_season}
#'      is set to 243 (conditions for NE Brazil), the first detectable rainy season
#'      is the one starting around Dec 78 to Feb 79. To get also the rainy season
#'      for year 1978 for this example, 365 daily values can be artifically inserted
#'      at the beginning of the time series, representing the hypothetical year 1977.
#'      Just copy the data of 1978.
#'      
#' @references Function uses the FORTRAN 77 code developed by \emph{Gerstengarbe et al.}.
#'      The algorithm is described in:
#'      
#'      Gerstengarbe & Werner (1999): Estimation of the beginning and end of recurrent
#'      events within a climate regime. \emph{Climate Research}, 11(2), 97-107.
#'      
#'      
#'      Function further used by A. Guentner to simulate vegetation dynamics within
#'      the hydrological model WASA:
#'      
#'      Guentner, A. (2002): Large-scale hydrological modelling in the semi-arid 
#'      North-East of Brazil. \emph{PIK Report 77}, Potsdam Institute for Climate
#'      Impact Research, Potsdam, Germany.
#'
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}
#'
#' @export
#' 
#' @useDynLib LUMP
#' 

rainy_season <- function(
  ### INPUT ###
  prec_ts,
  dry_season,
  nodata=NA
) {
  
  # check if time series is continuous
  if (length(seq(min(index(prec_ts)), max(index(prec_ts)), by="day")) != nrow(prec_ts))
    stop("Time series of precipitation input is not continuous, check the input!")
  
  # get start date of time series
  start_day <- as.integer(format(index(prec_ts)[1], format="%d"))
  start_month <- as.integer(format(index(prec_ts)[1], format="%m"))
  start_year <- as.integer(format(index(prec_ts)[1], format="%Y"))
  
  no_years <- length(unique(format(index(prec_ts), format="%Y")))
  no_stat <- ncol(prec_ts)
  
  # nodata must be -999.9 in fortran function
  if (is.na(nodata)) {
    prec_ts[which(is.na(prec_ts))] <- -999.9
    nodata <- -999.9
  } else {
    prec_ts[which(prec_ts==nodata)] <- -999.9
    nodata <- -999.9
  }
  
  
  # loop over stations/columns of data (Fortran function can handle only one time series at a time)
  out <- NULL
  for (s in 1:no_stat) {
    
    # get vector of time series
    vals <- prec_ts[,s]
    
    # call external fortran function
    season <- .Fortran("rainyseason", 
                    # INPUT #  
                    h=as.double(vals), 
                    day=start_day, month=start_month, year=start_year, 
                    n=as.integer(length(vals)), lw=as.integer(dry_season), 
                    mima=as.integer(1), xe=as.double(nodata), 
                    # OUTPUT#
                    outyear=as.integer(rep(-999, no_years)), 
                    outbegin=as.integer(rep(-999, no_years)), 
                    outend=as.integer(rep(-999, no_years)))
    
    # remove missing year if present
    na_t <- which(season$outyear==-999)
    season$outyear <- season$outyear[-na_t]
    season$outbegin <- season$outbegin[-na_t]
    season$outend <- season$outend[-na_t]
    
    # if outbegin > outend -> value must be 365/366 - outbegin (negative values used in WASA)
    greater <- which(season$outbegin > season$outend)
    doys <- rep(365, length(season$outyear))
    doys[which((season$outyear %% 4) == 0)] <- 366
    season$outbegin[greater] <- -1*(doys[greater] - season$outbegin[greater])
    
    # data.frame of fortran output
    out_t <- data.frame(sub_id=colnames(prec_ts)[s],
                        year=season$outyear,
                        begin_rainy=season$outbegin,
                        max_veg=season$outbegin +30,
                        end_rainy=season$outend,
                        end_trans=season$outend+30)
    
    # merge with output data.frame
    out <- rbind(out,out_t)
    
  } # loop over stations / columns in time series
  
  # re-order and return
  out <- out[order(out$year),]
  rownames(out) <- NULL
  return(out)
  
} # EOF
