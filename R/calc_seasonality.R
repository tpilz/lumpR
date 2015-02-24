#' Calculate time series based on seasonality
#' 
#' Function calculates a daily time series of a (vegetation) variable based on 
#' information about the seasonality of that variable.
#' 
#' @param rainy_season \code{data.frame} giving start and end dates of the rainy season.
#'      Output of function \code{\link[LUMP]{rainy_season}}. See doc for information on
#'      data structure.
#'      
#' @param seasonality \code{data.frame} or \code{matrix} giving id value (corresponding
#'      to first column of \code{rainy_season}) and the respective 4 node values of the
#'      target variable: First value: Start day of year (DOY) of rainy season. 2: DOY 
#'      when climax of vegetation is reached. 3: End DOY of rainy season (begin of
#'      vegetation degradation). 4: End of main phase of vegetation degradation.
#'
#' @param timezone Timezone the date-time values of the output object shall refer to.
#'      If nothing is specified your system's timezone is used.
#'      
#' @return Object of class \code{\link[xts]{xts}} with daily values of the target
#'      variable. Columns refer to the stations specified in the first column of 
#'      the input object \code{rainy_season}.
#'      
#' @references Function uses subroutine in FORTRAN 90 code extracted from the hydrological 
#'      model WASA. General model description (information on vegetation seasonality 
#'      see chapter 4.3.3):
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

calc_seasonality <- function(
  # INPUT #
  rainy_season,
  seasonality,
  timezone=""
) {


#rainy_season as integer matrix
rainy_season_mat <- apply(rainy_season, 2, as.integer)

# loop over different stations
start_date <- strftime(paste0(head(rainy_season$year,n=1), "-01-01 00:00:00"), format="%Y-%m-%d %H:%M:%S")
end_date <- strftime(paste0(tail(rainy_season$year,n=1), "-12-31 00:00:00"), format="%Y-%m-%d %H:%M:%S")
output <- xts(NULL, seq.POSIXt(as.POSIXct(start_date, tz=timezone), as.POSIXct(end_date, tz=timezone), by="day"))                   

for (s in 1:nrow(seasonality)) {
  
  sub_t <- seasonality[s,1]
  # get relevant rows in rainy_season
  rows <- which(rainy_season$sub_id == sub_t)
  dat_t <- rainy_season_mat[rows,]
  
  # loop over years
  out_t <- NULL
  for (y in unique(rainy_season$year)) {
    
    # apply function over all days of year y
    if (y %% 4 == 0) {
      days <- 366
    } else {
      days <- 365
    }
    f_out <- lapply(1:days, function(x,id=sub_t,year=y,rs=dat_t,sn=seasonality[s,c(2:5)],out=as.double(-99.9)) {
                      .Fortran("calc_seasonality2",
                           # INPUT #
                           subbas_id=as.integer(id), year=as.integer(year), julian_day=as.integer(x),
                           seasonality_in=as.integer(rs), nv=as.integer(length(rs)),
                           support_values=as.double(sn),
                           # OUTPUT #
                           seasonality_out=out)$seasonality_out
    })
    
    # merge with output object
    out_t <- c(out_t,unlist(f_out))
  } # loop over years
  
  # merge with output object
  output <- merge.xts(output,out_t)
  colnames(output)[s] <- paste0("id_", sub_t)

} # loop over stations

return(output)

} # EOF
