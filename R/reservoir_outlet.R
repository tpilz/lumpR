#' Determine outlet coordinates of reservoirs
#' 
#' Takes a flow accumulation raster (or DEM for computation) and reservoir vector
#' file from a GRASS location and estimates the outlet coordinates of each reservoir
#' polygon.
#' 
#' @param flowacc Flow accumulation raster in GRASS location used for determination
#'      of reservoir outlets (= highest accumulation value). Set to \code{NULL} if
#'      it should be calculated based on a DEM. Default: \code{NULL}.
#' 
#' @param dem Digital elevation model in GRASS location used for calculation of
#'      flow accumulation if no flow accumulation raster is given. Default: \code{NULL}.
#'  
#' @param res_vct Reservoir vector file in GRASS location. For each polygon coordinates
#'      of the outlet (cell with highest flow accumulation) are determined. Needs
#'      at least the column \code{name} as reservoir identifier.
#'      
#' @param outlets_vect Output: Name of vector file of outlet locations to be exported
#'      to GRASS location.
#'      
#' @return \code{SpatialPoints} object containing outlet position for each reservoir
#'      polygon and vector file \code{outlets_vect} exported to GRASS location.
#'      
#' @note Prepare GRASS location and necessary files in advance and start GRASS
#'      session in R using \code{\link[spgrass6]{initGRASS}}. Location should not
#'      contain any maps ending on *_t as these will be removed by calling the
#'      function to remove temporary maps.
#'      
#'      Check the results by investigationg vector file with outlet points written
#'      to GRASS location.
#'
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}
#' 
#' @export
reservoir_outlet <- function(
  ### INPUT ###
  flowacc = NULL,
  dem = NULL,
  res_vct,
  
  ### OUTPUT ###
  outlets_vect
  
) {
  
  tryCatch({
    
    # remove mask if any
    execGRASS("r.mask", flags=c("r"))
    
    # GRASS calculation of flow accumulation #
    if (is.null(flowacc) & is.null(dem))
      stop("'flowacc' and 'dem' are NULL. One of it must be specified (non-NULL)!")
    
    if (is.null(flowacc) & !is.null(dem)) {
      flowacc <- "accum_t"
      execGRASS("r.watershed", elevation=dem, accumulation=flowacc)
    } else {
      stop("Both 'flowacc' and 'dem' are specified (non-NULL). Use only one of them!")
    }
    
    
    # load reservoirs from GRASS
    res_polygon <- readVECT6(res_vct)
    # load accumulation values from GRASS
    accum_rast <- raster(readRAST6(flowacc))
    
    
    # identify reservoir outlets #
    coords_out <- NULL
    for (r in unique(res_polygon@data$cat)) {
      
      # mask accum_rast by reservoir
      res_rows <- which(res_polygon@data$cat == r)
      accum_res <- mask(accum_rast, res_polygon[res_rows,])
      
      # get coordinates of point with highest accumulation value = point of reservoir outlet
      vals <- getValues(accum_res)
      
      if (!any(is.finite(vals)))
        stop(paste0("No finite flow accumulation values within reservoir ('cat') ", r, ". 'flowacc' (or 'dem') and 'res_vct' may not fully overlap?!"))
      
      vals <- abs(vals)
      max_pos <- which(vals == max(vals, na.rm=T))
      coords_out$name <- c(coords_out$name, as.character(res_polygon@data$name[res_rows]))
      coord_t <- xyFromCell(accum_res, max_pos)
      coords_out$x <- c(coords_out$x, coord_t[,"x"])
      coords_out$y <- c(coords_out$y, coord_t[,"y"])
    }
    
    # output matrix as spatial object
    coords_out <- data.frame(coords_out)
    coordinates(coords_out) <- c("x", "y")
    projection(coords_out) <- getLocationProj()
    
    # export points to GRASS
    writeVECT6(coords_out, outlets_vect)
    
    # remove temporary maps
    execGRASS("g.mremove", rast="*_t", flags=c("f"))
    
    
    # return spatial object
    return(coords_out)
    
  
  # exception handling
  }, error = function(e) {
    execGRASS("g.mremove", rast=paste0("*_t,"), flags=c("f"))
    stop(paste(e))  
  })
    
} # EOF
