#internal functions
check_raster <- function(map, argument_name="") { #check existence of raster map
  cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
  if (!grepl(map, pattern = "@"))
    map = paste0(map,"@", cur_mapset)  #add mapset name, unless already given. Otherwise, these checks may fall back on PERMANENT yielding true, but read_RAST will fail later

  cmd_out <-suppressWarnings(execGRASS("r.info", map=map, intern = T, ignore.stderr = TRUE))
  
  if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")==1)
    stop(paste0("Raster map '", map, "' not found", ifelse(argument_name=="", ".", paste0(" (argument '", argument_name,"'). If the map is in another mapset, add '@othermapset' to the name."))))
}

check_vector <- function(map, argument_name="") { #check existence of vector map
  cmd_out=suppressWarnings(execGRASS("v.info", map=map, intern = T, ignore.stderr = TRUE))
  
  if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")==1)
    stop(paste0("Vector map '", map, "' not found", ifelse(argument_name=="", ".", paste0(" (argument '", argument_name,"'). If the map is in another mapset, add '@othermapset' to the name."))))
}

read_raster <- function(raster_name) { 
  #read raster from GRASS-location
  #if raster exist both in local mapset and PERMANENT, force the reading of the local version, otherwise read_RAST fails
  cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
  # mapset name in R (meta symbols replaced by ".")
  cur_mapset_r = gsub("[-+*/@.?!]", ".", cur_mapset)
  if (!grepl(raster_name, pattern = "@")) #expand raster name because of bug in read_RAST 
  {  
    raster_name = paste0(raster_name,"@", cur_mapset) #add mapset name, unless already given. Otherwise, strange errors may occur when the same raster exists in PERMANENT
    name_expanded = TRUE #indicate that 
  }  else name_expanded = FALSE
  tmp <- read_RAST(raster_name)
  tmp <- raster(tmp)
  if (name_expanded) #"de-expand" name, if expanded before
    tmp@data@names = sub(x = tmp@data@names, pattern = paste0("\\.", cur_mapset_r), repl="")
  return(tmp)
}

clean_temp_dir <- function(file_name) {
# MS Windows only: clean temporary files that otherwise can obstruct subseqent import/export operations  
  if(.Platform$OS.type == "windows") {
    dir_del <- dirname(execGRASS("g.tempfile", pid=1, intern=TRUE, ignore.stderr=T))
    files_del <- grep(substr(file_name, 1, 8), dir(dir_del), value = T)
    if (length(files_del)>0)
      file.remove(paste(dir_del, files_del, sep="/"), showWarnings=FALSE)
  }
}

# calculation of reservoir volume / area by empirical relationship of Molle (1989) h[m]; A[m2]; V[m3]
molle_v <- function(alpha, k, A) k* (A/(alpha*k))^(alpha/(alpha-1)) 
molle_a <- function(alpha, k, V) (V/k * (alpha * k)^(alpha/(alpha-1)))^((alpha-1)/alpha) #cas
molle_h <- function(alpha, k, A) exp(log(A/alpha/k)/(alpha-1))




# Addition for subbas.R (calc_subbas)
# Modification in function snapPointsToLines
# Fixes problem, that attributes of SpatialPointsDataFrame "drain_points"
# are not passed when snapping drain_points (subbas.R, Line 351)
# Source: https://stackoverflow.com/questions/60945115/snappointstolines-cant-keep-attributes-in-r

snapPointsToLines1 <-  function (points, lines, maxDist = NA, withAttrs = TRUE, idField = NA) 
{
  if (rgeosStatus()) {
    if (!requireNamespace("rgeos", quietly = TRUE)) 
      stop("package rgeos required for snapPointsToLines")
  }
  else stop("rgeos not installed")
  if (is(points, "SpatialPointsDataFrame")==FALSE && missing(withAttrs)) 
    withAttrs = FALSE
  if (is(points, "SpatialPointsDataFrame")==FALSE && withAttrs == TRUE) 
    stop("A SpatialPointsDataFrame object is needed! Please set withAttrs as FALSE.")
  d = rgeos::gDistance(points, lines, byid = TRUE)
  if (!is.na(maxDist)) {
    distToLine <- apply(d, 2, min, na.rm = TRUE)
    validPoints <- distToLine <= maxDist
    distToPoint <- apply(d, 1, min, na.rm = TRUE)
    validLines <- distToPoint <= maxDist
    points <- points[validPoints, ]
    lines = lines[validLines, ]
    d = d[validLines, validPoints, drop = FALSE]
    distToLine <- distToLine[validPoints]
    if (!any(validPoints)) {
      if (is.na(idField)) {
        idCol = character(0)
      }
      else {
        idCol = lines@data[, idField][0]
      }
      newCols = data.frame(nearest_line_id = idCol, snap_dist = numeric(0))
      if (withAttrs) 
        df <- cbind(points@data, newCols)
      else df <- newCols
      res <- SpatialPointsDataFrame(points, data = df, 
                                    proj4string = CRS(proj4string(points)), match.ID = FALSE)
      return(res)
    }
  }
  else {
    distToLine = apply(d, 2, min, na.rm = TRUE)
  }
  nearest_line_index = apply(d, 2, which.min)
  coordsLines = coordinates(lines)
  coordsPoints = coordinates(points)
  mNewCoords = vapply(1:length(points), function(x) nearestPointOnLine(coordsLines[[nearest_line_index[x]]][[1]], 
                                                                       coordsPoints[x, ]), FUN.VALUE = c(0, 0))
  if (!is.na(idField)) {
    nearest_line_id = lines@data[, idField][nearest_line_index]
  }
  else {
    nearest_line_id = sapply(slot(lines, "lines"), 
                             function(i) slot(i, "ID"))[nearest_line_index]
  }
  if (withAttrs) 
    df = cbind(points@data, data.frame(nearest_line_id, snap_dist = distToLine))
  else df = data.frame(nearest_line_id, snap_dist = distToLine, 
                       row.names = names(nearest_line_index))
  SpatialPointsDataFrame(coords = t(mNewCoords), data = df, 
                         proj4string = CRS(proj4string(points)))
}
