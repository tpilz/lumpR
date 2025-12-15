#internal functions
test_grass = function() #test if connection to GRASS is working properly
{  
  #test if its working
  testGRASS = suppressWarnings(try(execGRASS("g.version", intern = TRUE), silent=TRUE)) #test if GRASS is responding
  if (class(testGRASS)=="try-error") stop("GRASS session could not be initialized. Check the output of the initGRASS command for errors.")
  
  stat = attr(testGRASS, "status")
  if (!is.null(stat) && stat != 0)
   stop(paste0("Error in connecting to GRASS. Please check name of location and mapset.\n ", testGRASS))
}


check_raster <- function(map, argument_name="", raiseerror=TRUE) { #check existence of raster map
  cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
  if (!grepl(map, pattern = "@"))
    map = paste0(map,"@", cur_mapset)  #add mapset name, unless already given. Otherwise, these checks may fall back on PERMANENT yielding true, but read_RAST will fail later

  cmd_out <-suppressWarnings(execGRASS("r.info", map=map, intern = T, ignore.stderr = TRUE))
  
  if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")==1)
  {
    if (raiseerror)
        stop(paste0("Raster map '", map, "' not found", ifelse(argument_name=="", ".", paste0(" (argument '", argument_name,"'). If the map is in another mapset, add '@othermapset' to the name."))))
      else
        return(FALSE)
  } else
  {
    if (raiseerror)
      return() #do nothing
    else
      return(TRUE)
  }  
  
  
}


check_vector <- function(map, argument_name="") { #check existence of vector map
  cmd_out=suppressWarnings(execGRASS("v.info", map=map, intern = T, ignore.stderr = TRUE))
  
  if (!is.null(attr(cmd_out, "status")) && attr(cmd_out, "status")==1)
    stop(paste0("Vector map '", map, "' not found", ifelse(argument_name=="", ".", paste0(" (argument '", argument_name,"'). If the map is in another mapset, add '@othermapset' to the name."))))
}

read_raster <- function(raster_name) { 
  #as of GRASS 8.3.1, rgrass0.4-1  read_RAST() on Windows does not seem to work in other than the active mapset
  # rGRASS 0.4-2 seems to solve this issue
  
  # #read raster from GRASS-location
  # #if raster exist both in local mapset and PERMANENT, force the reading of the local version, otherwise read_RAST fails
  # cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
  # # mapset name in R (meta symbols replaced by ".")
  # cur_mapset_r = gsub("[-+*/@.?!]", ".", cur_mapset)
  # if (!grepl(raster_name, pattern = "@")) #expand raster name because of bug in read_RAST 
  # {  
  #   raster_name = paste0(raster_name,"@", cur_mapset) #add mapset name, unless already given. Otherwise, strange errors may occur when the same raster exists in PERMANENT
  #   name_expanded = TRUE #indicate that 
  # }  else name_expanded = FALSE
  tmp <- read_RAST(raster_name)
  # tmp <- raster(tmp)
  # if (name_expanded) #"de-expand" name, if expanded before
  #   tmp@data@names = sub(x = tmp@data@names, pattern = paste0("\\.", cur_mapset_r), repl="")
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

cleanup = function(cleanup_pattern_ = NULL) {
  # restore original error handling routine
  olderror = try(get("olderror"), silent = TRUE)
  if (class(olderror) != "try-error") #no old error handler found
    options(error = olderror) else
    options(error = NULL)
  
  if (!is.null(cleanup_pattern_))
    cleanup_pattern = cleanup_pattern_ else #use supplied argument, if any. Otherwise try to get a global variable
    {  
      cleanup_pattern = try(get("cleanup_pattern"), silent = TRUE)
      if (class(cleanup_pattern) == "try-error") #no cleanup pattern defined
        #cleanup_pattern = paste0("*_t,",stream,"_*,", ",", points_processed, "_*")
        cleanup_pattern = paste0("*_t")
    }
  
  #cleanup function in case of errors or normal termination 
  # stop sinking
  closeAllConnections()
  
  # restore original warning mode
  oldw = try(get("oldw"), silent = TRUE)
  if (class(oldw) == "try-error") #no old warning defined
    oldw=0  
    
  if(silent)
    options(warn = oldw)
  
  # remove mask if there is any (and ignore error in case there is no mask)
  tt = try(execGRASS("r.mask", flags=c("r"), intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
  tt <- try(execGRASS("g.region", region="region_org_t", intern = T), silent=TRUE) #restore original region settings, if any
  
  #delete temporarily created maps
  keep_temp = try(get("keep_temp"), silent = TRUE)
  if (class(keep_temp) == "try-error") #no keeptemp warning defined
    keep_temp=FALSE  
  
  if(keep_temp == FALSE & cleanup_pattern != "")
    try(execGRASS("g.remove", type="raster,vector", pattern=cleanup_pattern, flags=c("f", "b"), intern = TRUE, ignore.stderr = TRUE), silent=TRUE)
  
  #olderror() #release error handling
  
}


# compareRaster_i = function(rasterlist, ...)
# {
#   rasterlist = list(flowaccum_rast, relelev_rast, dist2river_rast, eha_rast, 
#                     qual_rast)
#   #check congruence of raster maps in list
#   #internal analogon to raster::compareRaster, which no longer seems to work on stacks [correction: seems to wiórk again, function is now obsolete]
#   for (rr in rasterlist[-1])
#   {
#     if (
#       !all(terra::res(rr) ==   terra::res(rasterlist[[1]]))
#       ||
#       !(terra::ncell(rr) ==   terra::ncell(rasterlist[[1]]))
#     )
#       return(TRUE)
#       #stop("Some raster maps do not match in resolution or extent. Please check.")
#   }
#   return(TRUE)
# }
