#internal functions
check_raster <- function(map, argument_name="") { #check existence of raster map
  cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
  if (!grepl(map, pattern = "@"))
    map = paste0(map,"@", cur_mapset)  #add mapset name, unless already given. Otherwise, these checks may fall back on PERMANENT yielding true, but readRAST will fail later

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
  #if raster exist both in local mapset and PERMANENT, force the reading of the local version, otherwise readRAST fails
  cur_mapset = execGRASS("g.mapset", flags="p", intern=TRUE) #determine name of current mapset
  # mapset name in R (meta symbols replaced by ".")
  cur_mapset_r = gsub("[-+*/@.?!]", ".", cur_mapset)
  if (!grepl(raster_name, pattern = "@")) #expand raster name because of bug in readRAST 
  {  
    raster_name = paste0(raster_name,"@", cur_mapset) #add mapset name, unless already given. Otherwise, strange errors may occur when the same raster exists in PERMANENT
    name_expanded = TRUE #indicate that 
  }  else name_expanded = FALSE
  tmp <- readRAST(raster_name)
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