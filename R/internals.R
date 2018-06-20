#internal functions
check_raster <- function(map, argument_name="") { #check existence of raster map
  cmd_out <-tryCatch(suppressWarnings(execGRASS("r.info", map=map, intern = T)), error=function(e){})
  stat = attr(cmd_out, "status")
  if (!is.null(stat) && stat== 1)
    stop(paste0("Raster map '", map, "' not found", ifelse(argument_name=="", ".", paste0(" (argument '", argument_name,"')."))))
}

check_vector <- function(map, parameter_name="") { #check existence of vector map
  cmd_out <-tryCatch(suppressWarnings(execGRASS("v.info", map=map, intern = T)), error=function(e){})
  stat = attr(cmd_out, "status")
  if (!is.null(stat) && stat== 1)
    stop(paste0("Vector map '", map, "' not found", ifelse(argument_name=="", ".", paste0(" (argument '", argument_name,"')."))))
}