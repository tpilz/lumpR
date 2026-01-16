# lumpR/modify_eha_head_files.R
# Copyright (C) 2026 Till Francke
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


#' Change header file needed by prof_class() according to desired number of classes to create in classification 
#'
#' This function adjusts the file \code{eha_2d_head_file} (default: \code{"eha_2d_head.txt"} ), which is used as a configuration file 
#' for the classification of the catenas by \code{\link[lumpR]{prof_class()}}.
#' To be run after \code{\link[lumpR]{area2catena()}}, followed by \code{\link[lumpR]{prof_class()}}.
#'
#' @param eha_2d_head_file Name of file containing meta-information for classification
#'      generated with \code{\link[lumpR]{area2catena}} describing 2-D representation of the EHAs (see \code{Notes}).'
#' @param eha_1d_file=NULL Name of file containing meta-information for classification
#'      (e.g. generated with  \code{\link[lumpR]{prepare_snow_input()}} (see \code{Notes}).'
#' @param no_classes Named integer vector holding the number of classes to create for each attribute. Its first three 
#' elements should be named "shape", "extent", and "weighting".
#'
#' @details  \code{eha_2d_head_file} (default: \code{"eha_2d_head.txt"} ) is used as a descriptor file to 
#' \code{eha_2d_file} (default: \code{"eha_2d.txt"} ) created by \code{\link[lumpR]{area2catena()}} and serves as
#' configuration file for the classification of the catenas by \code{\link[lumpR]{prof_class()}}. It may be adjusted manually or using  \code{\link[lumpR]{modify_eha_head_files()}} .
#' It should resemble the structure as in the following example: \code{
#' #This file works as a header to the output of area2catena and input to prof_class. Don't add additional headerlines.
#' #1. line after header: description/field names of the data columns contained in file eha_2d_file
#' #2. line after header: specifies, how many columns of data belong to the respective data-field given in line 1
#' #3. line after header: number of classes/ weighting factors for classification process
#' #4. line after header: factors used for weighting in partition process (column: 1: number of TC to create; 2.: partition method (not yet used); 3.: not used; 4-nn: weighting of supplemental data in TC-partitioning relative to slope.)
#' id	p_no	elevation	svc	soils	slope_width	aspect	rel_alt
#'1	1	1	72	11	1	NA	NA
#'-1	3	10	1	3	1	4	2
#'3	0	0	0	0	0	NA	NA
#' }      
#'
#' Please note: Attributes with \code{NA} in line 2 after header (i.e. 'aspect' and 'rel_alt' in the example) are not expected to be part of \code{eha_2d_file}.
#' Instead, they can be provided as an optional extra file via argument \code{eha_1d_file} to \code{\link[lumpR]{prof_class()}}.
#' @return 'Returns nothing. Re-writes \code{eha_2d_head_file}, adjusting it with the supplied parameters.
#'
#'
#' @author
#'  Till Francke
#' @export

modify_eha_head_files <- function(eha_2d_head_file, eha_1d_file=NULL, no_classes) {
  if (!identical(names(no_classes[1:3]), c("shape", "extent", "weighting"))) {
    stop("Argument no_classes needs to be a integer vector with its first three elements named 'shape', 'extent',  'weighting'.")
  }
  
  #read 2D-file
  header_2d_comments = readLines(eha_2d_head_file, n=5) #read header comments only
  
  header_2d_dat = read.table(eha_2d_head_file, sep="\t", skip = 5, fill=TRUE, header=TRUE) #we need to set fill=TRUE as there may be empty fields trailing. However, this shifts the first column to the column names
  if(!all(diff(as.numeric(row.names(header_2d_dat)))==1)) #are the row names not sequentially numbered? Then, fix table structure.
    header_2d_dat[,1:ncol(header_2d_dat)] = cbind(as.numeric(row.names(header_2d_dat)), as.matrix(header_2d_dat)[,-ncol(header_2d_dat)]) #shift columns names back to first column
  attr_2d = names(header_2d_dat)
  attr_2d[1:3] = c("shape", "extent", "weighting") #rename, as they are named differently in the outpu of area2catena
  
  #read 1D-file
  if (!is.null(eha_1d_file))
  {
    data_1d_dat = read.table(eha_1d_file, sep="\t", skip = 0, fill=TRUE, header=TRUE, nrow=1) #read data file to infer its structure
    attr_1d = names(data_1d_dat)[-1] #ignore first column as this contains the EHA-id
    attr_1d = gsub(pattern = "\\..*", repl="", x=attr_1d) #remove parts behind the .  - these are the "parent" attributes, e.g. "aspect.sin" and "aspect.cos" belong to parent "aspect"
    column_counts = table(attr_1d) #count columns used for each parent attribute
    attr_1d = names(column_counts) # remove duplicates
  } else
    attr_1d = NULL
  
  #check for missing attributes
  unspec_attributes = setdiff(c(attr_2d, attr_1d), names(no_classes)) #find attributes in header file not specified in no_classes
  if (length(unspec_attributes)>0) 
    stop(paste0("The following attributes are contained in '", eha_2d_head_file, "' and/or '", eha_1d_file, "' but not contained in 'no_classes':", paste(unspec_attributes, collapse=", "),
                "."))
  
  missing_cols = setdiff(names(no_classes), c(attr_2d, attr_1d)) #find columns not yet contained in the file
  if (length(missing_cols)>0) 
    stop(paste0("The following attributes are contained in 'no_classes', but neither in '", eha_2d_head_file, "' nor '", eha_1d_file, "' :", paste(missing_cols, collapse=", "),
                "."))
  
  #modify 2d-header file
  #header_2d_dat[, missing_cols] = NA #add missing columns
  header_2d_dat[2, 1:3] = no_classes[1:3] #set number of LU-classes for "shape", "extent" and "weighting"
  header_2d_dat[2, 1] = abs(header_2d_dat[1,1]) * -1 #force "successive" mode
  header_2d_dat[2, attr_2d[-(1:3)]] = no_classes[attr_2d[-(1:3)]] #set number of classes for remaining 2D-attributes
  #header_2d_dat[3,] = ... #todo: add writing TC-weighting factors
  
  writeLines(text=header_2d_comments, con=eha_2d_head_file)
  options(warn=-1) #suppress warning about row names)
  write.table(x=header_2d_dat, file=eha_2d_head_file, append=TRUE,  row.names=FALSE, quote=FALSE, sep="\t")
  options(warn=1) #restore warnings
  message(paste0(eha_2d_head_file, " rewritten."))
  
  #write 1d-header file
  if (!is.null(eha_1d_file))
  {
    header_1d_dat = array(NA, dim=c(2, length(attr_1d)+1), dimnames=list(c("columns", "no_classes"), c("eha_id", attr_1d)))
    
    header_1d_dat["columns", 1] = 1 #set number of columns used eha_id
    header_1d_dat["columns", names(column_counts)] = column_counts #set number of columns used for each attribute
    header_1d_dat["no_classes",  names(column_counts)] = no_classes[ names(column_counts)] #set number of classes for 1D-attributes
    eha_1d_head_file = gsub(pattern="\\.([^\\.]*)", repl="_head.\\1", x=eha_1d_file)
    write.table(x=header_1d_dat, file=eha_1d_head_file, row.names=FALSE, quote=FALSE, sep="\t")
    
    message(paste0(eha_1d_head_file, " rewritten."))
  }
}
