# LUMP/wasa_reservoir_par.R
# Copyright (C) 2015 Tobias Pilz
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


#' Generation of WASA parameter file reservoir.dat
#' 
#' Function generates the WASA parameter file reservoir.dat from a pre-processed
#' reservoir vector file stored in a GRASS location.
#' 
#' @param res_vect Reservoir vector file in GRASS location. Needs attribute table
#'      with WASA parameters for input file reservoir.dat which has to be prepared
#'      in advance. See Details for more information and mandatory columns.
#'      
#' @param res_out_vect Vector (points) file with location of reservoir outlets. This
#'      can be created with \code{\link[LUMP]{reservoir_outlet}}. It is needed because
#'      reservoir polygons of \code{res_vect} might overlap with multiple subbasins.
#'      Attribute table needs column \code{name} as for \code{res_vect} for identification.
#'      
#' @param subbasin Subbasin raster map in GRASS location. Can be created with
#'      \code{\link[LUMP]{calc_subbas}}.
#'      
#' @param dir_out Character string specifying output directory (will be created if it
#'      does not yet exist).
#'      
#' @return Writes the WASA reservoir parameter file reservoir.dat into the specified
#'      output directory. The file is ready to use and needs no further adjustments.
#'      
#' @details For each reservoir that should be modelled explicitly within WASA the
#'      following information need to be collected and written into the vector file's
#'      attrbute table (order is not important):
#'      
#'      \emph{name}\cr
#'      Reservoir identifier, e.g. the name of the reservoir.
#' 
#'      \emph{minlevel}\cr
#'      Initial minimum level in the reservoir [m]. Value varies because of sediment
#'      accumulation.
#'      
#'      \emph{maxlevel}\cr
#'      Maximum water level in the reservoir [m].
#'      
#'      \emph{vol0}\cr
#'      Initial volume of the reservoir [10^3 m^3]. Value varies because of sediment
#'      accumulation. Set to '-999' if information is not available.
#'      
#'      \emph{storecap}\cr
#'      Initial storage capacity of the reservoir [10^3 m^3]. Value varies because of
#'      sediment accumulation.
#'      
#'      \emph{damflow}\cr
#'      Target outflow discharge of the reservoir (90 % reliability) [m^3/s].
#'      
#'      \emph{damq_frac}\cr
#'      Fraction of Q90 released from the reservoir in regular years [-].
#'      
#'      \emph{withdrawal}\cr
#'      Water withdrawal discharge from the reservoir to supply the water use sectors 
#'      [m^3/s]. Outflow discharge through the dam is not considered.
#'      
#'      \emph{damyear}\cr
#'      Year of construction of the dam (YYYY).
#'      
#'      \emph{maxdamarea}\cr
#'      Initial maximum area of the reservoir [ha]. Value varies because of sediment
#'      accumulation.
#'      
#'      \emph{damdead}\cr
#'      Initial dead volume of the reservoir [10^3 m^3]. Value varies because of
#'      sediment accumulation.
#'      
#'      \emph{damalert}\cr
#'      Initial alert volume of the reservoir [10^3 m^3]. Value varies because of
#'      sediment accumulation.
#'      
#'      \emph{dama, damb}\cr
#'      Parameters of the area-volume relationship in the reservoir:
#'      area = dama * Vol^damb [-]. Values of reservoir area and volume are
#'      expressed in m^2 and m^3, respectively.
#'      
#'      \emph{q_outlet}\cr
#'      Maximum outflow discharge released through the bottom outlets of the
#'      reservoir [m^3/s].
#'      
#'      \emph{fvol_botm}\cr
#'      Fraction of storage capacity that indicates the minimum storage volume for
#'      sediment release through the bottom outlets of the reservoir [-].
#'      
#'      \emph{fvol_over}\cr
#'      Fraction of storage capacity that indicates the minimum storage volume for
#'      water release through the spillway of the reservoir [-].
#'      
#'      \emph{damc, damd}\cr
#'      Parameters of the spillway rating curve of the reservoir: Qout = damc * Hv^damd
#'      [-]. Values of water height over the spillway and overflow discharges are
#'      expressed in m and m^3/s, respectively.
#'      
#'      \emph{elevbottom}\cr
#'      Bottom outlet elevation of the reservoir [m].
#'      
#' @author Tobias Pilz \email{tpilz@@uni-potsdam.de}
#'
#' @export 
wasa_reservoir_par <- function(
  ### INPUT ###
  res_vect,
  res_out_vect,
  subbasin,
  dir_out="./"
  
) {
  
  # create output directory
  dir.create(dir_out, recursive=T)
  
  # remove mask if any
  execGRASS("r.mask", flags=c("r"))
  
  # get subbasin values
  sub_rast <- raster(readRAST6(subbasin))
  
  # get reservoir data
  res <- readVECT6(res_vect)
  
  # get coordinates of outlets
  res_out <- readVECT6(res_out_vect)
  
  # get subbasin no for each outlet point
  res_sub_all <- extract(sub_rast, res_out, df=T)
  names(res_sub_all)[2] <- "sub_id_new"
  
  # check for duplicates (multiple outlet points per subbasin)
  dupl <- duplicated(res_sub_all[[2]], incomparables=c(NA))
  if (any(dupl))
    stop(paste0("Subbasin no. ", res_sub_all[dupl], " contains multiple reservoir outlet points."))
  
  # combine subbasin information with reservoir parameter data
  res_dat <- res@data
  outlet_dat <- cbind(res_out@data$name, res_sub_all)
  outlet_dat <- na.omit(outlet_dat) # remove reservoirs outside study area
  res_dat_all <- merge(res_dat, outlet_dat, by.x="name", by.y=1)
  
  # sort data for writing output
  res_dat_sort <- data.frame(res_dat_all$sub_id_new,
                    res_dat_all$minlevel,
                    res_dat_all$maxlevel,
                    res_dat_all$vol0,
                    res_dat_all$storecap,
                    res_dat_all$damflow,
                    res_dat_all$damq_frac,
                    res_dat_all$withdrawal,
                    res_dat_all$damyear,
                    res_dat_all$maxdamarea,
                    res_dat_all$damdead,
                    res_dat_all$damalert,
                    res_dat_all$dama,
                    res_dat_all$damb,
                    res_dat_all$q_outlet,
                    res_dat_all$fvol_botm,
                    res_dat_all$fvol_over,
                    res_dat_all$damc,
                    res_dat_all$damd,
                    res_dat_all$elevbottom)
  
  # prepare output file
  header_str <- "Subasin-ID, minlevel[m], maxlevel[m], vol0([1000m**3]; unknown=-999), storcap[1000m**3],
        damflow[m**3/s], damq_frac[-], withdrawal[m**3/s], damyear[YYYY], maxdamarea[ha],
        damdead[1000m**3], damalert[1000m**3], dama[-], damb[-], qoutlet[m**3/s],
        fvol_bottom[-], fvol_over[-], damc[-], damd[-], elevbottom[m]"
  header_str <- gsub("\n|\\s|\t","",header_str)
  
  write(file=paste(dir_out, "reservoir.dat", sep="/"),
        x=c("Specification of reservoir parameters", header_str))
  
  # write data
  write.table(res_dat_sort, paste(dir_out, "reservoir.dat", sep="/"), append=T, quote=F,
              sep="\t", row.names=F, col.names=F)
  
  
} # EOF
