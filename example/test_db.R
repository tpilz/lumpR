# example file to illustrate and test LUMP parameter database functions

# TODO:
#   - function to create WASA input files from database
#   - finish db_check
#   - solve DBMS issues (see github)
#   - standard values for database (e.g. particle classes, standard soil and landcover parametrizations, ...)

library(LUMP)

# create database
# consider LUMP wiki for necessary pre-processing steps: https://github.com/tpilz/LUMP/wiki/02-Databases-and-ODBC
dbname <- "test_wasa" # DSN registered at ODBC
db_create(dbname)


# LUMP output and other information (e.g. soil parameters) to database
# further information see function doc
?db_fill
db_fill(dbname=dbname,
        tables = c("r_subbas_contains_lu", "subbasins",
                   "landscape_units", "r_lu_contains_tc", "terrain_components",
                   "r_tc_contains_svc", "vegetation", "soils", "horizons", "soil_veg_components",
                   "particle_classes", "r_soil_contains_particles")
        dat_files=c("lu_stats.txt", "sub_stats.txt", "lu_db.dat", "lucontainstc.dat",
                    "terraincomponents.dat", "tc_contains_svc.dat", "vegetation.dat",
                    "soils.dat", "horizons.dat", "soil_vegetation_components.dat",
                    "particle_classes.dat", "r_soil_contains_particles.dat"), 
        dat_dir="/home/tobias/Promotion/Modellierung/Jaguaribe/WASA/LUMP/test/",
        overwrite=T, verbose=T)



# update database in case you have a pre-R-package version of the database; version 18 required
db_update(dbname)


# check database and do some post-processing
# consider function doc
# not yet finished
# open questions: 
#   proxy_frgw_delay: where does that formula come from? 
# Till: just made it up from plausibility considerations. feel free to improve.
# Slope in %? 
# Till: which one? Afaik all slope values in the DB are given as %
#How can total_mean_delay be estimated (e.g. baseflow analysis?)?
# Till: sounds like a good idea. In that case, "total_mean_delay" should probably be a weighted mean
# (weights from areal fractions of LUs) and specifiable for different subcatchments


?db_check

#Please process these cleaning actions step-by-step according to your needs
db_check(dbname, 
         check=c("filter_small_areas"), 
         option=list(area_thresh=0.005),
         fix=F,
         verbose=T)

db_check(dbname, 
         check=c("tc_slope"), 
         option=list(treat_slope=c(3,0.01,0.1)),
         fix=F,
         verbose=T)

db_check(dbname, 
         check=c("special_areas"), 
         option=list(special_area = data.frame(reference_tbl=c("vegetation", "vegetation", "soils"), ref_id=c(3,4,10), special_id=c(1,1,2))),
         fix=F,
         verbose=T)

db_check(dbname, 
         check=c("remove_water_svc"),
         fix=F,
         verbose=T)

db_check(dbname, 
         check=c("compute_rocky_frac"),
         fix=F,
         verbose=T)

db_check(dbname, 
         check=c("remove_impervious_svc"),
         fix=F,
         verbose=T)

db_check(dbname, 
         check=c("proxy_frgw_delay"), 
         option=list(total_mean_delay=200),
         fix=F,
         verbose=T)

db_check(dbname,
         check=c("delete_obsolete"),
         fix=F,
         verbose=T)

db_check(dbname,
         check=c("completeness"),
         fix=F,
         verbose=T)




