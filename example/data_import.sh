# command collection for data i/o for preparing GRASS for use with LUMP
# These are just examples! adjust to your needs!
# Most steps can also be performed via the GUI.

cd e:/till/uni/organisation/iran/2_soil #set working directories, use "/" instead of "\" in Windows!

#create a new location using GUI (Settings -> GRASS working env -> Create New Location) or using the command below:
	#import ESRI-grid into GRASS and create new location (create or use arbitrary location/mapset, run command below, exit location and change to new mapset)
	r.in.gdal input=/cygdrive/d/till/ output=dem -e location=mynewlocation


#import DEM from GEO-Tiff	
r.in.gdal input=C:/Users/francke/ttt.tif output=dem15_filled -o                          

	
#export DEM for treatment in SAGA
r.out.arc input=DEM15@PERMANENT output=dem15
#run SAGA, "import export/grids", "Esri arcinfo grids"; "Terrain analy", "Preproc" "Planchon/Darboux"; 

#reimport SAGA filled DEM	
r.in.gdal input=C:/Users/francke/ttt.tif output=dem15_filled -o                          


#import soil map
	v.in.ogr dsn=e:/till/uni/parameterisierung/Esera_vergleich/lump/soils/soils.shp output=soils_vec -r
	v.to.rast input=soils_vec layer=1 output=soils_org use=attr column=soil_id rows=4096
	r.mapcalc soils_org_t="int(soils_org-46000)" #simplify IDs
	g.remove soils_org
	g.rename soils_org_t,soils_org
	
#alluvial areas
	v.in.ogr dsn=e:/till/uni/gis/esera/geology/chebro/aluvial/aluvial_sandsiltclay.shp output=aluvial_vec layer=aluvial_sandsiltclay min_area=0.0001 type=boundary snap=-1 
	v.to.rast input=aluvial_vec layer=1 output=aluvial use=cat value=1 rows=4096


#badlands
	v.in.ogr dsn=e:/till/uni/gis/esera/badlands/Badland_Polygons.shp output=badland_vec layer=Badland_Polygons min_area=0.0001 type=boundary snap=-1 
	v.to.rast input=badland_vec layer=1 output=badland use=val value=1 rows=4096

#vegetation / landuse
	v.in.ogr dsn=e:/till/uni/parameterisierung/Esera_vergleich/lump/landuse/landuse_31n_2.shp output=vegetation_vec min_area=0.0001 type=boundary snap=-1 -r
	v.to.rast input=vegetation_vec layer=1 output=vegetation use=attr col=wasa_id rows=4096

	RMASK="RMASK" 
	r.mapcalc RMASK_t="if(isnull(subbas),null(),1)"
	r.grow in=RMASK_t out=RMASK radius=20 #eliminate empty spaces
	g.remove RMASK_t
	g.copy $RMASK,MASK

	TOBEGROWN=vegetation
	g.remove grown >> /dev/null
	r.grow in=$TOBEGROWN out=grown radius=20 #eliminate empty spaces
	r.mapcalc $TOBEGROWN=$RMASK*grown

	r.mapcalc nodata="$RMASK * isnull($TOBEGROWN)"
	r.stats nodata -c #if still 1s in this map, repeat block above
	g.remove nodata 
	

#overlay maps
	r.mapcalc soils_aluv="if(isnull(aluvial),soils_org,soils_org+10000)" #denote alluvial soils
	
	#superimpose badlands as extra class in soil and landuse
	r.mapcalc soils_aluv_t="if(isnull(badland),soils_aluv,50)" #denote badlands
	g.remove soils_aluv
	g.rename soils_aluv_t,soils_aluv
	r.mapcalc vegetation_t="if(isnull(badland),vegetation,50)" #denote badlands
	g.remove vegetation
	g.rename vegetation_t,vegetation
	
	#generate SVC-map from landuse and soil data with unique SVC_IDs consisting of [soil_id,5 digits][veg_id, 2 digits]
	r.mapcalc svc="soils_aluv*(100)+vegetation"
	
	#output table with relation svc-id:soil-id_veg-id
	r.stats svc,soils_aluv,vegetation -n > scv_key.txt

	g.region zoom=RMASK save=esera_region --overwrite #sets a region corresponding to the extent of the area of interest
	
#replace NAs in badland
	r.mapcalc badland_="if(isnull(badland),0,1)"
	g.remove badland
	g.rename badland_,badland

	
#import barasona reservoir
	v.in.ogr dsn=e:/till/uni/gis/esera/water/barasona_utm31.shp output=reservoir_vec layer=barasona_utm31 min_area=0.0001 type=boundary snap=-1 
	v.to.rast input=reservoir_vec layer=1 output=reservoir use=val val=1 rows=4096
	d.rast reservoir

	#update river information with extent of reservoir (can also be done after processing river part in lump_2.sh
	r.mapcalc river_t="if((river_rast==1) || not(isnull(reservoir)),1,0)"
	d.rast river_t
	g.remove river_rast
	g.rename river_t,river_rast
	

