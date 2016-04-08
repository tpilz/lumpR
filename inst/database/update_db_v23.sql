 
ALTER TABLE vegetation ADD COLUMN crop_faoref DOUBLE COMMENT 'Crop-factor for calculation of pot. evapotransp. after FAO reference approach (optional) [-]';
ALTER TABLE vegetation ADD COLUMN wc_etmax DOUBLE COMMENT 'Parameter giving the volumetric water content where et_act equals et_pot, typically wc_etmax / wc_fk = [0.5..0.8] [m3/m3]';
ALTER TABLE vegetation ADD COLUMN par_stressHum DOUBLE COMMENT 'Parameter to calculate water vapour deficit stomatal conductance stress factor (in WASA hard-coded as 0.03) [1/hPa]';
ALTER TABLE vegetation ADD COLUMN glo_half DOUBLE COMMENT 'Solar radiation at which stomatal conductance is half of its maximum (in WASA hard-coded as 100) [W/m2]';

ALTER TABLE soils ADD COLUMN Phil_s DOUBLE COMMENT 'Infiltration: Philip parameter: Sorptivity, in ECHSE calculated internally if set to NA [ms^(-1/2)]';
ALTER TABLE soils ADD COLUMN Phil_a DOUBLE COMMENT 'Infiltration: Philip parameter: Second term parameter, in ECHSE calculated internally if set to NA [m/s]';
ALTER TABLE soils ADD COLUMN Hort_ini DOUBLE COMMENT 'Infiltration: Horton parameter: initial infiltration rate [m/s]';
ALTER TABLE soils ADD COLUMN Hort_end DOUBLE COMMENT 'Infiltration: Horton parameter: final infiltration rate [m/s]';
ALTER TABLE soils ADD COLUMN Hort_k DOUBLE COMMENT 'Infiltration: Horton parameter: decay constant [1/s]';

ALTER TABLE subbasins ADD COLUMN lon DOUBLE COMMENT 'Longitude of subbasin centroid [decimal degrees]';
ALTER TABLE subbasins ADD COLUMN elev DOUBLE COMMENT 'Average elevation above sea level of subbasin [m]';

INSERT INTO db_version VALUES (
	23, 
	23, 
	'Added some more parameter values nedded for ECHSE engine WASA', 
	'none', 
	'vegetation, soils, subbasins', 
	'', 
	'2016-03-24 10:45:00');
