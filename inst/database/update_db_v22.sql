 
ALTER TABLE vegetation ADD COLUMN intfc DOUBLE COMMENT 'Interception capacity per unit LAI [m]';
ALTER TABLE vegetation ADD COLUMN crop_makk DOUBLE COMMENT 'Crop-factor for calculation of pot. evapotransp. after Makkink (optional) [-]';

ALTER TABLE subbasins ADD COLUMN lat DOUBLE COMMENT 'Latitude of subbasin centroid [decimal degree]';

ALTER TABLE horizons ADD COLUMN soil_dens DOUBLE COMMENT 'Bulk density [kg/m3]';

INSERT INTO db_version VALUES (
	22, 
	22, 
	'Added some parameter values nedded for ECHSE engine WASA', 
	'none', 
	'vegetation, subbasins, horizons', 
	'', 
	'2015-09-14 14:40:00');

