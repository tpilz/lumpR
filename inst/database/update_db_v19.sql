 
ALTER TABLE horizons CHANGE descr description NVARCHAR(50);
ALTER TABLE landscape_units CHANGE descr description NVARCHAR(50);
ALTER TABLE particle_classes CHANGE `desc` description NVARCHAR(50);
ALTER TABLE soils CHANGE `desc` description NVARCHAR(50);
ALTER TABLE soil_veg_components CHANGE descr description NVARCHAR(50);
ALTER TABLE subbasins CHANGE `desc` description NVARCHAR(50);
ALTER TABLE terrain_components CHANGE descr description NVARCHAR(50);
ALTER TABLE vegetation CHANGE `desc` description NVARCHAR(50);

ALTER TABLE db_version CHANGE `date` date_time DATETIME;

INSERT INTO db_version VALUES (
	19, 
	19, 
	'First version within LUMP R-package', 
	'none', 
	'horizons, landscape_units, particle_classes, soils, soil_veg_components, subbasins, terrain_components, vegetation, db_version', 
	'adjusted data type in db_version and column names for the other tables', 
	'2015-05-11 17:08:00');

