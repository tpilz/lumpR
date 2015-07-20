 
ALTER TABLE meta_info MODIFY affected_tables NVARCHAR(400);
ALTER TABLE meta_info MODIFY remarks NVARCHAR(400);

ALTER TABLE db_version MODIFY remarks NVARCHAR(400);
ALTER TABLE db_version MODIFY description NVARCHAR(400);

ALTER TABLE vegetation MODIFY description NVARCHAR(255);

ALTER TABLE horizons MODIFY description NVARCHAR(255);

ALTER TABLE soils MODIFY description NVARCHAR(255);

ALTER TABLE subbasins MODIFY description NVARCHAR(255);

ALTER TABLE landscape_units MODIFY description NVARCHAR(255);

ALTER TABLE terrain_components MODIFY description NVARCHAR(255);

ALTER TABLE soil_veg_components MODIFY description NVARCHAR(255);

ALTER TABLE particle_classes MODIFY description NVARCHAR(255);

INSERT INTO db_version VALUES (
	21, 
	21, 
	'Enlarged columns to hold more characters', 
	'none', 
	'meta_info, db_version, vegetation, horizons, soils, subbasins, landscape_units, terrain_components, soil_veg_components, particle_classes', 
	'Error of oversized character strings detected when testing LUMP for MariaDB, SQLite never complained, access max length only 255', 
	'2015-07-17 15:15:00');

