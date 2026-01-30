ALTER TABLE landscape_units ADD COLUMN aspect DOUBLE COMMENT 'Aspect of LU, clockwise from North (optional, required for snow modelling) [deg]';
ALTER TABLE landscape_units ADD COLUMN relative_altitude DOUBLE COMMENT 'Relative altitude of LU relative to mean subbasin altitude [m]';

INSERT INTO db_version VALUES (
	29, 
	29, 
	'Added columns for snow parameters', 
	'none', 
	'landscape_units', 
	'', 
	'2026-01-30 14:35:00');
