
ALTER TABLE subbasins ADD COLUMN chan_len DOUBLE COMMENT 'Subbasin main channel length [m]';

INSERT INTO db_version VALUES (
	24, 
	24, 
	'Added main channel length parameter for ECHSE engine WASA', 
	'none', 
	'subbasins', 
	'', 
	'2017-02-07 12:00:00');
