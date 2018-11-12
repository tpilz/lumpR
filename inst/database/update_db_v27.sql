ALTER TABLE horizons CHANGE COLUMN shrinks shrinks TINYINT(1) NULL DEFAULT 0 AFTER coarse_frag;

INSERT INTO db_version VALUES (
	27, 
	27, 
	'corrected default value for column shrinks', 
	'horizons', 
	'none',
	'', 
	'2018-11-12 11:00:00');

