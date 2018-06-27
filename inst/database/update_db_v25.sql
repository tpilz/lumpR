 
CREATE TABLE x_seasons (
	pid INT AUTO_INCREMENT NOT NULL COMMENT 'row counter',
	parameter NVARCHAR(20) NOT NULL COMMENT 'K: K-factor, C: C-factor, P: P-factor, coarse: coarse-fraction, n : Mannings n',
	subbas_id INT NOT NULL COMMENT 'foreign key to subbas (use -1 as wildcard)',
	svc_id INT NOT NULL COMMENT 'foreign key to soil_veg_components (use -1 as wildcard)',
	yearm INT NOT NULL COMMENT 'year (use -1 as wildcard)',
	node1 INT NOT NULL COMMENT '1. node in cycle (as DOY/julian day)',
	node2 INT NOT NULL COMMENT '2. node in cycle (as DOY/julian day)',
	node3 INT NOT NULL COMMENT '3. node in cycle (as DOY/julian day)',
	node4 INT NOT NULL COMMENT '4. node in cycle (as DOY/julian day)',
	PRIMARY KEY (pid)
) ENGINE=InnoDB;

INSERT INTO db_version VALUES (
	25, 
	25, 
	'Added table with information about yearly start, end and transition of rainy season for each subbasin and svc type for specifying additional seasonalities', 
	'x_seasons', 
	'none',
	'originally already included in db ver 17 but forgot to include into R package LUMP', 
	'2018-06-27 9:00:00');

