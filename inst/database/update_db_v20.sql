 
CREATE TABLE rainy_season (
	pid INT AUTO_INCREMENT NOT NULL COMMENT 'row counter',
	subbas_id INT NOT NULL COMMENT 'foreign key to subbas (use -1 as wildcard)',
	veg_id INT NOT NULL COMMENT 'foreign key vegetation (use -1 as wildcard)',
	yearm INT NOT NULL COMMENT 'year (use -1 as wildcard)',
	node1 INT NOT NULL COMMENT '1. node in cycle (as DOY/julian day)',
	node2 INT NOT NULL COMMENT '2. node in cycle (as DOY/julian day)',
	node3 INT NOT NULL COMMENT '3. node in cycle (as DOY/julian day)',
	node4 INT NOT NULL COMMENT '4. node in cycle (as DOY/julian day)',
	PRIMARY KEY (pid)
) ENGINE=InnoDB;

INSERT INTO db_version VALUES (
	20, 
	20, 
	'Added table with information about yearly start, end and transition of rainy season for each subbasin and vegetation type', 
	'rainy_season', 
	'none',
	'originally already included in db ver 17 but forgot to include into R package LUMP', 
	'2015-07-14 15:00:00');

