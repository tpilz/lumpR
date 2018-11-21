CREATE TABLE reservoirs_small_classes (
	pid INT AUTO_INCREMENT NOT NULL COMMENT 'primary key, reservoir class ID',
	name NVARCHAR(20) COMMENT 'reservoir class description (optional)',
	maxlake0 double COMMENT 'Upper limit of reservoir size class in terms of volume [m^3]',
	lake_vol0_factor double COMMENT 'Fraction of storage capacity that indicates the initial water volume in the reservoir size classes [-]',
	lake_change double COMMENT 'Factor that indicates yearly variation in the number of reservoirs of the size classes [-]',
	alpha_Molle double COMMENT 'Parameters of the area-volume relationship in the reservoir size classes (Area=alpha.k.(Vol/k)alpha/(alpha-1)) [-]. Values of reservoir area and volume are expressed in m² and m³, respectively',
	k_Molle double COMMENT 'Parameters of the area-volume relationship in the reservoir size classes (Area=alpha.k.(Vol/k)alpha/(alpha-1)) [-]. Values of reservoir area and volume are expressed in m² and m³, respectively',
	damc double COMMENT 'Parameters of the spillway rating curve in the reservoir size classes (Qout=damc.Hv^damd) [-]. Values of water height over the spillway and overflow discharges are expressed in m and m³/s, respectively',
	damd double COMMENT 'Parameters of the spillway rating curve in the reservoir size classes (Qout=damc.Hv^damd) [-]. Values of water height over the spillway and overflow discharges are expressed in m and m³/s, respectively',
	PRIMARY KEY (pid)
);

CREATE TABLE r_subbas_contains_reservoirs_small (
	subbas_id INT NOT NULL COMMENT 'foreign key to subbas',
	res_class_id INT NOT NULL COMMENT 'foreign key to reservoirs_small_classes',
	n_reservoirs INT NOT NULL COMMENT 'number of reservoirs [-]',
	maxlake double COMMENT 'Mean value of initial storage capacity of the hypothetical representative reservoirs of the size classes [m^3]',
	UNIQUE (subbas_id, res_class_id)
);

ALTER TABLE reservoirs_strategic MODIFY pid INT AUTO_INCREMENT NOT NULL COMMENT 'primary key';

INSERT INTO db_version VALUES (
	28, 
	28, 
	'Added tables for small reservoirs', 
	'reservoirs_small,r_subbas_contains_reservoirs_small', 
	'none',
	'', 
	'2018-11-20 11:00:00');

