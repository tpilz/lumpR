CREATE TABLE reservoirs_strategic (
	pid INT AUTO_INCREMENT NOT NULL COMMENT 'row counter',
	subbas_id INT NOT NULL COMMENT 'foreign key to subbas',
	name NVARCHAR(20) NOT NULL COMMENT 'reservoir name (optional)',
	minlevel double COMMENT 'Initial minimum level [m]',
	maxlevel double COMMENT 'Maximum water level [m]',
	vol0 double COMMENT 'Initial volume [10^3 m^3]',
	storecap double COMMENT 'Initial storage capacity [10^3 m^3]. ',
	damflow double COMMENT 'Target release through the barrage’s intake devices [m^3/s]',
	damq_frac double COMMENT 'Maximum fraction of damflow which is released from the reservoir [-]',
	withdrawal double COMMENT 'Water withdrawal discharge to supply the water use sectors in the reservoir [m^3/s]. ',
	damyear double COMMENT 'Year of construction of the dam',
	maxdamarea double COMMENT 'Initial maximum area of the reservoir (ha). ',
	damdead double COMMENT 'Initial dead volume of the reservoir [10^3 m^3]',
	damalert double COMMENT 'Initial alert volume of the reservoir [10^3 m^3]',
	dama double COMMENT 'Parameters of the area-volume relationship (area=dama*Vol^damb) [-]',
	damb double COMMENT 'Parameters of the area-volume relationship (area=dama*Vol^damb) [-]',
	q_outlet double COMMENT 'Maximum outflow discharge released through the bottom outlets [m^3/s]',
	fvol_botm double COMMENT 'Fraction of storage capacity that indicates the minimum storage volume for sediment release through the bottom outlets [-]',
	fvol_over double COMMENT 'Fraction of storage capacity that indicates the minimum storage volume for water release through the spillway [-]',
	damc double COMMENT 'Parameters of the spillway rating curve (Qout=damc*Hv^damd) [-]',
	damd double COMMENT 'Parameters of the spillway rating curve (Qout=damc*Hv^damd) [-]',
	elevbottom double COMMENT 'bottom outlet elevation [m]',
	PRIMARY KEY (pid)
) ENGINE=InnoDB;

INSERT INTO db_version VALUES (
	26, 
	26, 
	'Added table for strategic reservoirs', 
	'reservoirs_strategic', 
	'none',
	'', 
	'2018-07-12 11:00:00');

