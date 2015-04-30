

CREATE TABLE db_version (
  ID INT(11) AUTO_INCREMENT NOT NULL,
  version INT DEFAULT 0 COMMENT 'version number',
	description NVARCHAR(250) COMMENT 'verbal description of changes',
	new_tables NVARCHAR(250) COMMENT 'tables new in this version',
	modified_tables NVARCHAR(250) COMMENT 'tables modified in this version',
	remarks NVARCHAR(250) COMMENT 'additional remarks',
	date_time DATETIME,
	PRIMARY KEY (ID)
) ENGINE=InnoDB;


CREATE TABLE meta_info (
pid INT(11) AUTO_INCREMENT NOT NULL COMMENT 'automatic counter, can be used as data-version number',
mod_date DATETIME COMMENT 'date of modification',
mod_user NVARCHAR(50) COMMENT 'modified by',
affected_tables NVARCHAR(255) COMMENT 'modified tables',
affected_columns NVARCHAR(255) COMMENT 'modified columns',
remarks NVARCHAR(255) COMMENT 'remarks',
PRIMARY KEY (pid)
) ENGINE=InnoDB;


CREATE TABLE vegetation (
pid INT DEFAULT 0 COMMENT 'primary key',
description NVARCHAR(50) COMMENT 'description',
stomat_r DOUBLE DEFAULT 0 COMMENT 'stomata resistance without water stress [sec/m]',
min_suction DOUBLE DEFAULT 0 COMMENT 'suction threshold for water stress effect on resistance (begin of stomata closure) [hPa]',
max_suction DOUBLE DEFAULT 0 COMMENT 'suction threshold for water stress effect on resistance (total closure of stomata – wilting point) [hPa]',
height1 DOUBLE DEFAULT 0 COMMENT 'Average height of vegetation canopy [m] before rainy season',
height2 DOUBLE DEFAULT 0 COMMENT 'Average height of vegetation canopy [m] beginning rainy season',
height3 DOUBLE DEFAULT 0 COMMENT 'Average height of vegetation canopy [m] end rainy season',
height4 DOUBLE DEFAULT 0 COMMENT 'Average height of vegetation canopy [m] after rainy season',
root_depth1 DOUBLE DEFAULT 0 COMMENT 'Rooting depth of vegetation [m] before rainy season',
root_depth2 DOUBLE DEFAULT 0 COMMENT 'Rooting depth of vegetation [m] beginning rainy season',
root_depth3 DOUBLE DEFAULT 0 COMMENT 'Rooting depth of vegetation [m] end rainy season',
root_depth4 DOUBLE DEFAULT 0 COMMENT 'Rooting depth of vegetation [m] after rainy season',
lai1 DOUBLE DEFAULT 0 COMMENT 'Leaf area index of vegetation cover  [-] before rainy season',
lai2 DOUBLE DEFAULT 0 COMMENT 'Leaf area index of vegetation cover  [-] beginning rainy season',
lai3 DOUBLE DEFAULT 0 COMMENT 'Leaf area index of vegetation cover  [-] end rainy season',
lai4 DOUBLE DEFAULT 0 COMMENT 'Leaf area index of vegetation cover  [-] after rainy season',
alb1 DOUBLE DEFAULT 0 COMMENT 'Surface albedo [-]  before rainy season',
alb2 DOUBLE DEFAULT 0 COMMENT 'Surface albedo [-]  beginning rainy season',
alb3 DOUBLE DEFAULT 0 COMMENT 'Surface albedo [-]  end rainy season',
alb4 DOUBLE DEFAULT 0 COMMENT 'Surface albedo [-]  after rainy season',
c_manning_n DOUBLE COMMENT '(optional for erosion modelling) transferred to soil_veg_components if m_copy_mannings_n is called',
c_musle_c1 DOUBLE COMMENT '(optional for erosion modelling) transferred to soil_veg_components if m_copy_c_factor is called',
c_musle_c2 DOUBLE COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
c_musle_c3 DOUBLE COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
c_musle_c4 DOUBLE COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
UNIQUE (pid)
) ENGINE=InnoDB;


CREATE TABLE horizons (
pid INT DEFAULT 0,
descr NVARCHAR(50),
soil_id INT DEFAULT 0 COMMENT 'foreign_key soils',
position INT DEFAULT 0 COMMENT 'position of horizon from top',
theta_r DOUBLE DEFAULT 0 COMMENT 'residual soil water content [Vol.-]',
theta_pwp DOUBLE DEFAULT 0 COMMENT 'water content at permanent wilting point [Vol.-]',
fk DOUBLE DEFAULT 0 COMMENT 'field capacity FK [Vol.-]',
fk63 DOUBLE DEFAULT 0 COMMENT 'field capacity FK63 [Vol.-]',
nfk DOUBLE DEFAULT 0 COMMENT 'usable field capacity  [Vol.-]',
theta_s DOUBLE DEFAULT 0 COMMENT 'saturated water content [Vol.-]',
thickness DOUBLE DEFAULT 0 COMMENT 'thickness of soil horizon [mm]',
ks DOUBLE DEFAULT 0 COMMENT 'saturated hydraulic conductivity [mm/d]',
suction DOUBLE DEFAULT 0 COMMENT 'suction at the wetting front [mm]',
pore_size_i DOUBLE DEFAULT 0 COMMENT 'pore-size-index',
bubb_pres DOUBLE DEFAULT 0 COMMENT 'bubble pressure [cm]',
coarse_frag DOUBLE DEFAULT 0 COMMENT 'fraction of coarse fragments [Vol.-]',
shrinks BOOL NOT NULL COMMENT 'flag for soil structure, currently not used, set to 0',
UNIQUE (soil_id, position)
) ENGINE=InnoDB;


CREATE TABLE soils (
pid INT DEFAULT 0,
description NVARCHAR(50),
bedrock_flag BOOL NOT NULL COMMENT 'bedrock below deepest horizon',
alluvial_flag BOOL NOT NULL COMMENT 'this is an alluvial soil',
b_om DOUBLE DEFAULT 0 COMMENT 'topsoil organic matter content [mass fraction]',
a_musle_k DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_clay DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_silt DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_sand DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_f_csand DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_f_cl_si DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_f_orgc DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
a_f_hisand DOUBLE DEFAULT 0 COMMENT 'used for automatic calculations, do not insert anything',
UNIQUE (pid)
) ENGINE=InnoDB;


CREATE TABLE subbasins (
pid INT NOT NULL DEFAULT 0,
description NVARCHAR(50),
drains_to INT NOT NULL DEFAULT 0 COMMENT 'forein key to subbasin',
area DOUBLE NOT NULL DEFAULT 0 COMMENT 'area [km**2]',
a_stream_order TINYINT DEFAULT 0 COMMENT 'stream order (gerated automatically by make_input)',
lag_time DOUBLE COMMENT 'runoff lag time [d]',
retention DOUBLE COMMENT 'runoff retention time [d]',
UNIQUE (pid)
) ENGINE=InnoDB;


CREATE TABLE landscape_units (
pid INT DEFAULT 0,
descr NVARCHAR(50),
kf_bedrock DOUBLE DEFAULT 0 COMMENT 'Hydraulic conductivity of bedrock [mm/d]',
slopelength DOUBLE DEFAULT 0 COMMENT 'Mean  slope length in landscape unit [m]',
soil_depth DOUBLE DEFAULT 0 COMMENT 'Mean maximum depth of soil zone [mm]',
allu_depth DOUBLE DEFAULT 0 COMMENT 'Maximum depth of alluvial soil zone [mm]',
riverbed_depth DOUBLE DEFAULT 0 COMMENT 'Depth of river bed below terrain component [mm]',
gw_flag BOOL NOT NULL COMMENT 'Flag for landscape unit [0: no groundwater, 1: with groundwater]',
gw_dist DOUBLE DEFAULT 0 COMMENT 'Initial depth of groundwater below surface [mm]',
frgw_delay DOUBLE DEFAULT 0 COMMENT 'Storage coefficient for groundwater outflow [day]',
sdr_lu DOUBLE COMMENT '(optional for erosion modelling) sediment delivery ratio on LU-scale',
UNIQUE (pid)
) ENGINE=InnoDB;


CREATE TABLE terrain_components (
pid INT DEFAULT 0,
descr NVARCHAR(50),
slope DOUBLE DEFAULT 0 COMMENT 'Slope of terrain component [%]',
frac_rocky DOUBLE DEFAULT 0 COMMENT 'fraction of impermeable (rock) area in each terrain component [-]',
beta_fac DOUBLE COMMENT '(optional for erosion modelling) ratio of rill/interrill erosion (computation of the L-factor see Renard et al., 1997, pp.101)',
sdr DOUBLE COMMENT '(optional for erosion modelling) sediment delivery ratio on TC-scale',
UNIQUE (pid)
) ENGINE=InnoDB;


CREATE TABLE soil_veg_components (
pid INT DEFAULT 0,
descr NVARCHAR(50),
soil_id INT NOT NULL DEFAULT 0 COMMENT 'foreign key soils',
veg_id INT NOT NULL DEFAULT 0 COMMENT 'foreign key vegetation',
musle_k DOUBLE DEFAULT 0 COMMENT 'MUSLE K-factor [(ton acre hr)/(acre ft-ton inch)]',
musle_p DOUBLE DEFAULT 0 COMMENT 'MUSLE P-factor',
coarse_frac DOUBLE DEFAULT 0 COMMENT 'coarse fraction [%]',
special_area INT DEFAULT 0 COMMENT 'Flag for special areas: 1: water, 2: impervious, 0: ordinary SVC.',
manning_n DOUBLE DEFAULT 0 COMMENT 'Mannings n roughness coefficient for overland flow',
musle_c1 DOUBLE DEFAULT 0 COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
musle_c2 DOUBLE COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
musle_c3 DOUBLE COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
musle_c4 DOUBLE COMMENT '(optional for erosion modelling) C-factor seasonality (leave empty for invariant value), transferred to soil_veg_components if m_copy_c_factor is called',
UNIQUE (pid)
) ENGINE=InnoDB;


CREATE TABLE r_subbas_contains_lu (
subbas_id INT NOT NULL DEFAULT 0 COMMENT 'foreign key to subbas',
lu_id INT DEFAULT 0 COMMENT 'foreign key to landscape units',
fraction DOUBLE NOT NULL DEFAULT 0 COMMENT 'fraction that this LU covers in the subbasin',
UNIQUE (subbas_id, lu_id)
) ENGINE=InnoDB;


CREATE TABLE r_lu_contains_tc (
lu_id INT DEFAULT 0 COMMENT 'foreign key to landscape_units',
tc_id INT NOT NULL DEFAULT 0 COMMENT 'foreign key to terrain_components',
fraction DOUBLE DEFAULT 0 COMMENT 'areal fraction of TC in the LU',
position TINYINT DEFAULT 0 COMMENT 'hillslope position of TC in LU (counting from hillslope bottom)',
UNIQUE (lu_id, tc_id)           
) ENGINE=InnoDB;


CREATE TABLE r_tc_contains_svc (
tc_id INT NOT NULL DEFAULT 0 COMMENT 'foreign key terrain_component',
svc_id INT NOT NULL DEFAULT 0 COMMENT 'foreign key soil_veg_comp',
fraction DOUBLE NOT NULL DEFAULT 0 COMMENT 'fraction of SVC in TC',
UNIQUE (tc_id, svc_id)
) ENGINE=InnoDB;


CREATE TABLE particle_classes (
class_id INT DEFAULT 0 COMMENT 'continuous number of particle size class',
description NVARCHAR(50) COMMENT 'optional name of particle size class (e.g. clay)',
upper_limit DOUBLE DEFAULT 0 COMMENT 'upper_limit of respective particle size class [mm]',
UNIQUE (class_id)
) ENGINE=InnoDB;


CREATE TABLE r_soil_contains_particles (
soil_id INT DEFAULT 0 COMMENT 'foreign key to soils',
class_id INT DEFAULT 0 COMMENT 'foreign key to particle_classes',
fraction DOUBLE DEFAULT 0 COMMENT 'mass-fraction that falls into the respective particle size class',
UNIQUE (soil_id, class_id)
) ENGINE=InnoDB;


