<?php
/* user settings for make_wasa_input.php and compute_musle_k.php */

//#DB-NAME specify database name here (the name you used in the ODBC-setup)
//$con = sql_connect('MySQL-db','root','root');  
//$con=sql_connect('access_bethany_fallow','','');	//bethany_fallow
//$con=sql_connect('bengue','','');	//Bengue
//$con=sql_connect('esera_sascha','','');	//Esera Sascha

//$con=sql_connect('esera_egu_center','','');	//esera with balanced LUMP weighting
//$con=sql_connect('esera_egu_form','','');	//esera with exclusive form weighting for derivation of LUs
//$con=sql_connect('esera_egu_soil','','');	//esera with exclusive soil weighting for derivation of LUs
//$con=sql_connect('esera_egu_veg','','');	//esera with exclusive vegetation weighting for derivation of LUs

//$con=sql_connect('wasa_conrad','','');	//Conrad's basin

//$con=sql_connect('esera30','','');	//Esera 30 m resolution
//$dest_dir="./out_esera30/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created

//$con=sql_connect('villacarli_30','','');	//Villacarli extract from Esera 30 m resolution
//$dest_dir="./out_villacarli30/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created


//$con=sql_connect('cabecera_30','','');	//Cabecera extract from Esera 30 m resolution
//$dest_dir="./out_cabecera30/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created

//$con=sql_connect('lowerisabena_30','','');	//lowerisabena extract from Esera 30 m resolution
//$dest_dir="./out_lowerisabena_30/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created
//$dest_dir="./out_isabena_30/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created

//$con=sql_connect('nfis','','');	//lowerisabena extract from Esera 30 m resolution
//$dest_dir="./out_nfis/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created

//$con=sql_connect('tutorial','','');	//WASA tutorial
//$dest_dir="./out_tutorial/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are 

$con=sql_connect('test_wasa','','');	//Esera 2014
$dest_dir="/home/tobias/Promotion/Modellierung/Jaguaribe/WASA/make_wasa_input/php_test_checks_dis/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are 

//$con=sql_connect('esera_2014_B','','');	//Esera 2014 with model enhancements
//$dest_dir="./out_esera_2014_b/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are 

//$dest_dir="./out_singleslope/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created


// $con=sql_connect('Ribera_Salada_areal','','');	//Ribera Salada, areal version
//$con=sql_connect('bengue_areal_v9','','');	//Bengue, areal version

//$con=sql_connect('bengue_areal_v9_alluvium','','');	//Bengue, areal version with alluvium


//$con=sql_connect('aiuaba_extracted_v10','','');	//Aiuaba extracted, derived from Bengue areal, simplified



//$con=sql_connect('bengue_areal_v9_140','','');	//Aiuaba extracted, derived from Bengue areal, simplified, alluvial areas added
//$dest_dir="./out_bengue_areal_v9_140/";	//output directory, end with "/"

//$con=sql_connect('AEB_alluvium','','');	//Aiuaba extracted, derived from Bengue areal, simplified, alluvial areas added
//$dest_dir="./out_aiuaba_alluvium/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created

//$con=sql_connect('bengue_areal_v10_140_calibrated','','');	//Bengue areal, with corrections from calibration, 140 subbasins
//$dest_dir="./out_bengue_areal_v10_140_calibrated/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created

//$con=sql_connect('bengue_areal_v10_30_calibrated','','');	//Bengue areal, with corrections from calibration, 30 subbasins
//$dest_dir="./out_bengue_areal_v10_30_calibrated/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created


//$con=sql_connect('ribera_areal_1957','','');	//Ribera Salada 30 m resolution
//$dest_dir="./out_ribera_areal_1957/";	//#DEST-DIR	specify the directory in which the output files (=WASA input files) are created


//#USER_SETTINGS
$print_process=1;		//enable/disable detailed screen output of process
$normalize_lus_in_subbas=0;  //enable/disable automatic correction that all fractions of lus within a subbasin sum up to 1	
$normalize_tc_in_lu=0;		//enable/disable automatic correction that all fractions of tcs within a lu sum up to 1
$normalize_svc_in_tc=0;		//enable/disable automatic correction that all fractions of svcs within a tc sum up to 1
$compute_stream_order=0;	//enable/disable automatic computation of column a_stream_order in subbasins

$coarse_fraction=1;		//source of coarse fraction for erosion modelling
				//options:	0:read from table soil_veg_components
				//		1:use entry of topmost horizon of respective soil
?>