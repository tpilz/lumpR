<?php

//Till:
//0.9$ 14.7.2014: fixed faulty checking of dd_version

//Till:
//0.9$ alpha:	3.8.2006
//RENAMED table cum_* to t_cum_*
//computationally irrelevant

//Till:
//0.93 alpha:	3.8.2006
//fixed bug in computation of USDA-silt content (computationally relevant)

//Till:
//0.92 alpha:	1.8.2006
//fixed bug in output of unreferenced soils (computationally irrelevant)

//Till:
//0.91 alpha:	27.2.2006
//version check and warning counter included

//Till:
//0.9 alpha:	20.12.2005
//include settings.php for user settings

//compute the MUSLE-K factor according to Williams (1995) from particle size distribution and organic matter content
//insert calculated values into database tabel soil_veg_components

//see make_wasa_input_manual.txt for details (yet to be done)
//Till Francke, till@comets.de
//-------------------------------------------------------------

include("sql_lib_odbc.php");	//include ODBC-support
global $sql_err_msg;

include ("settings.php");	//include user settings


if (!$con)
  die ("$sql_err_msg: could not connect to odbc-database, quitting.");

$warnings=0;

$db_ver_exp=7;		//this script expects a database of version 14
include("check_db_version.php"); //check data-dase version



echo("\ncheck availability of all soils referenced in soil_veg_components...");
$sql =  "SELECT soil_veg_components.pid AS svc_pid, soil_id".
" FROM soil_veg_components LEFT JOIN soils ON soil_veg_components.soil_id=soils.pid".
" WHERE soils.pid is NULL";
//look for soils that are referenced in "soil_veg_components", but not contained in "soils"

$res = sql_query($sql);  
if(!$res)
	die("\nCould not read one of the relevant tables ($sql_err_msg).");
else
{
	if ($row = sql_fetch_array($res))
	{	
		print("\nWARNING: \"soil_veg_components\" contains soils not contained in \"soils\"\n");
		do 
		{
			print(" SVC: ".$row["svc_pid"]." soil: ".$row["soil_id"]."\n");
		}
		while($row = sql_fetch_array($res));		//do for all soils found
	}
	else print("OK\n");
}

echo("\ncheck availability of particle information of all soils referenced in soil_veg_components...");
$sql =  "SELECT soil_veg_components.pid AS svc_pid, soil_veg_components.soil_id".
" FROM soil_veg_components LEFT JOIN r_soil_contains_particles ON r_soil_contains_particles.soil_id=soil_veg_components.soil_id".
" WHERE r_soil_contains_particles.soil_id is NULL";
//look for soils that are referenced in "soil_veg_components", but not contained in "r_soil_contains_particles"

$res = sql_query($sql);  
if(!$res)
	die("\nCould not read one of the relevant tables ($sql_err_msg).");
else
{
	if ($row = sql_fetch_array($res))
	{	
		print("\nWARNING: \"soil_veg_components\" contains soils unknown in \"r_soil_contains_particles\"\n");
		do
		{
			print(" SVC: ".$row["svc_pid"]." soil: ".$row["soil_id"]."\n");
		}
		while($row = sql_fetch_array($res));		//do for all soils found
	}
	else print("OK\n");
}


//these are the particle size classes that are converted to (USDA-soil classification), used by Williams (1995)
$clay_upper_limit=0.002;
$silt_upper_limit=0.05;
$sand_upper_limit=2.0;

//compute clay fraction according to USDA-----------------------------------------------------
$sql =  "select class_id, upper_limit from particle_classes where upper_limit>=$clay_upper_limit order by upper_limit";
//get ID of class that is at or just over usda clay
$res = sql_query($sql);  
if(!$res)
	die("Could not update relevant tables ($sql_err_msg).");
if (!($row = sql_fetch_array($res)))		
	die("All specified particle classes are finer than USDA-clay, cannot compute K-factor.");

$class_above_usda_clay=$row["class_id"];		//get the id of the user-defined class that sits just above USDA-clay
$class_above_usda_clay_limit=$row["upper_limit"];	//get uppper limit of respective class


$sql =  "DROP TABLE t_cum_above;";
$res = sql_query($sql);  	//delete any existing table

$sql =  "SELECT soil_id, sum(fraction) AS a_cum_above INTO t_cum_above".
" FROM r_soil_contains_particles WHERE class_id<=$class_above_usda_clay GROUP BY soil_id;";




$res = sql_query($sql);  	//produce a table containing the cumulative fractions up to $class_above_usda_clay for each soil
if(!$res)
	die("Table t_cum_above could not be created ($sql_err_msg).");



$sql =  "DROP TABLE a_cum_below;";
$res = sql_query($sql);  	//delete any existing table


$class_below_usda_clay=$class_above_usda_clay-1;		//the class below USDA clay
if ($class_below_usda_clay==0)	//no lower classes that can be used as a point for interpolation
{
	$class_below_usda_clay_limit=0; //interpolation starts at 0
	$sql="SELECT soil_id, 0 AS a_cum_below INTO t_cum_below".
	" FROM r_soil_contains_particles".
	" GROUP BY soil_id";		//sql-statement that produces a table containing zeros for each soil
}
else	//use the nearest lower class as a point for interpolation
{
	$sql =  "select upper_limit from particle_classes where class_id<=$class_below_usda_clay";
	$res = sql_query($sql);  
	if(!$res)
		die("Could not update relevant tables ($sql_err_msg).");
	if (!($row = sql_fetch_array($res)))
		die("USER-Class below USDA-clay not found.");
		$class_below_usda_clay_limit=$row["upper_limit"];	//get uppper limit of respective class
	$sql="SELECT soil_id, sum(fraction) AS a_cum_below  INTO t_cum_below".
	" FROM r_soil_contains_particles".
	" WHERE class_id<=$class_below_usda_clay".
	" GROUP BY soil_id";		//sql-statement that produces a table containing the cumulative fractions up to $class_below_usda_clay for each soil
}

$res = sql_query($sql);  	//produce a table containing the cumulative fractions up to $class_below_usda_clay for each soil
if(!$res)
	die("Table t_cum_below could not be created ($sql_err_msg).");



$sql =  "UPDATE soils SET a_clay=0,a_silt=0,a_sand=0";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not initialise USDA-fractions content ($sql_err_msg).");



print("\ncomputing USDA-clay-content...");
$sql =  "UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id".
" SET a_clay = ((a_cum_above-a_cum_below)*($clay_upper_limit-$class_below_usda_clay_limit))/($class_above_usda_clay_limit-$class_below_usda_clay_limit);";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute USDA-clay content ($sql_err_msg).");
else
print("OK\n");
//die();


//compute silt fraction according to USDA-----------------------------------------------------
$sql =  "select class_id, upper_limit from particle_classes where upper_limit>=$silt_upper_limit order by upper_limit";
//get ID of class that is at or just over usda silt
$res = sql_query($sql);  
if(!$res)
	die("Could not read relevant tables ($sql_err_msg).");
if (!($row = sql_fetch_array($res)))		
	die("All specified particle classes are finer than USDA-silt, cannot compute K-factor.");

$class_above_usda_silt=$row["class_id"];		//get the user-class that sits just above USDA-silt
$class_above_usda_silt_limit=$row["upper_limit"];	//get uppper limit of respective class

$sql =  "DROP TABLE t_cum_above;";
$res = sql_query($sql);  	//delete any existing table
if(!$res)
	print("Table t_cum_above could not be deleted ($sql_err_msg).");

$sql =  "SELECT soil_id, sum(fraction) AS a_cum_above INTO t_cum_above".
" FROM r_soil_contains_particles WHERE class_id<=$class_above_usda_silt GROUP BY soil_id;";
$res = sql_query($sql);  	//produce a table containing the cumulative fractions up to $class_above_usda_silt for each soil
if(!$res)
	die("Table t_cum_above could not be created ($sql_err_msg).");


$sql =  "DROP TABLE t_cum_below;";
$res = sql_query($sql);  	//delete any existing table
if(!$res)
	print("Table t_cum_below could not be deleted ($sql_err_msg).");


$class_below_usda_silt=$class_above_usda_silt-1;		//the class below USDA silt
if ($class_below_usda_silt==0)	//no lower classes that can be used as a point for interpolation
{
	$class_below_usda_silt_limit=0; //interpolation starts at 0
	$sql="SELECT soil_id, 0 AS a_cum_below INTO t_cum_below".
	" FROM r_soil_contains_particles".
	" GROUP BY soil_id";		//sql-statement that produces a table containing zeros for each soil
}
else	//use the nearest lower class as a point for interpolation
{
	$sql =  "select upper_limit from particle_classes where class_id=$class_below_usda_silt"; //?
	$res = sql_query($sql);  
	if(!$res)
		die("Could not update relevant tables ($sql_err_msg).");
	if (!($row = sql_fetch_array($res)))
		die("USER-Class below USDA-silt not found.");
		$class_below_usda_silt_limit=$row["upper_limit"];	//get uppper limit of respective class
	$sql="SELECT soil_id, sum(fraction) AS a_cum_below  INTO t_cum_below".
	" FROM r_soil_contains_particles".
	" WHERE class_id<=$class_below_usda_silt".
	" GROUP BY soil_id";		//sql-statement that produces a table containing the cumulative fractions up to $class_below_usda_silt for each soil
}

$res = sql_query($sql);  	//produce a table containing the cumulative fractions up to $class_below_usda_silt for each soil
if(!$res)
	die("Table t_cum_below could not be created ($sql_err_msg).");

/*
print("computing USDA-silt-content...");
$sql =  "UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id".
" SET a_silt = ((a_cum_above-a_cum_below)*($silt_upper_limit-$class_below_usda_silt_limit))/($class_above_usda_silt_limit-$class_below_usda_silt_limit);";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute USDA-silt content ($sql_err_msg).");
else
*/
print("OK\n");



//compute sand fraction according to USDA-----------------------------------------------------
$sql =  "select class_id, upper_limit from particle_classes where upper_limit>=$sand_upper_limit order by upper_limit";
//get ID of class that is at or just over usda sand
$res = sql_query($sql);  
if(!$res)
	die("Could not update relevant tables ($sql_err_msg).");
if (!($row = sql_fetch_array($res)))		
	die("All specified particle classes are finer than USDA-sand, cannot compute K-factor.");

$class_above_usda_sand=$row["class_id"];		//get the user-class that sits just above USDA-sand
$class_above_usda_sand_limit=$row["upper_limit"];	//get uppper limit of respective class

$sql =  "DROP TABLE t_cum_above;";
$res = sql_query($sql);  	//delete any existing table
if(!$res)
	print("Table t_cum_above could not be deleted ($sql_err_msg).");

$sql =  "SELECT soil_id, sum(fraction) AS a_cum_above INTO t_cum_above".
" FROM r_soil_contains_particles WHERE class_id<=$class_above_usda_sand GROUP BY soil_id;";
$res = sql_query($sql);  	//produce a table containing the cumulative fractions up to $class_above_usda_sand for each soil
if(!$res)
	die("Table t_cum_above could not be created ($sql_err_msg).");


$sql =  "DROP TABLE t_cum_below;";
$res = sql_query($sql);  	//delete any existing table
if(!$res)
	print("Table t_cum_below could not be deleted ($sql_err_msg).");


$class_below_usda_sand=$class_above_usda_sand-1;		//the class below USDA sand
if ($class_below_usda_sand==0)	//no lower classes that can be used as a point for interpolation
{
	$class_below_usda_sand_limit=0; //interpolation starts at 0
	$sql="SELECT soil_id, 0 AS a_cum_below INTO t_cum_below".
	" FROM r_soil_contains_particles".
	" GROUP BY soil_id";		//sql-statement that produces a table containing zeros for each soil
}
else	//use the nearest lower class as a point for interpolation
{
	$sql =  "select upper_limit from particle_classes where class_id<=$class_below_usda_sand";
	$res = sql_query($sql);  
	if(!$res)
		die("Could not update relevant tables ($sql_err_msg).");
	if (!($row = sql_fetch_array($res)))
		die("USER-Class below USDA-sand not found.");
		$class_below_usda_sand_limit=$row["upper_limit"];	//get uppper limit of respective class
	$sql="SELECT soil_id, sum(fraction) AS a_cum_below  INTO t_cum_below".
	" FROM r_soil_contains_particles".
	" WHERE class_id<=$class_below_usda_sand".
	" GROUP BY soil_id";		//sql-statement that produces a table containing the cumulative fractions up to $class_below_usda_sand for each soil
}

$res = sql_query($sql);  	//produce a table containing the cumulative fractions up to $class_below_usda_sand for each soil
if(!$res)
	die("Table t_cum_below could not be created ($sql_err_msg).");


print("computing USDA-sand-content...");
$sql =  "UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id".
" SET a_sand = ((a_cum_above-a_cum_below)*($sand_upper_limit-$class_below_usda_sand_limit))/($class_above_usda_sand_limit-$class_below_usda_sand_limit);";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute USDA-sand content ($sql_err_msg).");
else
print("OK\n");

$sql =  "UPDATE (soils LEFT JOIN t_cum_above ON soils.pid=t_cum_above.soil_id) LEFT JOIN t_cum_below ON soils.pid=t_cum_below.soil_id".
" SET a_silt=1-a_sand-a_clay;";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not set USDA-silt content ($sql_err_msg).");
else
print("OK\n");



print("computing f_cl_si factor...");
$sql =  "UPDATE soils SET a_f_cl_si = 1 WHERE a_silt=0;";		//formula doesn't work for zero silt content - factor is set to 1
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute f_cl_si factor ($sql_err_msg).");

$sql =  "UPDATE soils SET a_f_cl_si = exp(0.3*log(a_silt/(a_clay+a_silt)))".
" WHERE a_silt>0;";		
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute f_cl_si factor ($sql_err_msg).");
else
print("OK\n");


print("computing f_csand factor...");
//$sql =  "UPDATE soils SET a_f_csand = (0.2+(0.3*exp(-0.256*a_sand*100*(1-a_silt))));";		//as cited in SWAT manual
$sql =  "UPDATE soils SET a_f_csand = (0.2+(0.3*exp(-0.0256*a_sand*100*(1-a_silt))));";		//as in Williams, 1995
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute f_csand factor ($sql_err_msg).");
else
print("OK\n");

print("computing f_hisand factor...");
$sql =  "UPDATE soils SET a_f_hisand = (1-(0.7*(1-a_sand)/((1-a_sand)+exp(-5.51+22.9*(1-a_sand)))));";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute f_hisand factor ($sql_err_msg).");
else
print("OK\n");

print("computing f_orgc factor...");
$sql =  "UPDATE soils SET a_f_orgc = (1-(0.25*b_om/1.72*100/(b_om/1.72*100+exp(3.72-2.95*b_om/1.72*100))));";
$res = sql_query($sql);  
if(!$res)
	die("\nCould not compute f_orgc factor ($sql_err_msg).");
else
print("OK\n");


//compute K and insert results into table soil-----------------------------------------------------
print("computing MUSLE-K and inserting into \"soil\"...");
$sql =  "UPDATE soils SET a_musle_k = a_f_csand*a_f_cl_si*a_f_orgc*a_f_hisand;";
$res = sql_query($sql);  
if(!$res)
	die("Could not update MUSLE-K ($sql_err_msg).");
else
print("OK\n");

//compute insert K into table soil_veg_components-----------------------------------------------------
print("inserting MUSLE-K into \"soil_veg_components\"...");
$sql =  "UPDATE soil_veg_components INNER JOIN soils ON soils.pid=soil_veg_components.soil_id SET musle_k = a_musle_k;";
$res = sql_query($sql);  
if(!$res)
	die("Could not update MUSLE-K ($sql_err_msg).");
else
print("OK\n");


$sql =  "DROP TABLE t_cum_above;";
$res = sql_query($sql);  	//delete  table
if(!$res)
	print("Table t_cum_above could not be deleted ($sql_err_msg).");

$sql =  "DROP TABLE t_cum_below;";
$res = sql_query($sql);  	//delete table
if(!$res)
	print("Table t_cum_below could not be deleted ($sql_err_msg).");



echo("\nfinished script, $warnings warnings issued.\n");
die();


?>
