<?php
//create WASA input files (ASCII) from a given database
//see make_wasa_input_manual.doc for details
//Till Francke, till@comets.de
//-------------------------------------------------------------

$script_ver=1.33;

// Till (1.33): 30.3.2015: correctly check sum of SVC-fractions considering rocky fraction of TC
// Till (1.32): 27.3.2015: write meta info into "info.dat"
// Till (1.31): 26.3.2015: fixed typo "lenght" to "length"
// Till (1.3): 20.11.2014: generate rainy_seasons.dat and *_seasons.dat
// Till (1.22): 15.7.2014: added missing \n in generation of do.dat
// Till (1.21): 7.10.2013: consideration of optional columns beta_fac, sdr_lu, sdr_tc
// Till (1.20): 14.6.2012: added optional line to do.dat
// Till (1.19): allowed "9999" and "-9999" as denoting outlet basin in table subbasin

// Till (1.18): 29.4.2009
//improved handling of nonexistent output directories

// Till:
//1.17 25.9.2008
//handle seasonality of c-factor

// Till:
//1.16 13.8.2008
//write script version into header of do.dat (instead of db_version)

// Till:
//1.15 12.8.2008
//set maximum execution time of script to one hour

// Till:
//1.14 20.5.2008
//minor cosmetics, improved consistency check when making soil_vegetation

// Till:
//1.13 29.1.2008
//irrelevant TCs appeared in terrain.dat, fixed; formatting soil.dat

// Till:
//1.12 25.10.2007
//fixed faulty correction of SVC-in-TC-fraction, added optianal automatic correction for fraction of TC in LU

// Till 10.10.2007
//refined warning system

//Doro:
//1.11:16.8.07
//set flag to automatically normalize fractions of lus in a subbasin
//Till:
//1.1 :8.5.2007
//expects db-version 7 (runs with version 4 till 9)
//TCs in db are numbered from bottom to top - WASA expects otherwise; fixed this wrong order scheme

//Till:
//1.04 :17.1.2007
//expects db-version 7 (runs with version 4 till 8)
//fraction values rounded to 3 decimals in svc_in_tc.dat

//Till:
//1.03 alpha: 7.8.2006
//expects db-version 7 (runs with version 4 till 7)

//Till:
//1.02 alpha: 3.8.2006
//optional autocorrection if fractions of SVCs in TC don't sum up to 1
//optional calculation of stream order
//output of routing.dat
//subbasins in hymo.dat is ordered in the same scheme as in routing.dat
//script file version is written to do.dat
//introduced tolerance for sum checking in particle classes (0.01)

//Till:
//1.01 alpha: 16.3.2006
//create destination directory, if nonexistent

//Till:
//1 alpha: 27.2.2006
//check database format (version number)
//count warnings issued

//Till:
//0.997 alpha: 20.12.2005
//fixed bug in output of vegetation.dat (multiple listings)
//cosmetics in file headers (./templ-files)
//user settings outsourced to settings.php

//Till:
//0.996 alpha: 1.12.2005
//printed unset flags (bedrock, gw_flag, shrinks) as 0 (not empty string)
//fixed error in SQL-statement for particle_class.dat
//add alluvial flag to soil.dat

//Till:
//0.995 alpha: 9.11.2005
//particle_classes now contains an entry 0 for organic matter, which is only for clarity in reference, but omitted in the creation of part_class.dat

//Till:
//0.994 alpha: 10.10.2005
//bug fixed in terrain.dat: some TCs were listed multiple times
//modified fraction check, now done with SQL

//Till:
//0.993 alpha: 28.9.2005
//changed horizon_particles.dat to soil_particles.dat

//0.992 alpha: 26.9.2005
//fixed serious error in formatting hymo.dat
//improved frac_check handling sum of fractions coming close to 1

//0.991 alpha: 23.9.2005
//create horizon_particles.dat
//improved program screen-output

//0.99 alpha: 21.9.2005
//create part_class.dat, number of classes inserted to do.dat

//0.98 alpha: 25.8.2005
//modified structure of do.dat

//0.97 alpha: 23.8.2005
//output directory specifyable in dest_dir

//0.96 alpha: 10.8.2005
//svc.dat now also contains manning's n roughness coefficient

//0.95 alpha: 9.8.2005
//soil.dat contained the same soils several times - fixed
//error in counting total number of LUs, TCs, soils and vegs - fixed

//0.94 alpha: 12.7.2005
//svc.dat now contains also the coarse fraction (optional identical with the coarse fraction of the top soil layer of respective soil)

//0.93 alpha: 11.7.2005
//create maxdim.dat containing maximum dimensions of spatial variables (not yet used in WASA)
//fixed several bugs (counting of spatial units)

//0.92 alpha: 8.7.2005
//improved integrity checking (sum of fractions)
//create svc.dat containing svc information and MUSLE factors (not yet used in WASA)
//create svc_in_tc.dat containing which svc are contained in which tc (not yet used in WASA)
//create do.dat

//0.91 alpha: 7.7.2005
//create svc.dat containing svc information and MUSLE factors

//0.9 alpha:	7.7.2005
//-connects to Access, MySQL or excel via ODBC
//-do partial consistency checks of db (sum(frac)=1, etc.)

set_time_limit(3600);		//set maximum execution time of script to one hour

include("sql_lib_odbc.php");	//include ODBC-support
global $sql_err_msg;

include ("settings.php");	//include user settings

$warnings=0;			//count warnings issued during run of script

if (!$con)
  die ("$sql_err_msg: could not connect to odbc-database, quitting.");


$db_ver_exp=18;		//this script expects a database of version 18
include("check_db_version.php"); //check data-dase version

$dirmake=1;
if(!is_dir($dest_dir))
{
	print("destination directory \"$dest_dir\" not found.");
	$dirmake*=mkdir($dest_dir);	//create output directories
}

if(!is_dir($dest_dir."hillslope")) $dirmake*=mkdir($dest_dir."hillslope");	//create output directories
if(!is_dir($dest_dir."river"))     $dirmake*=mkdir($dest_dir."river");	//create output directories
if ($dirmake)
	print(" \"$dest_dir\" created.\n\n");
else
	die(" \"$dest_dir\" could not be created.\n");

//make info.dat-----------------------------------------------------
$dest=$dest_dir."info.dat";
echo("\ncreating $dest...\n");

$fid=fopen($dest,"w");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT max(pid) as ver,max(mod_date) as mdate from meta_info";
$res = sql_query($sql);
if(!$res)
die("Could not read table meta_info ($sql_err_msg).");

$row = sql_fetch_array($res);
//write meta_info
fwrite($fid, "generated on ".date('Y-m-d H:i:s', time())." with make_wasa_input.php, version ".$script_ver."\n");
fwrite($fid, "from database version ".$row["ver"]." of ".$row["mdate"]."\n");
fclose($fid);
echo("finished writing $dest.\n\n");
	
	
//make routing.dat-----------------------------------------------------
$source="./templ/routing.dat";
$dest=$dest_dir."river/routing.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");


if ($compute_stream_order)
{
	echo(" computing stream order, column a_stream_order will be overwritten\n");
	$sql =  "UPDATE subbasins SET a_stream_order = NULL";	//empty whole column
	$res = sql_query($sql);
	if(!$res)
	die("Could not update table subbasins ($sql_err_msg).");

	$sql =  "UPDATE subbasins SET a_stream_order = 1 WHERE (drains_to=9999) OR (drains_to=-9999);";	//mark outlet basins
	$res = sql_query($sql);
	if(!$res)
	die("Could not update table subbasins ($sql_err_msg).");

	for ($i=1;$i<100;$i++)
	{
		$sql="UPDATE subbasins AS a LEFT JOIN subbasins AS b ON a.drains_to=b.pid SET a.a_stream_order = ".($i+1)." WHERE (b.a_stream_order=$i);";
		//update next order basins
		$res = sql_query($sql);
		if(!$res)
		die("Could not update table subbasins ($sql_err_msg).");

		$sql =  "SELECT pid from subbasins WHERE (a_stream_order IS NULL);";
		//check if already finished
		$res = sql_query($sql);
		if (sql_num_rows($res)==0)
		break;			//all subbasins treated

	}
	if ($i==100)
		echo(" There was a problem calculation the stream order. Please check column a_stream_order in subbasins\n");
}
else
	echo(" using existing column a_stream_order\n");

$sql =  "SELECT pid,drains_to from subbasins ORDER BY a_stream_order DESC,pid";
$res = sql_query($sql);
if(!$res)
die("Could not read table subbasins ($sql_err_msg).");

echo (" selection completed, ".sql_num_rows($res)." subbasins selected.\n");
$i=1;
while($row = sql_fetch_array($res))		//do for all subbasins found
{
	//write subbasin info
	fwrite($fid, ($i++)."\t".$row["pid"]."\t".$row["drains_to"]."\n");
}

fclose($fid);
echo("finished writing $dest.\n\n");

//make response.dat-----------------------------------------------------
$source="./templ/response.dat";
$dest=$dest_dir."river/response.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT pid,lag_time,retention from subbasins ORDER BY a_stream_order DESC,pid";
$res = sql_query($sql);
if(!$res)
die("Could not read table subbasins ($sql_err_msg).");

echo (" selection completed, ".sql_num_rows($res)." subbasins selected.\n");
$i=1;
while($row = sql_fetch_array($res))		//do for all subbasins found
{
	//write subbasin info
	fwrite($fid, $row["pid"]."\t".$row["lag_time"]."\t".$row["retention"]."\n");
}

fclose($fid);
echo("finished writing $dest.\n\n");



//make hymo.dat-----------------------------------------------------
$source="./templ/hymo.dat";
$dest=$dest_dir."hillslope/hymo.dat";


echo("creating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "select pid,area from subbasins ORDER BY a_stream_order DESC,pid";
$res = sql_query($sql);

if(!$res)
die("Could not read table subbasins ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." subbasins selected.\n");
	if ($print_process)
	echo (" treating no. ...");
	while($row = sql_fetch_array($res))		//do for all subbasin found
	{
		if ($print_process)
		echo(" ".$row["pid"]);
		$sql =  "select lu_id,fraction from r_subbas_contains_lu where subbas_id=".$row["pid"]." order by lu_id" ; // get LUs contained in this subbasin
		$res2 = sql_query($sql);
		if(!$res2)
		die("Could not read one of the relevant tables ($sql_err_msg).");
		$no_lu=sql_num_rows($res2);	//count LUs found in this subbasin
		if ($no_lu==0)
		{
			echo("WARNING: Subbasin ".$row["pid"]." contains no landscape units. Skipped.\n");
			$warnings++;
			continue;
		}
		fwrite($fid, $row["pid"]."\t".$row["area"]."\t");
		fwrite($fid, $no_lu);		//write number of contained LUs
		$frac_check=0;		//for checking if all fraction-fields sum up to 1
		while($row2 = sql_fetch_array($res2))
		{
			fwrite($fid, "\t".$row2["lu_id"]);		//write IDs of contained LUs
			$frac_check+=$row2["fraction"];
		}

		$norm_fac=1;
		if (abs($frac_check-1) > 0.01)
		{
			echo("WARNING: Sum of fractions of LUs for subbasin ".$row["pid"]." = $frac_check, but should be 1.");
			
			if ($normalize_lus_in_subbas)		//automatic normalisation of fractions enabled?
			{
				$norm_fac=1/$frac_check;
				echo(" (automatic correction enabled)");
			}
			else
			$warnings++;
			echo(".\n");
		}

		sql_data_seek($res2,0);
		$dummy="";
		while($row2 = sql_fetch_array($res2))
		{
			$dummy.="\t".$row2["fraction"]*$norm_fac;			//assemble string containing fractions of LUs
		}

		fwrite($fid, "$dummy\n");		//write fractions, newline -> end of record
	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n\n");


//make soter.dat-----------------------------------------------------
$source="./templ/soter.dat";
$dest=$dest_dir."hillslope/soter.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT DISTINCT landscape_units.* FROM landscape_units INNER JOIN r_subbas_contains_lu ON r_subbas_contains_lu.lu_id=landscape_units.pid;";
	//only get those LUs that are part of any of the subbasins
$res = sql_query($sql);

if(!$res)
die("Could not read table landscape_units ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." LUs found.\n");
	if ($print_process)
	echo (" treating no. ...");
	while($row = sql_fetch_array($res))		//do for all landscape units found
	{
		
		if ($print_process)
			echo (" ".$row["pid"]);
		$sql =  "select tc_id,fraction from r_lu_contains_tc where lu_id=".$row["pid"]." order by position desc" ; // get all TCs contained in this LU
		$res2 = sql_query($sql);
		if(!$res2)
			die("Could not read one of the relevant tables ($sql_err_msg).");
		$no_tc=sql_num_rows($res2);	//count TCs found in this LU
		if ($no_tc==0)
		{
			echo("WARNING: Landscape unit ".$row["pid"]." contains no terrain components. Skipped.\n");
			$warnings++;
			continue;
		}
		
		fwrite($fid, $row["pid"]."\t".$no_tc);
		while($row2 = sql_fetch_array($res2))
		{
			fwrite($fid, "\t".$row2["tc_id"]);		//write IDs of contained TCs
		}
		if (!$row["gw_flag"]) $row["gw_flag"]="0";
		fwrite($fid, "\t".$row["kf_bedrock"]."\t".$row["length"]."\t".$row["soil_depth"]."\t".$row["allu_depth"]."\t".$row["riverbed_depth"]."\t".$row["gw_flag"]."\t".$row["gw_dist"]."\t".$row["frgw_delay"]."\t"."\n");		//newline, end of record

	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n");


//write sdr_lu.dat (optional)
	$dest=$dest_dir."hillslope/sdr_lu.dat";
	unlink($dest); 
	if ($db_ver_cur >= 15)
	{
		$sql =  "SELECT pid, sdr_lu FROM landscape_units where not sdr_lu=1"; // check if any sdr other than 1 is specified
		$res2 = sql_query($sql);
		if (sql_num_rows($res2)>0)
		{
			$source="./templ/sdr_lu.dat";
			$dest=$dest_dir."hillslope/sdr_lu.dat";
			echo("\ncreating $dest...\n");
			if(!copy ($source,$dest ))
			die("template file $source not found.");

			$fid=fopen($dest,"a");
			if(!$fid) die("Could not write to $dest");
			while($row = sql_fetch_array($res2))		//do for all landscape_units found
				fwrite($fid, $row["pid"]."\t".$row["sdr_lu"]."\n");
			fclose($fid);
		}
	} 

//make terrain.dat-----------------------------------------------------
$source="./templ/terrain.dat";
$dest=$dest_dir."hillslope/terrain.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");


$sql =  "drop table temp_table"; 	//drop temporary table
$res=sql_query($sql);

$sql =  "SELECT lu_id, max(position) as max_pos into temp_table FROM r_lu_contains_tc GROUP BY lu_id;";
	//determine maximum number of terrain components in each LU and write to temporary table
if(!($res = sql_query($sql)))
	die("Could not write temp_table ($sql_err_msg).");

	if ($db_ver_cur >= 15) #these columns are not available in lower db-version
	{
		// check if any sdr other than 1 is specified
		$sql =  "SELECT sdr FROM terrain_components where not sdr=1";
		$res2 = sql_query($sql);
		$include_sdr = (sql_num_rows($res2)>0); #flag if sdr needs to be included in query
		
		// check if any beta_fac other than 1 is specified
		$sql =  "SELECT beta_fac FROM terrain_components where not beta_fac=1";
		$res2 = sql_query($sql);
		$include_beta_fac = $include_sdr | (sql_num_rows($res2)>0); #flag if beta_fac needs to be included in query
	}
	else
	$include_sdr = $include_beta_fac = FALSE;
	
	//write header
	fwrite($fid, "TC-ID	fraction	slope[%]	position[-]");
	if ($include_beta_fac) fwrite($fid, "	beta_fac[-]");
	if ($include_sdr)      fwrite($fid, "	SDR[-]");
	fwrite($fid, "\n");
		

$sql =  "SELECT terrain_components.pid, slope, fraction, position, max_pos";
if ($include_beta_fac) $sql .= ", beta_fac";
if ($include_sdr)      $sql .= ", sdr";
$sql .= ", r_lu_contains_tc.lu_id as lu_id, (max_pos-position+1) AS wasa_position
FROM (r_lu_contains_tc LEFT JOIN terrain_components ON r_lu_contains_tc.tc_id=terrain_components.pid) LEFT JOIN temp_table ON r_lu_contains_tc.lu_id=temp_table.lu_id
where terrain_components.pid IS NOT NULL ORDER BY r_lu_contains_tc.lu_id, (max_pos-position+1);";

$res = sql_query($sql);

$curr_lu_id=0;		//ID of LU currently treated

if(!$res)
	die("Could not read table terrain_components ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." TCs selected.\n");

	if ($print_process)
		echo (" treating no. ...");
	
	while($row = sql_fetch_array($res))		//do for all terrain components found
	{
		if ($print_process)
			echo (" ".$row["pid"]);
			
		if ($curr_lu_id!=$row["lu_id"])		//check if this tc belongs to a new lu
		{
				$curr_lu_id=$row["lu_id"];
				$sql =  "SELECT lu_id, sum(fraction) AS summ FROM r_lu_contains_tc where lu_id=".$curr_lu_id." GROUP BY lu_id ";
				// check if all fractions of all TCs of a LU sum up to 1
				$res2 = sql_query($sql);

				if(!$res2)
					die("Could not read table r_lu_contains_tc ($sql_err_msg).");
				else
				{
					$row2 = sql_fetch_array($res2);
					$norm_fac=1;
					if (abs($row2["summ"]-1)>0.01)
					{
						echo("\nWARNING: Sum of fractions for lu ".$row2["lu_id"]." = ".$row2["summ"].", but should be 1.\n  ");
						if ($normalize_tc_in_lu)		//automatic normalisation of fractions enabled?
						{
							$norm_fac=1/$row2["summ"];
							echo(" (automatic correction enabled)");
						}
						else
						$warnings++;
						echo(".\n");
					}
				}
		}
			
		fwrite($fid, $row["pid"]."\t".$row["fraction"]/$norm_fac."\t".$row["slope"]."\t".$row["wasa_position"]);
		if ($include_beta_fac) fwrite($fid, "\t".$row["beta_fac"]);
		if ($include_sdr)      fwrite($fid, "\t".$row["sdr"]);
		fwrite($fid, "\n");

	}
}
fclose($fid);

$sql =  "drop table temp_table"; 	//drop temporary table
sql_query($sql);


echo("\nfinished writing $dest.\n\n");


//make soil_vegetation.dat-----------------------------------------------------
$source="./templ/soil_vegetation.dat";
$dest=$dest_dir."hillslope/soil_vegetation.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT r_subbas_contains_lu.subbas_id, r_lu_contains_tc.lu_id, r_lu_contains_tc.tc_id, frac_rocky".
" FROM (r_subbas_contains_lu INNER JOIN r_lu_contains_tc ON r_subbas_contains_lu.lu_id=r_lu_contains_tc.lu_id)".
" INNER JOIN terrain_components ON r_lu_contains_tc.tc_id=terrain_components.pid order by r_subbas_contains_lu.subbas_id,r_lu_contains_tc.lu_id, r_lu_contains_tc.tc_id";
$res = sql_query($sql);

if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." TCs selected.\n");
	if ($print_process)
	echo (" treating no. ...");
	while($row = sql_fetch_array($res))		//do for all TCs found
	{
		if ($print_process)	echo (" ".$row["tc_id"]);

		$sql =  "SELECT fraction, soil_id, veg_id,r_tc_contains_svc.svc_id AS svc_id2 FROM r_tc_contains_svc LEFT JOIN soil_veg_components".
		" ON soil_veg_components.pid=r_tc_contains_svc.svc_id where tc_id=".$row["tc_id"]." ORDER BY fraction desc;";
			
		$res2 = sql_query($sql);
		if(!$res2)	die("\nCould not read one of the relevant tables ($sql_err_msg).");
		$no_svc=sql_num_rows($res2);	//count SVCs found in current TC
		if ($no_svc==0)
		{
			echo("\nWARNING: Terrain component ".$row["tc_id"]." contains no soil-vegetation components. Skipped.\n  ");
			$warnings++;
			continue;
		}
		
		$frac_check = $row["frac_rocky"];		//for checking if all fraction-fields sum up to 1
		
		while($row2 = sql_fetch_array($res2))		//sum up fractions
		{		
		$frac_check+=$row2["fraction"];
			if ($row2["soil_id"]=="" || $row2["veg_id"]=="")
				die("ERROR: Terrain component ".$row["tc_id"]." contains SVC ".$row2["svc_id2"].", which is not defined. Aborted.\n  ");

		}
		$norm_fac=1;				//normal case, no correction of fractions required
		if ((abs($frac_check-1) > 0.01) && ($normalize_svc_in_tc))		//automatic normalisation of fractions enabled?	// warning has been issued before for svc_in_tc.dat :echo("WARNING: Sum of fractions of SVCs for TC ".$row['tc_id']." = ".$frac_check.", but should be 1 ");
			$norm_fac=1/$frac_check;
	
		sql_data_seek($res2,0);		//reset result array
	
		//write soil info
		fwrite($fid, $row["subbas_id"]."\t".$row["lu_id"]."\t".$row["tc_id"]."\t".$row["frac_rocky"]."\t".$no_svc);
		while($row2 = sql_fetch_array($res2))
			fwrite($fid, "\t".$row2["soil_id"]);		//write IDs of contained soils
		fwrite($fid, "\n");		//end line with soil information, start newline

		//write vegetation info
		fwrite($fid, $row["subbas_id"]."\t".$row["lu_id"]."\t".$row["tc_id"]."\t".$row["frac_rocky"]."\t".$no_svc);

		sql_data_seek($res2,0);	//reset data pointer to retrieve vegetation info this time
		while($row2 = sql_fetch_array($res2))
			fwrite($fid, "\t".$row2["veg_id"]);		//write IDs of contained soils
		fwrite($fid, "\n");		//end line with vegetation information, start newline

		//write fraction info
		fwrite($fid, $row["subbas_id"]."\t".$row["lu_id"]."\t".$row["tc_id"]."\t".$row["frac_rocky"]."\t".$no_svc);
		sql_data_seek($res2,0);		//reset data pointer to retrieve vegetation info this time
		while($row2 = sql_fetch_array($res2))
			fwrite($fid, "\t".$row2["fraction"]*$norm_fac);		//write IDs of contained soils
		fwrite($fid, "\n");		//end line with vegetation information, start newline
	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n");


//make soil.dat-----------------------------------------------------
$source="./templ/soil.dat";
$dest=$dest_dir."hillslope/soil.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT DISTINCT soils.pid, bedrock_flag, alluvial_flag from soils INNER JOIN soil_veg_components".
	" ON soil_veg_components.soil_id=soils.pid order by soils.pid";
$res = sql_query($sql);

if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." soil-entries selected.\n");
	if ($print_process)
	echo (" treating no. ...");
	while($row = sql_fetch_array($res))		//do for all soils found
	{
		if ($print_process)
		echo (" ".$row["pid"]);

		$sql =  "SELECT * from horizons where soil_id=".$row["pid"]." ORDER BY position;";
		$res2 = sql_query($sql);
		if(!$res2)
		die("\nCould not read tables horizons ($sql_err_msg).");
		$no_hor=sql_num_rows($res2);	//count horizons found in current soil
		if ($no_hor==0)
		{
			echo("\nWARNING: Soil ".$row["pid"]." contains no horizons. Skipped.\n  ");
			$warnings++;
			continue;
		}

		//write soil info
		fwrite($fid, $row["pid"]."\t".$no_hor);
		while($row2 = sql_fetch_array($res2))
		{
			if (!$row2["shrinks"]) $row2["shrinks"]="0"; //use 0 instead of empty string
			fwrite($fid, "\t".$row2["theta_r"]."\t".$row2["theta_pwp"]."\t".$row2["fk"]."\t".$row2["fk63"]."\t".$row2["nfk"]."\t".$row2["theta_s"]."\t".$row2["depth"]);
			fwrite($fid, "\t".$row2["ks"]."\t".$row2["suction"]."\t".$row2["pore_size_i"]."\t".$row2["bubb_pres"]."\t".$row2["coarse_frag"]."\t".$row2["shrinks"]);
		}
		if (!$row["bedrock_flag"]) $row["bedrock_flag"]="0";	//use 0 instead of empty string
		if (!$row["alluvial_flag"]) $row["alluvial_flag"]="0";	//use 0 instead of empty string
		fwrite($fid, "\t".$row["bedrock_flag"]."\t".$row["alluvial_flag"]."\n");		//end line with horizon information, start newline
	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n");


//make vegetation.dat-----------------------------------------------------
$source="./templ/vegetation.dat";
$dest=$dest_dir."hillslope/vegetation.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT DISTINCT vegetation.pid, vegetation.* from vegetation INNER JOIN soil_veg_components".
	" ON soil_veg_components.veg_id=vegetation.pid order by vegetation.pid";

$res = sql_query($sql);

if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." vegetation-entries selected.\n");
	if ($print_process)
	echo (" treating no. ...");
	while($row = sql_fetch_array($res))		//do for all soils found
	{
		if ($print_process)
		echo (" ".$row["pid"]);
		//write veg info
		fwrite($fid, $row["pid"]."\t".$row["stomat_r"]."\t".$row["min_suction"]."\t".$row["max_suction"]."\t".$row["height1"]."\t".$row["height2"]."\t".$row["height3"]."\t".$row["height4"]."\t");
		fwrite($fid, $row["root_depth1"]."\t".$row["root_depth2"]."\t".$row["root_depth3"]."\t".$row["root_depth4"]."\t".$row["lai1"]."\t".$row["lai2"]."\t".$row["lai3"]."\t".$row["lai4"]."\t");
		fwrite($fid, $row["alb1"]."\t".$row["alb2"]."\t".$row["alb3"]."\t".$row["alb4"]."\n");
	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n");

//make svc_in_tc.dat-----------------------------------------------------
$source="./templ/svc_in_tc.dat";
$dest=$dest_dir."hillslope/svc_in_tc.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");


$sql =  "SELECT tc_id, sum(fraction) as sum_svc, max(frac_rocky) as frac_rocky_tc from r_tc_contains_svc INNER JOIN terrain_components".
	" ON terrain_components.pid=r_tc_contains_svc.tc_id group by tc_id order by tc_id";
	// do check if all fractions of all SVCs within a TC sum up to 1

$res = sql_query($sql);

if(!$res)
	die("Could not read tables r_tc_contains_svc/terrain_components ($sql_err_msg).");

	echo (" selection completed, ".sql_num_rows($res)." TCs selected.\n");

	if ($print_process)
	echo (" treating no. ...");

while($row = sql_fetch_array($res))
{
	$sql =  "SELECT tc_id,svc_id,fraction from r_tc_contains_svc INNER JOIN terrain_components".
	" ON terrain_components.pid=r_tc_contains_svc.tc_id where tc_id=".$row['tc_id']." order by fraction desc;";

	$res2 = sql_query($sql);

	if(!$res2)
		die("Could not read one of the relevant tables ($sql_err_msg).");

	$frac_check = $row["sum_svc"]+ $row["frac_rocky_tc"];		//for checking if all fraction-fields sum up to 1
	if (abs($frac_check-1) > 0.01)
	{
		echo("WARNING: Sum of fractions of SVCs for TC ".$row['tc_id']." = ".$frac_check.", but should be 1 ");
		if ($normalize_svc_in_tc)		//automatic normalisation of fractions enabled?
		{
			$norm_fac=1/$frac_check;
			echo(" (automatic correction enabled)");
		}
		else
			$warnings++;
		echo(".\n");
	}
	
	while($row2 = sql_fetch_array($res2))		//do for all SVCs found
	{
		if ($print_process)
		echo (" ".$row2["tc_id"]."/".$row2["svc_id"]);
		fwrite($fid, $row2["tc_id"]."\t".$row2["svc_id"]."\t".round($row2["fraction"]*$norm_fac,3)."\n");
	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n");

//make svc.dat-----------------------------------------------------
$source="./templ/svc.dat";
$dest=$dest_dir."hillslope/svc.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");


if ($coarse_fraction==0)
{
	$sql =  "SELECT DISTINCT pid, soil_veg_components.* FROM soil_veg_components".
	" INNER JOIN r_tc_contains_svc ON soil_veg_components.pid=r_tc_contains_svc.svc_id ORDER BY pid;";
	//get coarse_frac from soil_veg_components
}
else
{
	$sql =  "SELECT DISTINCT soil_veg_components.pid, soil_veg_components.*, horizons.coarse_frag AS coarse_frac".
	" FROM (soil_veg_components INNER JOIN r_tc_contains_svc ON soil_veg_components.pid=r_tc_contains_svc.svc_id) INNER JOIN horizons".
	" ON (soil_veg_components.soil_id=horizons.soil_id AND position=1)ORDER BY soil_veg_components.pid;";
	//get coarse_frag from topmost horizon from horizon table
}
$res = sql_query($sql);

if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).");
else
{
	echo (" selection completed, ".sql_num_rows($res)." SVCs selected.\n");
	
	$row = sql_fetch_array($res);
	$c_cols=4;			//expect 4 C-values
	
	if (array_key_exists('musle_c', $row))
	{
		$c_cols=1;			//expect one C-value
		$c_name_suffix="";	//to corectly address the column name
	}
	else
	{
		$res2 = sql_query("SELECT count(*) as cc FROM soil_veg_components where  (musle_c2 is not null) and  (musle_c3 is not null) and  (musle_c4 is not null);");
		if(!$res2)
			die("Could not read one of the relevant tables ($sql_err_msg).");
		$row2 = sql_fetch_array($res2);
		if ($row2["cc"]+1-1==0)			//additional C-columns are present, but are all empty
		{
			$c_cols=1;			//expect one C-value
			$c_name_suffix="1";	//to corectly address the column name
		}
	}
	
	//print header according to format
	fwrite($fid,"id	soil_id	veg_id	musle_k[(ton acre hr)/(acre ft-ton inch)]	");
	for ($i=1;$i<=$c_cols;$i++) fwrite($fid,"musle_c".$i."[-]	");	//repeat column headings, if necessary
	fwrite($fid,"musle_p[-]	coarse_fraction[%]	manning_n\n");
	if ($print_process)
	echo (" treating no. ...");
	sql_data_seek($res,0);		//reset array
	while($row = sql_fetch_array($res))		//do for all soils found
	{
		if ($print_process)
		echo (" ".$row["pid"]);
		//write SVC info
		fwrite($fid, $row["pid"]."\t".$row["soil_id"]."\t".$row["veg_id"]."\t".$row["musle_k"]."\t");
		if ($c_cols==1)	
			fwrite($fid, $row["musle_c".$c_name_suffix]."\t");
		else
			for ($i=1;$i<=$c_cols;$i++)
			{
				if ($row["musle_c".$i]=="") $row["musle_c".$i]=$row["musle_c1"];		//set empty fields to first value of season
				fwrite($fid,$row["musle_c".$i]."\t");	//repeat column 
			}	
		fwrite($fid, $row["musle_p"]."\t".$row["coarse_frac"]."\t".$row["manning_n"]."\n");
	}
}
fclose($fid);

echo("\nfinished writing $dest.\n\n");



//make do.dat-----------------------------------------------------
$source="./templ/do.dat";
$dest=$dest_dir."do.dat";
echo("\ncreating $dest...\n");

$fid=fopen($dest,"w");
if(!$fid) die("Could not write to $dest");

if(! file_exists($source))
	die("template file $source not found.");
else
	$templ=file_get_contents($source);

fwrite($fid,"v".$script_ver." ".$templ);		//write script version and contents of template file


$sql =  "SELECT count(*) AS s_count FROM (SELECT pid FROM subbasins INNER JOIN r_subbas_contains_lu".
	" ON subbasins.pid=r_subbas_contains_lu.subbas_id GROUP BY pid) AS tt;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	$row = sql_fetch_array($res);
	fwrite($fid,$row["s_count"]."\t//no. of sub-basins\n");
}

$sql =  "SELECT count(*) as  comb_count FROM (r_subbas_contains_lu INNER JOIN r_lu_contains_tc".
	" ON r_subbas_contains_lu.lu_id=r_lu_contains_tc.lu_id) INNER JOIN terrain_components ON r_lu_contains_tc.tc_id=terrain_components.pid;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	$row = sql_fetch_array($res);
	fwrite($fid,$row["comb_count"]."\t//no. of combinations of sub-basins, landscape units, terrain components (TC-instances)\n");
}

$sql =  "SELECT DISTINCT landscape_units.pid FROM landscape_units".
	" INNER JOIN r_subbas_contains_lu ON landscape_units.pid=r_subbas_contains_lu.lu_id";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	fwrite($fid,sql_num_rows($res)."\t//total no. of Landscape units in study area\n");
}

$sql =  "SELECT DISTINCT terrain_components.pid FROM terrain_components INNER JOIN r_lu_contains_tc ON r_lu_contains_tc.tc_id=terrain_components.pid;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	fwrite($fid,sql_num_rows($res)."\t//total no. of terrain components (types) in study area\n");
}

$sql =  "SELECT DISTINCT soils.pid from soils INNER JOIN soil_veg_components".
	" ON soil_veg_components.soil_id=soils.pid";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	fwrite($fid,sql_num_rows($res)."\t//total no. of soil components in study area\n");
}

$sql =  "SELECT DISTINCT vegetation.pid from vegetation INNER JOIN soil_veg_components".
	" ON soil_veg_components.veg_id=vegetation.pid";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	fwrite($fid,sql_num_rows($res)."\t//total no. of vegetation units in study area\n");
}


$sql =  "SELECT upper_limit FROM particle_classes ORDER BY upper_limit;";
$res = sql_query($sql);

if(!$res)
{
	print("Could not read particle_classes ($sql_err_msg). One class assumed.\n");
	$n_sed_class=1;
}
else
{
	$n_sed_class=sql_num_rows($res);
}



fwrite($fid,".f.	  //doreservoir: do reservoir calculations\n");
fwrite($fid,".f.   //doacudes:includes dam calculations\n");
fwrite($fid,".t.   //dolattc: do latflow between TCs\n");
fwrite($fid,".f.   //doalllattc: rout latflow compeletely to next downslope TC\n");
fwrite($fid,".t.   //dolatsc: do latflow within TCs (surface runoff)\n");
fwrite($fid,".t.   //dolatscsub: do latflow within TCs (subsurface runoff)\n");
fwrite($fid,".f.   //dotrans: do water transpositions betwen sub-basins\n");
fwrite($fid,".f.   //dohour: do hourly version\n");
fwrite($fid,"0     //scenario: choose scenario (0:less rain (ECHAM), 1:no trend, 2:more rain (Hadley))\n");
fwrite($fid,"0     //krig: type of precipitation interpolation (0:OK, 1:EDK, 2:EDKxyz, 3:csimabsed3, 4:csimreled3, 5:csimreled1, 6:csimabsed1, 7:statdata, 8:statdatacon, 9:gerstdatacon, 10:gerstdata, 11:ok_mean1cell)\n");
fwrite($fid,"15.0  //kfkorr:  hydraulic conductivity factor (for daily model version) (kfkorr)\n");
fwrite($fid,"0.30  //intcf: interception capacity per unit LAI (mm)\n");
fwrite($fid,"0     //dointc: type of interception routine (simple bucket:0, modified bucket:1)\n");
fwrite($fid,".f.   //doscale: do scaling due to rainfall interpolation ?\n");
fwrite($fid,".f.   //domuncell: for muni/ezg-nocell-version, use rainfall input derived from cells ? (change kf_calib.dat !)\n");
fwrite($fid,"1.    //sensfactor: factor for sensitivity studies\n");
fwrite($fid,"24	  //dt: time step in [hours]\n");
fwrite($fid,".f.   //dosediment\n");
fwrite($fid,"$n_sed_class	  //No. of grain size classes\n");
fwrite($fid,"1	  // type of sediment transport model at the hillslope	\n");
fwrite($fid,"1     // type of sediment transport model in the river (1) old WASA routing, (2) Muskingum\n");
fwrite($fid,"1     //type of sediment model in the reservoir: choose sediment transport equation (1:Wu et al., 2000; 2:Ashida and Michiue, 1973; 3: Yang, 1973 and 1984; 4: Ackers and White, 1973)\n");
fwrite($fid,".f. //load state of storages from files (if present) at start (optional)\n");
fwrite($fid,".f. //save state of storages to files after simulation period (optional)");

fclose($fid);
echo("finished writing $dest.\n\n");


//make maxdim.dat-----------------------------------------------------
$dest=$dest_dir."maxdim.dat";
echo("\ncreating $dest...\n");

$fid=fopen($dest,"w");
if(!$fid) die("Could not write to $dest");
fwrite($fid,"contains maximum dimensions of spatial units\n");

$sql =  "SELECT max(nlu) as max_nlu FROM (select count(*) AS nlu FROM r_subbas_contains_lu".
	" INNER JOIN landscape_units ON r_subbas_contains_lu.lu_id=landscape_units.pid GROUP BY subbas_id) as tt;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	$row = sql_fetch_array($res);
	fwrite($fid,$row["max_nlu"]."\t//maximum no. of landscape units in a sub-basins\n");
}

$sql =  "SELECT max(ntc) as max_ntc FROM (select count(*) AS ntc FROM r_lu_contains_tc".
	" INNER JOIN terrain_components ON r_lu_contains_tc.tc_id=terrain_components.pid GROUP BY lu_id) as tt;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	$row = sql_fetch_array($res);
	fwrite($fid,$row["max_ntc"]."\t//maximum no. of terrain components in a landscape unit\n");
}

$sql =  "SELECT max(nsvc) as max_nsvc FROM (select count(*) AS nsvc FROM r_tc_contains_svc".
	" INNER JOIN soil_veg_components ON r_tc_contains_svc.svc_id=soil_veg_components.pid GROUP BY tc_id) as tt;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	$row = sql_fetch_array($res);
	fwrite($fid,$row["max_nsvc"]."\t//maximum no. of soil vegetation components in a terrain component\n");
}

$sql =  "SELECT max(nhoriz) AS max_horiz FROM (select count(*) AS nhoriz FROM horizons GROUP BY soil_id) AS tt;";
$res = sql_query($sql);
if(!$res)
die("Could not read one of the relevant tables ($sql_err_msg).\nSQL-query: $sql");
else
{
	$row = sql_fetch_array($res);
	fwrite($fid,$row["max_horiz"]."\t//maximum no. of horizons in a soil\n");
}

fwrite($fid,"2"."\t//maximum no. transpositions between sub-basins\n");	//only dummy set, not read from db yet

fclose($fid);
echo("finished writing $dest.\n\n");



//make part_class.dat-----------------------------------------------------
$source="./templ/part_class.dat";
$dest=$dest_dir."part_class.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT upper_limit FROM particle_classes where class_id>0 ORDER BY upper_limit;";

$res = sql_query($sql);

if(!$res)
print("Could not read table particle_classes ($sql_err_msg). Creation of part_class.dat unsuccessful.\n");
else
{
	echo (" selection completed, ".sql_num_rows($res)." particle-classes selected.\n");
	if ($print_process)
	echo (" treating no. ...");
	for($i=1; $row = sql_fetch_array($res);$i++)		//do for all size classes found
	{
		if ($print_process)
		echo (" $i");
		//write class info
		fwrite($fid, $i."\t".$row["upper_limit"]."\n");
	}
}
fclose($fid);
echo("\nfinished writing $dest.\n\n");


//make soil_particles.dat-----------------------------------------------------
$source="./templ/soil_particles.dat";
$dest=$dest_dir."hillslope/soil_particles.dat";
echo("\ncreating $dest...\n");
if(!copy ($source,$dest ))
die("template file $source not found.");

$fid=fopen($dest,"a");
if(!$fid) die("Could not write to $dest");

$sql =  "SELECT * FROM r_soil_contains_particles ORDER BY soil_id, class_id;";

$res = sql_query($sql);

if(!$res)
print("Could not read table r_soil_contains_particles ($sql_err_msg). Creation of soil_particles.dat unsuccessful.\n");
else
{
	echo (" selection completed, ".sql_num_rows($res)." soil/particle-records selected.\n");
	if ($print_process)
	echo (" treating no. ...");
	$soil_id=0;	//for check of sum of fractions
	for($i=1; $row = sql_fetch_array($res);$i++)		//do for all soils found
	{
		if ($row["soil_id"]!=$soil_id)	//starting treatment of next soil
		{
			if ($soil_id && (abs($frac_check-1) > 0.01))	//check if all fractions sum up to one, don't do check for the very first loop
			{
				echo("\nWARNING: Sum of fractions of particle classes of soil ".$soil_id." = $frac_check, should be 1.\n  ");
				$warnings++;
			}
			$frac_check=0.;
			$soil_id=$row["soil_id"];	//restart summing up for the current soil
		}
		if ($print_process)
		echo (" $soil_id/".$row["class_id"]);
		//write soil info
		fwrite($fid, $row["soil_id"]."\t".$row["class_id"]."\t".$row["fraction"]."\n");
		$frac_check+=$row["fraction"];		//sum up fractions
	}
	if (abs($frac_check-1) > 0.01)	//check if all fractions sum up to one (last soil)
	{
		echo("\nWARNING: Sum of fractions of particle classes of soil ".$soil_id." = $frac_check, should be 1.\n");
		$warnings++;
	}

}
fclose($fid);
echo("\nfinished writing $dest.\n\n");



//make rainy_seasons.dat-----------------------------------------------------
if ($db_ver_cur > 16)
{
	$source="./templ/rainy_season.dat";
	$dest=$dest_dir."hillslope/rainy_season.dat";

	$sql =  "SELECT * FROM rainy_season order by id";
	$res = sql_query($sql);

	if(!$res)
		print("Could not read table rainy_season ($sql_err_msg). Creation of rainy_seasons.dat omitted.\n");
		else
	if(sql_num_rows($res) == 0)
		print("No rows in table rainy_season. Creation of rainy_seasons.dat omitted.\n");
	else
	{

		echo("\ncreating $dest...\n");
		if(!copy ($source,$dest ))
		die("template file $source not found.");

		$fid=fopen($dest,"a");
		if(!$fid) die("Could not write to $dest");
		
		echo (" selection completed, ".sql_num_rows($res)." rows found.\n");
		if ($print_process)
			echo (" treating no. ...");
		
		for($i=1; $row = sql_fetch_array($res);$i++)		//do for all rows
		{
			if ($print_process)
				echo (" $i");
			//write class info
			fwrite($fid, $row["subbas_id"]."\t".$row["veg_id"]."\t".$row["yearm"]."\t".$row["node1"]."\t".$row["node2"]."\t".$row["node3"]."\t".$row["node4"]."\n");
		}
		fclose($fid);
		echo("\nfinished writing $dest.\n\n");
	}

}


//make *seasons.dat-----------------------------------------------------
$parameters=array("K","C","P","coarse","n");
if ($db_ver_cur > 16)
for ($i=0; $i < count($parameters); $i++)
{
	$parm=strtolower($parameters[$i]);
	$source="./templ/".$parm."_seasons.dat";
	$dest=$dest_dir."hillslope/".$parm."_seasons.dat";

	if ($print_process)
			echo (" treating parameter ".$parameters[$i]);
			
	$sql =  "SELECT * FROM x_seasons where parameter='".$parameters[$i]."' order by id";
	$res = sql_query($sql);
	
	if(!$res)
	{
		print("Could not read table x_seasons ($sql_err_msg).\n Creation of x_seasons.dat omitted.\n");
		break;
	}	else
	if(sql_num_rows($res) == 0)
		print("No rows in table x_seasons for parameter ".$parameters[$i].".\n Creation of ".$parm."_season.dat omitted.\n");
	else
	{
		echo("\ncreating $dest...\n");
		if(!copy ($source,$dest ))
		die("\ntemplate file $source not found.");

		$fid=fopen($dest,"a");
		if(!$fid) die("Could not write to $dest");
		
		echo (" selection completed, ".sql_num_rows($res)." rows found.\n");
		if ($print_process)
			echo (" treating no. ...");
		
		for($i=1; $row = sql_fetch_array($res);$i++)		//do for all rows
		{
			if ($print_process)
				echo (" $i");
			//write class info
			fwrite($fid, $row["subbas_id"]."\t".$row["svc_id"]."\t".$row["yearm"]."\t".$row["node1"]."\t".$row["node2"]."\t".$row["node3"]."\t".$row["node4"]."\n");
		}
		fclose($fid);
		echo("\nfinished writing $dest.\n\n");
	}


}



echo("\nfinished script, $warnings warnings issued.\n");
?>
