<?php
//checks if the used database has the appropiate version number

$sql =  "select version from db_version order by date desc"; 
if(!($res = sql_query($sql)))
$row["version"]="unknown";
else
$row = sql_fetch_array($res);
$db_ver_cur = $row["version"];
if ($db_ver_cur != $db_ver_exp)
{
	echo("WARNING: The selected database has the format version number ".$row["version"].". This script expects format version $db_ver_exp.\nPlease consult db_version.txt if this is an issue and for possible update steps.\n");
	echo(" Trying to run anyway, please check output.\n\n");
	$warnings++;
}
?>