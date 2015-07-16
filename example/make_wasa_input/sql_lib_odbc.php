<?php

//enthält Umschreibungen für SQL-bezogene Funktionen für MySQL

$sql_err_msg="";		//globale Fehlervariable

function sql_connect($host,$username,$passwort)
{
$id = odbc_connect($host,$username,$passwort);
global $sql_err_msg;
$sql_err_msg = odbc_error($id).": ".odbc_errormsg($id);
if ($id > 0)
{
	global $con;
	$con=$id;		//store connection id for global access
}
return $id;
}

function sql_select_db($db)	//db wählen
{
//not used with ODBC
}

function sql_query($querystr)	//SQL-Statement verarbeiten
{
global $sql_err_msg;
global $con;

$res = odbc_exec($con,$querystr);  
$sql_err_msg = odbc_error($con).": ".odbc_errormsg($con);
return $res;
}

function sql_data_seek($res,$row)	//zu Ergebnis row springen
{
global $sql_err_msg;
global $con;

$no = odbc_fetch_row($res,$row);	//reset row pointer to 0;
$sql_err_msg = odbc_error($con).": ".odbc_errormsg($con);
return $no;
}

function sql_num_rows($res)	//Anzahl der Ergebnisdatensätze
{
global $sql_err_msg;
global $con;

sql_data_seek($res,0);	//reset row pointer to 0

$count = 0; 
while(odbc_fetch_row($res)) 
    $count++; 

$sql_err_msg = odbc_error($con).": ".odbc_errormsg($con);
sql_data_seek($res,0);	//reset row pointer to 0
return $count;
}




function sql_fetch_array($res)	//Ergebnisarray einlesen
{
/*
* odbc_fetch_array
*
* parameters :
*  - $id_res : result id (e.g. got from an odbc_exec)
*
* returns :
*  - a row of the result in an associative array with field names as keys
*  - false if there is no more result
*/
     unset($ar);
     if (odbc_fetch_row($res))
     {
       for ($i = 1; $i <= odbc_num_fields($res); $i++)
       {
         $field_name = odbc_field_name($res, $i);
         $ar[$field_name] = odbc_result($res, $field_name);
       }
       return $ar;
     }
     else
     {
	global $sql_err_msg;
	global $con;
	$sql_err_msg = odbc_error($con).": ".odbc_errormsg($con);

       return false;
     }
}


function sql_close()
{
$res = odbc_close();
return $res;
}



function sql_insert_id()	//letzte EinfügeID ermitteln
{
echo "not yet implemented";
/*
$id = mysql_insert_id();
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $id;
*/
}



?>