<?php

//enthält Umschreibungen für SQL-bezogene Funktionen für MySQL

$sql_err_msg="";		//globale Fehlervariable

function sql_connect($host,$username,$passwort)
{
$id = mysql_connect($host,$username,$passwort);
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $id;
}

function sql_select_db($db)	//db wählen
{
$id = mysql_select_db($db);
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $id;
}

function sql_query($querystr)	//SQL-Statement verarbeiten
{
global $sql_err_msg;
$res = mysql_query($querystr);
$sql_err_msg = mysql_errno().": ".mysql_error();
return $res;
}

function sql_fetch_array($res)	//Ergebnisarray einlesen
{
$row = mysql_fetch_array($res);
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $row;
}

function sql_insert_id()	//letzte EinfügeID ermitteln
{
$id = mysql_insert_id();
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $id;
}

function sql_num_rows($res)	//Anzahl der Ergebnisdatensätze
{
$no = mysql_num_rows($res);
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $no;
}

function sql_data_seek($res,$row)	//zu Ergebnis row springen
{
$no = mysql_data_seek($res,$row);
global $sql_err_msg;
$sql_err_msg = mysql_errno().": ".mysql_error();
return $no;
}


function sql_close()
{
$res = mysql_close();
return $res;
}

?>