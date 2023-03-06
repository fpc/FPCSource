{
  Contains the mysql calls for MySQL 4.0
}

{$IFNDEF FPC_DOTTEDUNITS}
unit mysql40dyn;
{$ENDIF FPC_DOTTEDUNITS}

{$DEFINE LinkDynamically}
{$UNDEF MYSQL41}

{$i mysql.inc}

end.
