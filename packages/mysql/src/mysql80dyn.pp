{
  Contains the MySQL calls for MySQL 8.0
}

{$IFNDEF FPC_DOTTEDUNITS}
unit mysql80dyn;
{$ENDIF FPC_DOTTEDUNITS}

{$DEFINE LinkDynamically}
{$DEFINE MYSQL80}

{$i mysql.inc}

end.
