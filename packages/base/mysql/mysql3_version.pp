unit mysql3_version;

{$undef use_mysql_321} { if undefined, use mysql 3.23 interface }


{
  Translated from mysql_version.h by Michael Van Canneyt
  (michael@tfdec1.fys.kuleuven.ac.be)

  updated to match version 3.23 header files of mysql by Bernhard Steffen
  (bernhard.steffen@gmx.net)
}

interface


{ Version numbers for protocol & mysqld }
Const

{$ifdef use_mysql_321}
  MYSQL_SERVER_VERSION : pchar ='3.21.28-gamma';
  FRM_VER = 6;
  MYSQL_VERSION_ID =32128;
{$else}
  MYSQL_SERVER_VERSION : pchar ='3.23.34';
  FRM_VER = 6; { ??? }
  MYSQL_VERSION_ID =32334;
{$endif}

implementation

end.

{
  $Log: mysql3_version.pp,v $
  Revision 1.3  2005/03/25 12:03:53  michael
  + MySQL 3 dynamic connection by Bram Kuijvenhoven

  Revision 1.2  2005/02/14 17:13:19  peter
    * truncate log

}
