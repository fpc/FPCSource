unit mysql_version;

{
  Translated from mysql_version.h by Michael Van Canneyt
  (michael@tfdec1.fys.kuleuven.ac.be)
}

interface

{ Version numbers for protocol & mysqld }
Const

 MYSQL_SERVER_VERSION : pchar ='3.21.28-gamma';
 FRM_VER = 6;
 MYSQL_VERSION_ID =32128;

implementation

end.
  $Log$
  Revision 1.1  2002-01-29 17:54:53  peter
    * splitted to base and extra

  Revision 1.2  2000/07/13 11:33:26  michael
  + removed logs
 
}
