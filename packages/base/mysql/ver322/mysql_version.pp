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
  Revision 1.2  2002-09-07 15:42:53  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/26 17:51:13  michael
  + Initial move

  Revision 1.1  2002/01/29 17:54:53  peter
    * splitted to base and extra

}
