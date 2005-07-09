unit mysql4_version;
interface

{
  Automatically converted by H2Pas 0.99.15 from mysql_version.ph
  The following command line parameters were used:
    -p
    -D
    -l
    mysqlclient
    mysql_version.ph
}

  const
    External_library='mysqlclient'; {Setup as you need}

  { Pointers to basic pascal types, inserted by h2pas conversion program.}
  Type
    PLongint  = ^Longint;
    PSmallInt = ^SmallInt;
    PByte     = ^Byte;
    PWord     = ^Word;
    PDWord    = ^DWord;
    PDouble   = ^Double;

{$PACKRECORDS C}

  { Copyright Abandoned 1996, 1999, 2001 MySQL AB
     This file is public domain and comes with NO WARRANTY of any kind  }
  { Version numbers for protocol & mysqld  }

  const
     PROTOCOL_VERSION = 10;
     MYSQL_SERVER_VERSION = '4.1.10a';
     MYSQL_SERVER_SUFFIX = '-max';
     FRM_VER = 6;
     MYSQL_VERSION_ID = 40001;
     MYSQL_PORT = 3306;
     MYSQL_UNIX_ADDR = '/tmp/mysql.sock';
  { mysqld compile time options  }
     MYSQL_CHARSET = 'latin1';

implementation


end.
